use crate::{ExternalService, ServiceError};
use async_trait::async_trait;
use exomonad_shared::protocol::{ServiceRequest, ServiceResponse};
use reqwest::{Client, Url};
use serde::Serialize;
use std::collections::HashMap;
use tracing::warn;

/// Service client for OpenTelemetry (OTLP) export.
///
/// Exports traces and metrics to an OTLP/HTTP/JSON endpoint.
/// This implementation uses a lightweight manual JSON serialization
/// to avoid the heavy dependencies of the full OpenTelemetry SDK,
/// ensuring compatibility with collectors like OpenObserve.
pub struct OtelService {
    client: Client,
    endpoint: Url,
    headers: HashMap<String, String>,
}

impl OtelService {
    /// Create a new Otel service with the given endpoint and auth headers.
    ///
    /// The endpoint should be the base URL (e.g., `https://api.openobserve.ai`).
    /// The client will append `/v1/traces` or `/v1/metrics`.
    pub fn new(endpoint: Url, headers: HashMap<String, String>) -> Self {
        Self {
            client: Client::new(),
            endpoint,
            headers,
        }
    }

    /// Create a new Otel service from environment variables.
    ///
    /// Required: `OTLP_ENDPOINT`.
    /// Optional: `OTLP_HEADERS` (format: `key=value,key2=value2`).
    pub fn from_env() -> Result<Self, anyhow::Error> {
        let endpoint_str = std::env::var("OTLP_ENDPOINT")?;
        let endpoint = Url::parse(&endpoint_str)?;

        let mut headers = HashMap::new();
        if let Ok(h_str) = std::env::var("OTLP_HEADERS") {
            for pair in h_str.split(',') {
                let trimmed = pair.trim();
                if trimmed.is_empty() {
                    continue;
                }
                if let Some((k, v)) = trimmed.split_once('=') {
                    headers.insert(k.trim().to_string(), v.trim().to_string());
                } else {
                    warn!(
                        "OTelService: ignoring malformed OTLP_HEADERS entry without '=': {:?}",
                        trimmed
                    );
                }
            }
        }

        Ok(Self::new(endpoint, headers))
    }
}

// Minimal OTLP JSON structures for Traces
#[derive(Serialize)]
struct TracesData {
    resource_spans: Vec<ResourceSpan>,
}

#[derive(Serialize)]
struct ResourceSpan {
    resource: Resource,
    scope_spans: Vec<ScopeSpan>,
}

#[derive(Serialize)]
struct Resource {
    attributes: Vec<KeyValue>,
}

#[derive(Serialize)]
struct ScopeSpan {
    scope: InstrumentationScope,
    spans: Vec<Span>,
}

#[derive(Serialize)]
struct InstrumentationScope {
    name: String,
    version: String,
}

#[derive(Serialize)]
struct Span {
    trace_id: String,
    span_id: String,
    name: String,
    start_time_unix_nano: u64,
    end_time_unix_nano: u64,
    attributes: Vec<KeyValue>,
    kind: i32, // 1=INTERNAL, 2=SERVER, 3=CLIENT, 4=PRODUCER, 5=CONSUMER
}

#[derive(Serialize)]
struct KeyValue {
    key: String,
    value: AnyValue,
}

#[derive(Serialize)]
struct AnyValue {
    string_value: Option<String>,
    // Add others if needed (int, double, bool, etc.)
}

// Minimal OTLP JSON structures for Metrics
#[derive(Serialize)]
struct MetricsData {
    resource_metrics: Vec<ResourceMetric>,
}

#[derive(Serialize)]
struct ResourceMetric {
    resource: Resource,
    scope_metrics: Vec<ScopeMetric>,
}

#[derive(Serialize)]
struct ScopeMetric {
    scope: InstrumentationScope,
    metrics: Vec<Metric>,
}

#[derive(Serialize)]
struct Metric {
    name: String,
    gauge: Option<Gauge>,
}

#[derive(Serialize)]
struct Gauge {
    data_points: Vec<NumberDataPoint>,
}

#[derive(Serialize)]
struct NumberDataPoint {
    time_unix_nano: u64,
    as_double: f64,
    attributes: Vec<KeyValue>,
}

impl From<HashMap<String, String>> for AnyValue {
    fn from(_: HashMap<String, String>) -> Self {
        // This is a simplification. Usually attributes map to KeyValue list.
        // But here we are converting a single value.
        // Wait, KeyValue has one value.
        AnyValue { string_value: None }
    }
}

fn map_attributes(attrs: &HashMap<String, String>) -> Vec<KeyValue> {
    attrs
        .iter()
        .map(|(k, v)| KeyValue {
            key: k.clone(),
            value: AnyValue {
                string_value: Some(v.clone()),
            },
        })
        .collect()
}

#[async_trait]
impl ExternalService for OtelService {
    type Request = ServiceRequest;
    type Response = ServiceResponse;

    async fn call(&self, req: Self::Request) -> Result<Self::Response, ServiceError> {
        match req {
            ServiceRequest::OtelSpan {
                trace_id,
                span_id,
                name,
                start_ns,
                end_ns,
                attributes,
            } => {
                let now = std::time::SystemTime::now()
                    .duration_since(std::time::UNIX_EPOCH)
                    .map_err(|e| {
                        warn!("SystemTime error (clock skew?): {}", e);
                        ServiceError::Api {
                            code: 500,
                            message: format!("SystemTime error: {}", e),
                        }
                    })?
                    .as_nanos() as u64;

                let payload = TracesData {
                    resource_spans: vec![ResourceSpan {
                        resource: Resource { attributes: vec![] },
                        scope_spans: vec![ScopeSpan {
                            scope: InstrumentationScope {
                                name: "exomonad-services".into(),
                                version: "0.1.0".into(),
                            },
                            spans: vec![Span {
                                trace_id,
                                span_id,
                                name,
                                start_time_unix_nano: start_ns.unwrap_or(now),
                                end_time_unix_nano: end_ns.unwrap_or(now),
                                attributes: map_attributes(&attributes.unwrap_or_default()),
                                kind: 1, // Internal
                            }],
                        }],
                    }],
                };

                let url = self.endpoint.join("/v1/traces").map_err(|e| ServiceError::Api {
                    code: 500,
                    message: format!("URL error: {}", e),
                })?;
                let mut builder = self.client.post(url).json(&payload);
                for (k, v) in &self.headers {
                    builder = builder.header(k, v);
                }

                let response = builder.send().await?;
                if !response.status().is_success() {
                    return Err(ServiceError::Api {
                        code: response.status().as_u16() as i32,
                        message: response.text().await.unwrap_or_default(),
                    });
                }

                Ok(ServiceResponse::OtelAck)
            }
            ServiceRequest::OtelMetric {
                name,
                value,
                labels,
            } => {
                let now = std::time::SystemTime::now()
                    .duration_since(std::time::UNIX_EPOCH)
                    .map_err(|e| {
                        warn!("SystemTime error (clock skew?): {}", e);
                        ServiceError::Api {
                            code: 500,
                            message: format!("SystemTime error: {}", e),
                        }
                    })?
                    .as_nanos() as u64;

                let payload = MetricsData {
                    resource_metrics: vec![ResourceMetric {
                        resource: Resource { attributes: vec![] },
                        scope_metrics: vec![ScopeMetric {
                            scope: InstrumentationScope {
                                name: "exomonad-services".into(),
                                version: "0.1.0".into(),
                            },
                            metrics: vec![Metric {
                                name,
                                gauge: Some(Gauge {
                                    data_points: vec![NumberDataPoint {
                                        time_unix_nano: now,
                                        as_double: value,
                                        attributes: map_attributes(&labels),
                                    }],
                                }),
                            }],
                        }],
                    }],
                };

                let url = self.endpoint.join("/v1/metrics").map_err(|e| ServiceError::Api {
                    code: 500,
                    message: format!("URL error: {}", e),
                })?;
                let mut builder = self.client.post(url).json(&payload);
                for (k, v) in &self.headers {
                    builder = builder.header(k, v);
                }

                let response = builder.send().await?;
                if !response.status().is_success() {
                    return Err(ServiceError::Api {
                        code: response.status().as_u16() as i32,
                        message: response.text().await.unwrap_or_default(),
                    });
                }

                Ok(ServiceResponse::OtelAck)
            }
            _ => panic!("Invalid request type for OtelService"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use wiremock::matchers::{method, path};
    use wiremock::{Mock, MockServer, ResponseTemplate};

    #[tokio::test]
    async fn test_otel_span() {
        let mock_server = MockServer::start().await;

        Mock::given(method("POST"))
            .and(path("/v1/traces"))
            .respond_with(ResponseTemplate::new(200))
            .mount(&mock_server)
            .await;

        let service = OtelService::new(mock_server.uri().parse().unwrap(), HashMap::new());

        let req = ServiceRequest::OtelSpan {
            trace_id: "12345678901234567890123456789012".into(),
            span_id: "1234567890123456".into(),
            name: "test-span".into(),
            start_ns: Some(1000),
            end_ns: Some(2000),
            attributes: Some(HashMap::new()),
        };

        match service.call(req).await.unwrap() {
            ServiceResponse::OtelAck => {}
            _ => panic!("Wrong response type"),
        }
    }
}
