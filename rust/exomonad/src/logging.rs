use std::path::PathBuf;
use tracing::Subscriber;
use tracing_subscriber::prelude::*;
use tracing_subscriber::Registry;

pub struct LoggingGuard {
    _file_guard: Option<tracing_appender::non_blocking::WorkerGuard>,
    tracer_provider: Option<opentelemetry_sdk::trace::TracerProvider>,
}

impl Drop for LoggingGuard {
    fn drop(&mut self) {
        if let Some(provider) = self.tracer_provider.take() {
            for result in provider.force_flush() {
                if let Err(e) = result {
                    eprintln!("[exomonad] OTel flush error: {e}");
                }
            }
        }
    }
}

pub fn init(otlp_endpoint: Option<&str>, service_name: &str) -> LoggingGuard {
    let use_json = std::env::var("EXOMONAD_LOG_FORMAT")
        .map(|v| v.eq_ignore_ascii_case("json"))
        .unwrap_or(false);

    let log_dir = PathBuf::from(".exo/logs");
    let file_ok = std::fs::create_dir_all(&log_dir).is_ok();

    let env_filter = tracing_subscriber::EnvFilter::from_default_env()
        .add_directive(tracing::Level::INFO.into());

    // Build file layer
    let mut file_guard = None;
    let (file_plain, file_json) = if file_ok {
        let appender = tracing_appender::rolling::daily(&log_dir, "sidecar.log");
        let (nb, g) = tracing_appender::non_blocking(appender);
        file_guard = Some(g);
        if use_json {
            (None, Some(tracing_subscriber::fmt::layer().json().with_writer(nb).with_ansi(false)))
        } else {
            (Some(tracing_subscriber::fmt::layer().with_writer(nb).with_ansi(false)), None)
        }
    } else {
        (None, None)
    };

    // Build stderr layer
    let (stderr_plain, stderr_json) = if use_json {
        (None, Some(tracing_subscriber::fmt::layer().json().with_writer(std::io::stderr)))
    } else {
        (Some(tracing_subscriber::fmt::layer().with_writer(std::io::stderr)), None)
    };

    // Build OTel layer
    let (otel_layer, tracer_provider) = match otlp_endpoint {
        Some(endpoint) => match build_otel_provider(endpoint, service_name) {
            Ok(provider) => {
                eprintln!("[exomonad] OTel tracing enabled → {endpoint}");
                use opentelemetry::trace::TracerProvider as _;
                let tracer = provider.tracer("exomonad");
                let layer = tracing_opentelemetry::layer().with_tracer(tracer);
                (Some(layer), Some(provider))
            }
            Err(e) => {
                eprintln!("[exomonad] OTel init failed (continuing without): {e}");
                (None, None)
            }
        },
        None => (None, None),
    };

    // Assemble and install the subscriber
    let subscriber = Registry::default()
        .with(env_filter)
        .with(file_plain)
        .with(file_json)
        .with(stderr_plain)
        .with(stderr_json)
        .with(otel_layer);
    set_global(subscriber);

    LoggingGuard {
        _file_guard: file_guard,
        tracer_provider,
    }
}

fn set_global(subscriber: impl Subscriber + Send + Sync + 'static) {
    tracing::subscriber::set_global_default(subscriber)
        .expect("Failed to set global tracing subscriber");
}

fn build_otel_provider(
    endpoint: &str,
    service_name: &str,
) -> Result<opentelemetry_sdk::trace::TracerProvider, Box<dyn std::error::Error>> {
    use opentelemetry::KeyValue;
    use opentelemetry_otlp::WithExportConfig;
    use opentelemetry_sdk::trace::TracerProvider;
    use opentelemetry_sdk::Resource;

    let exporter = opentelemetry_otlp::SpanExporter::builder()
        .with_tonic()
        .with_endpoint(endpoint)
        .build()?;

    let resource = Resource::new(vec![
        KeyValue::new("service.name", service_name.to_string()),
        KeyValue::new("service.version", env!("CARGO_PKG_VERSION").to_string()),
    ]);

    let provider = TracerProvider::builder()
        .with_batch_exporter(exporter, opentelemetry_sdk::runtime::Tokio)
        .with_resource(resource)
        .build();

    opentelemetry::global::set_text_map_propagator(
        opentelemetry_sdk::propagation::TraceContextPropagator::new(),
    );

    Ok(provider)
}
