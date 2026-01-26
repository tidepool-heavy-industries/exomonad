use bollard::container::InspectContainerOptions;
use bollard::Docker;
use serde::Serialize;

#[derive(Serialize)]
pub struct StatusResponse {
    pub status: String,
}

pub async fn run(container: String) -> anyhow::Result<String> {
    let docker = Docker::connect_with_local_defaults()?;
    
    match docker.inspect_container(&container, None::<InspectContainerOptions>).await {
        Ok(inspect) => {
            let status = inspect.state
                .and_then(|s| s.status)
                .map(|s| s.to_string())
                .unwrap_or_else(|| "unknown".to_string());
            
            Ok(serde_json::to_string(&StatusResponse { status })?)
        },
        Err(bollard::errors::Error::DockerResponseServerError { status_code: 404, .. }) => {
            Ok(serde_json::to_string(&StatusResponse { status: "not_found".to_string() })?)
        },
        Err(e) => Err(e.into()),
    }
}
