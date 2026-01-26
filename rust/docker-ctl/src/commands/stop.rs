use bollard::container::{StopContainerOptions, RemoveContainerOptions};
use bollard::Docker;
use serde::Serialize;

#[derive(Serialize)]
pub struct StopResponse {
    pub stopped: bool,
}

pub async fn run(container: String, timeout: u64) -> anyhow::Result<String> {
    let docker = Docker::connect_with_local_defaults()?;
    
    let stop_options = StopContainerOptions {
        t: timeout as i64,
    };
    
    match docker.stop_container(&container, Some(stop_options)).await {
        Ok(_) => (),
        Err(bollard::errors::Error::DockerResponseServerError { status_code: 404, .. }) => {
            return Ok(serde_json::to_string(&StopResponse { stopped: true })?);
        },
        Err(e) => return Err(e.into()),
    }

    docker.remove_container(&container, None::<RemoveContainerOptions>).await?;
    
    Ok(serde_json::to_string(&StopResponse { stopped: true })?)
}
