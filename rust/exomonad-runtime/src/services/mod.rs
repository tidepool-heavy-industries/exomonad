pub mod docker;

use self::docker::DockerService;

#[derive(Clone)]
pub struct Services {
    pub docker: DockerService,
}

impl Services {
    pub fn new() -> Self {
        Self {
            docker: DockerService::new(),
        }
    }
}