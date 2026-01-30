pub mod docker;
pub mod log;
pub mod github;
pub mod git;

use self::docker::DockerService;
use self::log::{HasLogService, LogService};

#[derive(Clone)]
pub struct Services {
    pub docker: DockerService,
    pub log: LogService,
}

impl Services {
    pub fn new() -> Self {
        Self {
            docker: DockerService::new(),
            log: LogService::default(),
        }
    }
}

impl HasLogService for Services {
    fn log_service(&self) -> &LogService {
        &self.log
    }
}