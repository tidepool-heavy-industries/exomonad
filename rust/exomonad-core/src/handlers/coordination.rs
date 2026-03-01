//! Coordination effect handler for the `coordination.*` namespace.
//!
//! Provides distributed mutexes for agents to coordinate access to shared resources.

use crate::effects::{dispatch_coordination_effect, CoordinationEffects, EffectResult};
use crate::services::mutex_registry::MutexRegistry;
use async_trait::async_trait;
use exomonad_proto::effects::coordination::*;
use std::sync::Arc;
use std::time::Duration;
use tracing::info;

/// Coordination effect handler.
pub struct CoordinationHandler {
    registry: Arc<MutexRegistry>,
}

impl CoordinationHandler {
    pub fn new(registry: Arc<MutexRegistry>) -> Self {
        Self { registry }
    }
}

crate::impl_pass_through_handler!(CoordinationHandler, "coordination", dispatch_coordination_effect);

#[async_trait]
impl CoordinationEffects for CoordinationHandler {
    async fn acquire_mutex(
        &self,
        req: AcquireMutexRequest,
        ctx: &crate::effects::EffectContext,
    ) -> EffectResult<AcquireMutexResponse> {
        let agent = ctx.agent_name.to_string();
        let resource = req.resource.clone();
        let intent = req.intent.clone();
        
        let estimated_time_secs = if req.estimated_time_secs == 0 {
            300
        } else {
            req.estimated_time_secs as u64
        };
        
        let timeout_secs = if req.timeout_secs == 0 {
            300
        } else {
            req.timeout_secs as u64
        };

        info!(
            resource = %resource,
            agent = %agent,
            intent = %intent,
            estimated_time_secs = estimated_time_secs,
            timeout_secs = timeout_secs,
            "Acquiring mutex via effect"
        );

        let resp = self
            .registry
            .acquire(
                resource,
                agent,
                intent,
                Duration::from_secs(estimated_time_secs),
                Duration::from_secs(timeout_secs),
            )
            .await;

        Ok(resp)
    }

    async fn release_mutex(
        &self,
        req: ReleaseMutexRequest,
        ctx: &crate::effects::EffectContext,
    ) -> EffectResult<ReleaseMutexResponse> {
        let resource = req.resource.clone();
        let lock_id = req.lock_id.clone();
        let agent = ctx.agent_name.to_string();

        info!(
            resource = %resource,
            lock_id = %lock_id,
            agent = %agent,
            "Releasing mutex via effect"
        );

        let released = self.registry.release(resource, lock_id).await;

        Ok(ReleaseMutexResponse { released })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::domain::{AgentName, BirthBranch};
    use crate::effects::{EffectContext, EffectHandler};

    fn test_ctx() -> EffectContext {
        EffectContext {
            agent_name: AgentName::from("test"),
            birth_branch: BirthBranch::from("main"),
        }
    }

    #[test]
    fn test_namespace() {
        let registry = Arc::new(MutexRegistry::new());
        let handler = CoordinationHandler::new(registry);
        assert_eq!(handler.namespace(), "coordination");
    }

    #[tokio::test]
    async fn test_acquire_and_release() {
        let registry = Arc::new(MutexRegistry::new());
        let handler = CoordinationHandler::new(registry);
        let ctx = test_ctx();

        let req = AcquireMutexRequest {
            resource: "test-resource".into(),
            intent: "test-intent".into(),
            estimated_time_secs: 60,
            timeout_secs: 10,
        };

        let resp = handler.acquire_mutex(req, &ctx).await.unwrap();
        assert!(resp.acquired);
        assert!(!resp.lock_id.is_empty());
        assert_eq!(resp.holder_intent, "test-intent");

        let release_req = ReleaseMutexRequest {
            resource: "test-resource".into(),
            lock_id: resp.lock_id.clone(),
        };

        let release_resp = handler.release_mutex(release_req, &ctx).await.unwrap();
        assert!(release_resp.released);
    }

    #[tokio::test]
    async fn test_idempotent_acquire() {
        let registry = Arc::new(MutexRegistry::new());
        let handler = CoordinationHandler::new(registry);
        let ctx = test_ctx();

        let req = AcquireMutexRequest {
            resource: "test-resource".into(),
            intent: "test-intent".into(),
            estimated_time_secs: 60,
            timeout_secs: 10,
        };

        let resp1 = handler.acquire_mutex(req.clone(), &ctx).await.unwrap();
        assert!(resp1.acquired);

        let resp2 = handler.acquire_mutex(req, &ctx).await.unwrap();
        assert!(resp2.acquired);
        assert_eq!(resp1.lock_id, resp2.lock_id);
    }

    #[tokio::test]
    async fn test_release_wrong_lock_id() {
        let registry = Arc::new(MutexRegistry::new());
        let handler = CoordinationHandler::new(registry);
        let ctx = test_ctx();

        let req = AcquireMutexRequest {
            resource: "test-resource".into(),
            intent: "test-intent".into(),
            estimated_time_secs: 60,
            timeout_secs: 10,
        };

        let resp = handler.acquire_mutex(req, &ctx).await.unwrap();
        assert!(resp.acquired);

        let release_req = ReleaseMutexRequest {
            resource: "test-resource".into(),
            lock_id: "wrong-id".into(),
        };

        let release_resp = handler.release_mutex(release_req, &ctx).await.unwrap();
        assert!(!release_resp.released);
    }
}
