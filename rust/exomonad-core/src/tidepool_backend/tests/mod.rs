use super::*;
use tidepool_effect::dispatch::EffectHandler;
use tidepool_effect::error::EffectError as TidepoolEffectError;
use tidepool_eval::value::Value;
use tidepool_bridge::{FromCore, ToCore};
use crate::effects::EffectContext;
use tidepool_effect::dispatch::EffectContext as TidepoolEffectContext;

pub(crate) struct MockIdentity;
impl EffectHandler<EffectContext> for MockIdentity {
    type Request = IdentityReq;
    fn handle(
        &mut self,
        req: IdentityReq,
        cx: &TidepoolEffectContext<'_, EffectContext>,
    ) -> Result<Value, TidepoolEffectError> {
        match req {
            IdentityReq::GetAgentId => cx.respond("test".to_string()),
            IdentityReq::GetParentTab => cx.respond("parent-tab".to_string()),
            IdentityReq::GetOwnTab => cx.respond("test-tab".to_string()),
            IdentityReq::GetWorkingDir => cx.respond(".".to_string()),
        }
    }
}

pub(crate) struct MockInbox;
impl EffectHandler<EffectContext> for MockInbox {
    type Request = InboxReq;
    fn handle(
        &mut self,
        req: InboxReq,
        cx: &TidepoolEffectContext<'_, EffectContext>,
    ) -> Result<Value, TidepoolEffectError> {
        match req {
            InboxReq::WriteMessage(_, _, _, _) => cx.respond("ok".to_string()),
            InboxReq::ReadMessages(_) => cx.respond("[]".to_string()),
            InboxReq::PollMessages(_, _) => cx.respond("[]".to_string()),
        }
    }
}

pub(crate) struct MockQuestions;
impl EffectHandler<EffectContext> for MockQuestions {
    type Request = QuestionsReq;
    fn handle(
        &mut self,
        req: QuestionsReq,
        cx: &TidepoolEffectContext<'_, EffectContext>,
    ) -> Result<Value, TidepoolEffectError> {
        match req {
            QuestionsReq::ResolveQuestion(_, _) => cx.respond("true".to_string()),
        }
    }
}

mod schema_tests;
mod bridge_tests;
mod pipeline_tests;
