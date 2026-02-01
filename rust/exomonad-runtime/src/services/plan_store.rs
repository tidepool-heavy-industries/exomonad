use chrono::{DateTime, Duration, Utc};
use extism::{CurrentPlugin, Error, Function, UserData, Val, ValType};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use uuid::Uuid;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RefactorPlan {
    pub id: Uuid,
    pub rule: String, // ast-grep YAML
    pub scope: String,
    pub language: String,
    pub diff: String,
    pub files: Vec<String>,
    pub created_at: DateTime<Utc>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct NewRefactorPlan {
    pub rule: String,
    pub scope: String,
    pub language: String,
    pub diff: String,
    pub files: Vec<String>,
}

#[derive(Clone)]
pub struct PlanStore {
    plans: Arc<Mutex<HashMap<Uuid, RefactorPlan>>>,
}

impl PlanStore {
    pub fn new() -> Self {
        Self {
            plans: Arc::new(Mutex::new(HashMap::new())),
        }
    }

    pub fn store(&self, input: NewRefactorPlan) -> Uuid {
        let id = Uuid::new_v4();
        let plan = RefactorPlan {
            id,
            rule: input.rule,
            scope: input.scope,
            language: input.language,
            diff: input.diff,
            files: input.files,
            created_at: Utc::now(),
        };
        self.plans.lock().unwrap().insert(id, plan);
        id
    }

    pub fn get(&self, id: &Uuid) -> Option<RefactorPlan> {
        self.cleanup();
        self.plans.lock().unwrap().get(id).cloned()
    }

    pub fn delete(&self, id: &Uuid) -> bool {
        self.plans.lock().unwrap().remove(id).is_some()
    }

    fn cleanup(&self) {
        let mut plans = self.plans.lock().unwrap();
        let now = Utc::now();
        // 10 minute TTL
        plans.retain(|_, plan| now.signed_duration_since(plan.created_at) < Duration::minutes(10));
    }
}

// --- Host Functions ---

#[derive(Deserialize)]
struct StorePlanInput {
    plan: NewRefactorPlan,
}

#[derive(Serialize)]
struct StorePlanOutput {
    id: Uuid,
}

#[derive(Deserialize)]
struct GetPlanInput {
    id: Uuid,
}

#[derive(Serialize)]
struct GetPlanOutput {
    plan: Option<RefactorPlan>,
}

#[derive(Deserialize)]
struct DeletePlanInput {
    id: Uuid,
}

#[derive(Serialize)]
struct DeletePlanOutput {
    deleted: bool,
}

#[derive(Serialize)]
#[serde(tag = "kind", content = "payload")]
enum HostResult<T> {
    Success(T),
    Error(HostError),
}

#[derive(Serialize)]
struct HostError {
    message: String,
}

impl<T> From<Result<T, anyhow::Error>> for HostResult<T> {
    fn from(res: Result<T, anyhow::Error>) -> Self {
        match res {
            Ok(val) => HostResult::Success(val),
            Err(e) => HostResult::Error(HostError {
                message: e.to_string(),
            }),
        }
    }
}

fn get_input<T: serde::de::DeserializeOwned>(
    plugin: &mut CurrentPlugin,
    val: Val,
) -> Result<T, Error> {
    let handle = plugin
        .memory_from_val(&val)
        .ok_or_else(|| Error::msg("Invalid memory handle in input"))?;
    let bytes = plugin.memory_bytes(handle)?;
    Ok(serde_json::from_slice(bytes)?)
}

fn set_output<T: Serialize>(plugin: &mut CurrentPlugin, data: &T) -> Result<Val, Error> {
    let json = serde_json::to_vec(data)?;
    let handle = plugin.memory_new(json)?;
    Ok(plugin.memory_to_val(handle))
}

pub fn register_host_functions(service: Arc<PlanStore>) -> Vec<Function> {
    vec![
        Function::new(
            "plan_store_save",
            [ValType::I64],
            [ValType::I64],
            UserData::new(service.clone()),
            plan_store_save,
        )
        .with_namespace("env"),
        Function::new(
            "plan_store_get",
            [ValType::I64],
            [ValType::I64],
            UserData::new(service.clone()),
            plan_store_get,
        )
        .with_namespace("env"),
        Function::new(
            "plan_store_delete",
            [ValType::I64],
            [ValType::I64],
            UserData::new(service),
            plan_store_delete,
        )
        .with_namespace("env"),
    ]
}

fn plan_store_save(
    plugin: &mut CurrentPlugin,
    inputs: &[Val],
    outputs: &mut [Val],
    user_data: UserData<Arc<PlanStore>>,
) -> Result<(), Error> {
    let input: StorePlanInput = get_input(plugin, inputs[0].clone())?;
    let service_arc = user_data.get()?;
    let service = service_arc
        .lock()
        .map_err(|_| Error::msg("Poisoned lock"))?;

    let id = service.store(input.plan);
    let output: HostResult<StorePlanOutput> = Ok(StorePlanOutput { id }).into();

    outputs[0] = set_output(plugin, &output)?;
    Ok(())
}

fn plan_store_get(
    plugin: &mut CurrentPlugin,
    inputs: &[Val],
    outputs: &mut [Val],
    user_data: UserData<Arc<PlanStore>>,
) -> Result<(), Error> {
    let input: GetPlanInput = get_input(plugin, inputs[0].clone())?;
    let service_arc = user_data.get()?;
    let service = service_arc
        .lock()
        .map_err(|_| Error::msg("Poisoned lock"))?;

    let plan = service.get(&input.id);
    let output: HostResult<GetPlanOutput> = Ok(GetPlanOutput { plan }).into();

    outputs[0] = set_output(plugin, &output)?;
    Ok(())
}

fn plan_store_delete(
    plugin: &mut CurrentPlugin,
    inputs: &[Val],
    outputs: &mut [Val],
    user_data: UserData<Arc<PlanStore>>,
) -> Result<(), Error> {
    let input: DeletePlanInput = get_input(plugin, inputs[0].clone())?;
    let service_arc = user_data.get()?;
    let service = service_arc
        .lock()
        .map_err(|_| Error::msg("Poisoned lock"))?;

    let deleted = service.delete(&input.id);
    let output: HostResult<DeletePlanOutput> = Ok(DeletePlanOutput { deleted }).into();

    outputs[0] = set_output(plugin, &output)?;
    Ok(())
}
