use extism::{CurrentPlugin, Error, UserData, Val};
use tracing::info;

pub fn git_get_branch(
    _plugin: &mut CurrentPlugin,
    _inputs: &[Val],
    outputs: &mut [Val],
    _user_data: UserData<()>,
) -> Result<(), Error> {
    info!("Host function called: git_get_branch");
    if outputs.is_empty() {
        return Err(Error::msg(
            "git_get_branch: expected at least one output value",
        ));
    }
    outputs[0] = Val::I64(0);
    Ok(())
}

pub fn log_info(
    _plugin: &mut CurrentPlugin,
    inputs: &[Val],
    _outputs: &mut [Val],
    _user_data: UserData<()>,
) -> Result<(), Error> {
    if inputs.len() < 1 {
        return Err(Error::msg(
            "log_info: expected at least 1 input argument, got 0",
        ));
    }
    let _offset = inputs[0].i64().unwrap_or(0);
    // let len = inputs[1].i64().unwrap_or(0);

    info!("Host function called: log_info");
    Ok(())
}
