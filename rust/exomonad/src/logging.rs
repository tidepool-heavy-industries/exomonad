use std::path::PathBuf;
use tracing_subscriber::prelude::*;

pub fn init() -> Option<tracing_appender::non_blocking::WorkerGuard> {
    let use_json = std::env::var("EXOMONAD_LOG_FORMAT")
        .map(|v| v.eq_ignore_ascii_case("json"))
        .unwrap_or(false);

    let log_dir = PathBuf::from(".exo/logs");
    let file_ok = std::fs::create_dir_all(&log_dir).is_ok();

    let env_filter = tracing_subscriber::EnvFilter::from_default_env()
        .add_directive(tracing::Level::INFO.into());

    let (file_layer_plain, file_layer_json, guard) = if file_ok {
        let appender = tracing_appender::rolling::daily(&log_dir, "sidecar.log");
        let (nb, g) = tracing_appender::non_blocking(appender);
        if use_json {
            let layer = tracing_subscriber::fmt::layer().json().with_writer(nb).with_ansi(false);
            (None, Some(layer), Some(g))
        } else {
            let layer = tracing_subscriber::fmt::layer().with_writer(nb).with_ansi(false);
            (Some(layer), None, Some(g))
        }
    } else {
        (None, None, None)
    };

    let (stderr_layer_plain, stderr_layer_json) = if use_json {
        let layer = tracing_subscriber::fmt::layer().json().with_writer(std::io::stderr);
        (None, Some(layer))
    } else {
        let layer = tracing_subscriber::fmt::layer().with_writer(std::io::stderr);
        (Some(layer), None)
    };

    tracing_subscriber::registry()
        .with(env_filter)
        .with(file_layer_plain)
        .with(file_layer_json)
        .with(stderr_layer_plain)
        .with(stderr_layer_json)
        .init();

    guard
}
