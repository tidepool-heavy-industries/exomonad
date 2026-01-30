use clap::Parser;
use exomonad_runtime::{PluginManager, Services, start_server};
use std::path::PathBuf;
use std::sync::Arc;
use tracing::info;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Path to the WASM plugin
    #[arg(short, long)]
    wasm: PathBuf,

    /// Port to listen on
    #[arg(short, long, default_value_t = 3000)]
    port: u16,
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    tracing_subscriber::fmt::init();

    let args = Args::parse();

    info!("Starting ExoMonad Runtime");
    info!("Loading WASM plugin from: {:?}", args.wasm);

    let services = Arc::new(Services);
    let plugin_manager = PluginManager::new(args.wasm, services.clone()).await?;

    start_server(args.port, plugin_manager, services).await;

    Ok(())
}
