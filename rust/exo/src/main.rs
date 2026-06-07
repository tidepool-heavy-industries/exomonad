//! `exo` — the minimal node entrypoint. The composition root: build the domain roster, hand it
//! to the engine, run the sidecar. Everything substantive lives in the framework (`exo-node` /
//! `exo-framework`) and the domain lib (`exo`); this `main` only wires them together.
//!
//! ```text
//!   exo --papers <path>     # run the node-mode sidecar for the node described by <path>
//! ```

use std::path::PathBuf;
use std::sync::Arc;

use anyhow::Context;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let papers = parse_papers().context("parsing --papers <path>")?;
    let cwd = std::env::current_dir().context("resolving node cwd")?;

    // Inject the domain roster into the engine — the engine never names a concrete role.
    let ctx = exo_node::bootstrap(&papers, cwd, exo::roster())
        .map(Arc::new)
        .context("node self-ID / bootstrap")?;
    exo_node::run_node(ctx).await.context("node run")?;
    Ok(())
}

/// Parse the single required `--papers <path>` flag (the node's birth papers).
fn parse_papers() -> anyhow::Result<PathBuf> {
    let mut args = std::env::args().skip(1);
    let mut papers: Option<PathBuf> = None;
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--papers" => {
                papers = Some(PathBuf::from(
                    args.next().context("--papers requires a path argument")?,
                ))
            }
            other => anyhow::bail!("unexpected argument: {other}"),
        }
    }
    papers.context("missing required --papers <path>")
}
