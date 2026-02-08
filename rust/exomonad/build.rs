use std::path::PathBuf;

fn main() {
    let wasm_dir = PathBuf::from("../../.exomonad/wasm");
    let out_dir = PathBuf::from(std::env::var("OUT_DIR").unwrap());

    for role in &["tl", "dev"] {
        let src = wasm_dir.join(format!("wasm-guest-{}.wasm", role));
        let dst = out_dir.join(format!("wasm-guest-{}.wasm", role));

        println!("cargo::rerun-if-changed={}", src.display());

        if src.exists() {
            std::fs::copy(&src, &dst).unwrap();
        } else {
            // Write empty placeholder so include_bytes! succeeds.
            // Runtime will reject empty WASM with a helpful error.
            std::fs::write(&dst, b"").unwrap();
        }
    }
}
