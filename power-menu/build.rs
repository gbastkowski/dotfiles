use std::env;
use std::path::Path;
use std::process::Command;

fn main() {
    println!("cargo:rerun-if-changed=src/main.rs");

    if let Err(e) = Command::new("pkg-config")
        .args(&["--exists", "gtk-layer-shell-0"])
        .status()
    {
        eprintln!("gtk-layer-shell not found: {}", e);
        eprintln!("Please install: sudo apt install libgtk-layer-shell-dev");
        panic!("Missing required dependency: gtk-layer-shell");
    }

    let output = Command::new("pkg-config")
        .args(&["--modversion", "gtk+-3.0"])
        .output()
        .expect("Failed to get GTK version");

    let version = String::from_utf8_lossy(&output.stdout);
    println!("Building with GTK version: {}", version.trim());

    let commit_hash = Command::new("git")
        .args(&["rev-parse", "--short", "HEAD"])
        .output()
        .ok()
        .and_then(|o| String::from_utf8(o.stdout).ok())
        .unwrap_or_else(|| "unknown".to_string());

    println!("cargo:rustc-env=COMMIT_HASH={}", commit_hash);

    let out_dir = env::var("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join("version_info.txt");
    std::fs::write(&dest_path, format!("Commit: {}", commit_hash)).unwrap();
}
