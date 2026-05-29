use gtk::prelude::*;
use gtk4_layer_shell::{Edge, KeyboardMode, Layer, LayerShell};
use std::fs::{read_to_string, remove_file, OpenOptions};
use std::io::Write;
use std::path::PathBuf;
use std::process::Command;
use std::sync::atomic::{AtomicBool, Ordering};

static WINDOW_OPEN: AtomicBool = AtomicBool::new(false);

struct InstanceLock {
    path: PathBuf,
}
impl Drop for InstanceLock {
    fn drop(&mut self) {
        let _ = remove_file(&self.path);
    }
}

fn lock_path() -> PathBuf {
    PathBuf::from(std::env::var("XDG_RUNTIME_DIR").unwrap_or("/tmp".to_string()))
        .join("power-menu.lock")
}

fn acquire_instance_lock() -> Option<InstanceLock> {
    let path = lock_path();
    match OpenOptions::new().write(true).create_new(true).open(&path) {
        Ok(mut f) => {
            let _ = writeln!(f, "{}", std::process::id());
            Some(InstanceLock { path })
        }
        Err(e) if e.kind() == std::io::ErrorKind::AlreadyExists => {
            if let Some(p) = read_to_string(&path)
                .ok()
                .and_then(|s| s.trim().parse::<u32>().ok())
            {
                if !std::path::Path::new(&format!("/proc/{p}")).exists() {
                    if remove_file(&path).is_ok() {
                        return acquire_instance_lock();
                    }
                }
            }
            eprintln!("Power menu is already open");
            None
        }
        Err(e) => {
            eprintln!("Failed to acquire power-menu lock: {e}");
            None
        }
    }
}

fn detect_focused_monitor_size() -> Option<(i32, i32)> {
    let out = Command::new("hyprctl").args(["monitors"]).output().ok()?;
    let s = String::from_utf8(out.stdout).ok()?;
    let mut focused = false;
    for line in s.lines() {
        let t = line.trim();
        if t.starts_with("Monitor ") {
            focused = false;
        } else if t.starts_with("focused:") {
            focused = t.contains("yes");
        } else if focused && t.contains('x') && t.contains(" at ") {
            let res = t.split(" at ").next()?.trim();
            let mut p = res.split('x');
            return Some((p.next()?.parse().ok()?, p.next()?.parse().ok()?));
        } else if focused && t.starts_with("current mode:") {
            let m = t.trim_start_matches("current mode:").trim();
            let mut p = m.split('x');
            return Some((p.next()?.parse().ok()?, p.next()?.parse().ok()?));
        }
    }
    None
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let _lock = match acquire_instance_lock() {
        Some(l) => l,
        None => {
            eprintln!("Power menu is already open");
            std::process::exit(0);
        }
    };
    if WINDOW_OPEN
        .compare_exchange(false, true, Ordering::SeqCst, Ordering::SeqCst)
        .is_err()
    {
        eprintln!("Power menu is already open");
        std::process::exit(0);
    }
    let app = gtk::Application::builder()
        .application_id("com.gbastkowski.power-menu")
        .build();
    app.connect_activate(|app| build_ui(app));
    app.run_with_args(&args.iter().map(String::as_str).collect::<Vec<_>>());
}

fn build_ui(app: &gtk::Application) {
    let w = gtk::ApplicationWindow::new(app);
    w.set_title(Some("Power Menu"));
    let (mw, mh) = detect_focused_monitor_size().unwrap_or((1920, 1080));
    w.set_default_size(mw, mh);
    w.set_resizable(false);
    w.add_css_class("power-menu-window");
    w.init_layer_shell();
    w.set_layer(Layer::Overlay);
    w.set_keyboard_mode(KeyboardMode::Exclusive);
    [Edge::Top, Edge::Bottom, Edge::Left, Edge::Right]
        .iter()
        .for_each(|&e| {
            w.set_anchor(e, true);
            w.set_margin(e, 0);
        });
    w.connect_close_request(|_| {
        WINDOW_OPEN.store(false, Ordering::SeqCst);
        gtk::glib::Propagation::Proceed
    });

    let overlay = gtk::Overlay::new();
    let blocker = gtk::Box::new(gtk::Orientation::Vertical, 0);
    blocker.set_hexpand(true);
    blocker.set_vexpand(true);
    let panel = gtk::Box::new(gtk::Orientation::Vertical, 15);
    panel.add_css_class("power-menu-panel");
    panel.set_margin_top(8);
    panel.set_margin_bottom(8);
    panel.set_margin_start(8);
    panel.set_margin_end(8);
    panel.set_halign(gtk::Align::Start);
    panel.set_valign(gtk::Align::Start);
    panel.set_width_request(320);

    let bwrap = gtk::Box::new(gtk::Orientation::Vertical, 10);
    bwrap.set_width_request(320);
    bwrap.set_halign(gtk::Align::Center);
    let status = gtk::Label::new(None);
    status.add_css_class("status-label");
    status.set_visible(false);

    let mkbtn = |label: &str, cls: &str, action: Box<dyn Fn() -> Result<(), String> + 'static>| {
        let b = gtk::Button::with_label(label);
        b.add_css_class(cls);
        b.set_hexpand(true);
        let s = status.clone();
        b.connect_clicked(move |_| match action() {
            Ok(()) => s.set_visible(false),
            Err(e) => {
                s.set_text(&format!("Error: {e}"));
                s.set_visible(true);
            }
        });
        bwrap.append(&b);
    };

    mkbtn(
        "⏾  Suspend",
        "power-button",
        Box::new(|| run_cmd(&[("systemctl", &["suspend", "-i"]), ("pm-suspend", &[])])),
    );
    mkbtn(
        "⏻  Shutdown",
        "power-button",
        Box::new(|| {
            run_cmd(&[
                ("systemctl", &["poweroff"]),
                ("shutdown", &["-h", "now"]),
                ("poweroff", &[]),
            ])
        }),
    );
    mkbtn(
        "↻  Reboot",
        "power-button",
        Box::new(|| {
            run_cmd(&[
                ("systemctl", &["reboot"]),
                ("shutdown", &["-r", "now"]),
                ("reboot", &[]),
            ])
        }),
    );
    mkbtn(
        "⎋  Logout",
        "power-button",
        Box::new(|| run_cmd(&[("hyprctl", &["dispatch", "exit"])])),
    );

    let cancel = gtk::Button::with_label("Cancel");
    cancel.add_css_class("cancel-button");
    cancel.set_hexpand(true);
    let wc = w.clone();
    cancel.connect_clicked(move |_| wc.close());
    bwrap.append(&cancel);

    panel.append(&bwrap);
    panel.append(&status);
    overlay.set_child(Some(&blocker));
    overlay.add_overlay(&panel);
    w.set_child(Some(&overlay));
    apply_css();
    w.present();
}

fn run_cmd(cmds: &[(&str, &[&str])]) -> Result<(), String> {
    let mut last = String::from("No commands available");
    for (bin, args) in cmds {
        let mut cmd = Command::new(bin);
        cmd.args(*args);
        match cmd.status() {
            Ok(s) if s.success() => return Ok(()),
            Ok(s) => last = format!("{} exited with code {}", bin, s.code().unwrap_or(-1)),
            Err(e) => last = format!("{}: {e}", bin),
        }
    }
    Err(last)
}

fn apply_css() {
    let css = gtk::CssProvider::new();
    for path in [
        format!(
            "{}/.config/power-menu/style.css",
            std::env::var("HOME").unwrap_or_default()
        ),
        format!(
            "{}/.local/share/power-menu/style.css",
            std::env::var("HOME").unwrap_or_default()
        ),
        "/usr/share/power-menu/style.css".to_string(),
    ]
    .iter()
    {
        if std::path::Path::new(path).exists() {
            css.load_from_path(path);
            break;
        }
    }
    if let Some(d) = gtk::gdk::Display::default() {
        gtk::style_context_add_provider_for_display(
            &d,
            &css,
            gtk::STYLE_PROVIDER_PRIORITY_APPLICATION,
        );
    }
}
