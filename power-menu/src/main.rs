use gtk::prelude::*;
use gtk4_layer_shell::{Edge, Layer, LayerShell};
use std::process::Command;

fn main() {
    let app = gtk::Application::builder()
        .application_id("com.gbastkowski.power-menu")
        .build();

    app.connect_activate(build_ui);
    app.run();
}

fn build_ui(app: &gtk::Application) {
    let window = gtk::ApplicationWindow::new(app);
    window.set_title(Some("Power Menu"));
    window.set_default_size(800, 500);
    window.set_resizable(false);

    window.init_layer_shell();
    window.set_layer(Layer::Overlay);
    window.set_anchor(Edge::Top, false);
    window.set_margin(Edge::Top, 40);
    window.set_margin(Edge::Bottom, 40);

    let main_box = gtk::Box::new(gtk::Orientation::Vertical, 15);
    main_box.set_margin_top(20);
    main_box.set_margin_bottom(20);
    main_box.set_margin_start(20);
    main_box.set_margin_end(20);
    main_box.set_vexpand(true);
    main_box.set_valign(gtk::Align::Center);

    let button_box = gtk::Box::new(gtk::Orientation::Vertical, 10);
    button_box.set_width_request(240);
    button_box.set_halign(gtk::Align::Center);

    let status_label = gtk::Label::new(None);
    status_label.add_css_class("status-label");
    status_label.set_visible(false);

    create_power_button(&button_box, "⏾  Suspend", &status_label, || {
        run_command_chain(&[&["systemctl", "suspend", "-i"], &["pm-suspend"]])
    });

    create_power_button(&button_box, "⏻  Shutdown", &status_label, || {
        run_command_chain(&[
            &["systemctl", "poweroff"],
            &["shutdown", "-h", "now"],
            &["poweroff"],
        ])
    });

    create_power_button(&button_box, "↻  Reboot", &status_label, || {
        run_command_chain(&[
            &["systemctl", "reboot"],
            &["shutdown", "-r", "now"],
            &["reboot"],
        ])
    });

    create_power_button(&button_box, "⎋  Logout", &status_label, || {
        run_command_chain(&[&["hyprctl", "dispatch", "exit"]])
    });

    main_box.append(&button_box);

    let cancel_box = gtk::Box::new(gtk::Orientation::Vertical, 0);
    cancel_box.set_width_request(240);
    cancel_box.set_halign(gtk::Align::Center);

    let cancel_button = gtk::Button::with_label("Cancel");
    cancel_button.add_css_class("cancel-button");
    cancel_button.set_hexpand(true);
    let window_clone = window.clone();
    cancel_button.connect_clicked(move |_| {
        window_clone.close();
    });
    cancel_box.append(&cancel_button);
    main_box.append(&cancel_box);

    main_box.append(&status_label);

    window.set_child(Some(&main_box));

    apply_css_styling();

    window.present();
}

fn run_command_chain(commands: &[&[&str]]) -> Result<(), String> {
    let mut last_error = String::from("No commands available");

    for cmd_parts in commands {
        if cmd_parts.is_empty() {
            continue;
        }
        let mut cmd = Command::new(cmd_parts[0]);
        cmd.args(&cmd_parts[1..]);

        match cmd.status() {
            Ok(status) if status.success() => return Ok(()),
            Ok(status) => {
                last_error = format!(
                    "{} exited with code {}",
                    cmd_parts[0],
                    status.code().unwrap_or(-1)
                );
            }
            Err(e) => {
                last_error = format!("{}: {e}", cmd_parts[0]);
            }
        }
    }

    Err(last_error)
}

fn create_power_button(
    box_container: &gtk::Box,
    label: &str,
    status: &gtk::Label,
    action: impl Fn() -> Result<(), String> + 'static,
) {
    let button = gtk::Button::with_label(label);
    button.add_css_class("power-button");
    button.set_hexpand(true);

    let status = status.clone();
    button.connect_clicked(move |_| match action() {
        Ok(()) => {
            status.set_visible(false);
        }
        Err(msg) => {
            status.set_text(&format!("Error: {msg}"));
            status.set_visible(true);
        }
    });

    box_container.append(&button);
}

fn apply_css_styling() {
    let css_provider = gtk::CssProvider::new();

    let css_paths = [
        format!(
            "{}/.config/power-menu/style.css",
            std::env::var("HOME").unwrap_or_default()
        ),
        format!(
            "{}/.local/share/power-menu/style.css",
            std::env::var("HOME").unwrap_or_default()
        ),
        "/usr/share/power-menu/style.css".to_string(),
    ];

    let mut loaded = false;
    for path in &css_paths {
        if std::path::Path::new(path).exists() {
            css_provider.load_from_path(path);
            loaded = true;
            break;
        }
    }

    if !loaded {
        eprintln!(
            "Warning: Could not find style.css. Checked: {:?}",
            css_paths
        );
    }

    if let Some(display) = gtk::gdk::Display::default() {
        gtk::style_context_add_provider_for_display(
            &display,
            &css_provider,
            gtk::STYLE_PROVIDER_PRIORITY_APPLICATION,
        );
    }
}
