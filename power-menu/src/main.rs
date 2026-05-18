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
    window.set_default_size(280, 360);
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

    let title = gtk::Label::new(Some("Power Menu"));
    title.add_css_class("title");
    main_box.append(&title);

    let separator = gtk::Separator::new(gtk::Orientation::Horizontal);
    main_box.append(&separator);

    let button_grid = gtk::Grid::new();
    button_grid.set_column_spacing(15);
    button_grid.set_row_spacing(15);

    let status_label = gtk::Label::new(None);
    status_label.add_css_class("status-label");
    status_label.set_visible(false);

    create_power_button(&button_grid, "Shutdown", 0, 0, &status_label, || {
        run_command_chain(&[
            &["systemctl", "poweroff"],
            &["shutdown", "-h", "now"],
            &["poweroff"],
        ])
    });

    create_power_button(&button_grid, "Reboot", 1, 0, &status_label, || {
        run_command_chain(&[
            &["systemctl", "reboot"],
            &["shutdown", "-r", "now"],
            &["reboot"],
        ])
    });

    create_power_button(&button_grid, "Suspend", 0, 1, &status_label, || {
        run_command_chain(&[&["systemctl", "suspend"], &["pm-suspend"]])
    });

    create_power_button(&button_grid, "Logout", 1, 1, &status_label, || {
        run_command_chain(&[&["hyprctl", "dispatch", "exit"]])
    });

    main_box.append(&button_grid);

    let cancel_button = gtk::Button::with_label("Cancel");
    cancel_button.add_css_class("cancel-button");
    let window_clone = window.clone();
    cancel_button.connect_clicked(move |_| {
        window_clone.close();
    });
    main_box.append(&cancel_button);

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
    grid: &gtk::Grid,
    label: &str,
    col: i32,
    row: i32,
    status: &gtk::Label,
    action: impl Fn() -> Result<(), String> + 'static,
) {
    let button = gtk::Button::with_label(label);
    button.add_css_class("power-button");

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

    grid.attach(&button, col, row, 1, 1);
}

fn apply_css_styling() {
    let css_provider = gtk::CssProvider::new();
    css_provider.load_from_data(
        r#"
        .title {
            font-size: 18px;
            font-weight: bold;
            margin-bottom: 10px;
            color: #e5e9f0;
        }

        .power-button {
            font-size: 14px;
            padding: 12px 0;
            border-radius: 8px;
            background-color: #4c566a;
            color: #e5e9f0;
            border: 1px solid #434c5e;
            transition: all 0.2s ease;
        }

        .power-button:hover {
            background-color: #5e81ac;
            border-color: #81a1c1;
            color: #eceff4;
        }

        .cancel-button {
            font-size: 14px;
            padding: 8px 0;
            margin-top: 10px;
            color: #d8dee9;
        }

        .cancel-button:hover {
            color: #ffffff;
        }

        .status-label {
            font-size: 12px;
            color: #bf616a;
            margin-top: 8px;
        }

        window {
            background-color: #2e3440;
            border-radius: 12px;
            box-shadow: 0 4px 20px rgba(0, 0, 0, 0.3);
        }
        "#,
    );

    if let Some(display) = gtk::gdk::Display::default() {
        gtk::style_context_add_provider_for_display(
            &display,
            &css_provider,
            gtk::STYLE_PROVIDER_PRIORITY_APPLICATION,
        );
    }
}
