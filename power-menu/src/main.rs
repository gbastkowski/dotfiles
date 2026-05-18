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
    window.set_default_size(280, 320);
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

    create_power_button(&button_grid, "Shutdown", 0, 0, || {
        Command::new("systemctl").arg("poweroff").status().ok();
    });

    create_power_button(&button_grid, "Reboot", 1, 0, || {
        Command::new("systemctl").arg("reboot").status().ok();
    });

    create_power_button(&button_grid, "Suspend", 0, 1, || {
        Command::new("systemctl").arg("suspend").status().ok();
    });

    create_power_button(&button_grid, "Logout", 1, 1, || {
        Command::new("hyprctl")
            .arg("dispatch")
            .arg("exit")
            .status()
            .ok();
    });

    main_box.append(&button_grid);

    let cancel_button = gtk::Button::with_label("Cancel");
    cancel_button.add_css_class("cancel-button");
    let window_clone = window.clone();
    cancel_button.connect_clicked(move |_| {
        window_clone.close();
    });
    main_box.append(&cancel_button);

    window.set_child(Some(&main_box));

    apply_css_styling(&window);

    window.present();
}

fn create_power_button(
    grid: &gtk::Grid,
    label: &str,
    col: i32,
    row: i32,
    action: impl Fn() + Send + Sync + 'static,
) {
    let button = gtk::Button::with_label(label);
    button.add_css_class("power-button");

    let action_copy = std::sync::Arc::new(action);
    let action_clone = std::sync::Arc::clone(&action_copy);

    button.connect_clicked(move |_| {
        let action = std::sync::Arc::clone(&action_clone);
        std::thread::spawn(move || {
            action();
        });
    });

    grid.attach(&button, col, row, 1, 1);
}

fn apply_css_styling(_window: &gtk::ApplicationWindow) {
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
