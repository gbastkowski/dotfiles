use gtk::prelude::*;
use gtk_layer_shell::LayerShell;
use std::process::Command;

fn main() {
    gtk::init().expect("Failed to initialize GTK");

    let window = gtk::Window::new(gtk::WindowType::Toplevel);
    window.set_title("Power Menu");
    window.set_default_size(280, 320);
    window.set_resizable(false);

    window.init_layer_shell();
    window.set_layer(gtk_layer_shell::Layer::Overlay);
    window.set_anchor(gtk_layer_shell::Edge::Top, false);
    window.set_layer_shell_margin(gtk_layer_shell::Edge::Top, 40);
    window.set_layer_shell_margin(gtk_layer_shell::Edge::Bottom, 40);

    let main_box = gtk::Box::new(gtk::Orientation::Vertical, 15);
    main_box.set_margin_top(20);
    main_box.set_margin_bottom(20);
    main_box.set_margin_start(20);
    main_box.set_margin_end(20);

    let title = gtk::Label::new(Some("Power Menu"));
    title.style_context().add_class("title");
    main_box.add(&title);

    let separator = gtk::Separator::new(gtk::Orientation::Horizontal);
    main_box.add(&separator);

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

    main_box.add(&button_grid);

    let cancel_button = gtk::Button::with_label("Cancel");
    cancel_button.style_context().add_class("cancel-button");
    cancel_button.connect_clicked(|_| {
        gtk::main_quit();
    });
    main_box.add(&cancel_button);

    window.add(&main_box);

    apply_css_styling(&window);

    window.connect_destroy(|_| {
        gtk::main_quit();
    });

    window.show_all();
    gtk::main();
}

fn create_power_button(
    grid: &gtk::Grid,
    label: &str,
    col: i32,
    row: i32,
    action: impl Fn() + Send + Sync + 'static,
) {
    let button = gtk::Button::with_label(label);
    button.style_context().add_class("power-button");

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

fn apply_css_styling(window: &gtk::Window) {
    let css_provider = gtk::CssProvider::new();
    css_provider
        .load_from_data(
            b"
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
        ",
        )
        .expect("Failed to load CSS");

    gtk::StyleContext::add_provider_for_screen(
        &gtk::prelude::GtkWindowExt::screen(window).unwrap(),
        &css_provider,
        gtk::STYLE_PROVIDER_PRIORITY_APPLICATION,
    );
}
