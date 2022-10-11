//! The clipboard here is provided by the terminal-clipboard crate
//! but you may replace it with another one

use {
    cli_log::*,
    termimad::InputField,
};

pub fn copy_from_input(input: &mut InputField) -> bool {
    let s = input.copy_selection();
    if let Err(err) = terminal_clipboard::set_string(s) {
        warn!("error while setting clipboard: {}", err);
        false
    } else {
        true
    }
}
pub fn cut_from_input(input: &mut InputField) -> bool {
    let s = input.cut_selection();
    if let Err(err) = terminal_clipboard::set_string(s) {
        warn!("error while setting clipboard: {}", err);
        false
    } else {
        true
    }
}
pub fn paste_into_input(input: &mut InputField) -> bool {
    if let Ok(s) = terminal_clipboard::get_string() {
        input.replace_selection(s);
        true
    } else {
        false
    }
}
