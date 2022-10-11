use crossterm::style::{Attribute::*, Color::*};
use termimad::*;

fn show(skin: &MadSkin, src: &str) {
    println!(" Raw       : {}", &src);
    println!(" Formatted : {}\n", skin.inline(src));
}

fn show_some(skin: &MadSkin) {
    show(&skin, "*Hey* **World!** Here's `some(code)`");
    show(&skin, "some *nested **style***");
}

fn main() {
    println!();
    println!("\nWith the default skin:\n");
    let mut skin = MadSkin::default();
    show_some(&skin);
    println!("\nWith a customized skin:\n");
    skin.bold.set_fg(Yellow);
    skin.italic = CompoundStyle::with_bg(DarkBlue);
    skin.inline_code.add_attr(Reverse);
    show_some(&skin);

    let mut skin = MadSkin::default();
    skin.bold.set_fg(Yellow);
    skin.print_inline("*Hey* **World!** Here's `some(code)`");
    skin.paragraph.set_fgbg(Magenta, rgb(30, 30, 40));
    skin.italic.add_attr(Underlined);
    skin.italic.add_attr(OverLined);
    println!(
        "\nand now {}\n",
        skin.inline("a little *too much* **style!** (and `some(code)` too)")
    );
}
