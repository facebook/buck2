//! Run this with
//!     cargo run --example ansi-variations
use {
    coolor::AnsiColor,
    crossterm::style::{Color, Stylize},
};

fn print_color(ansi: AnsiColor) {
    print!("{}", "█".with(Color::AnsiValue(ansi.code)));
}

fn main() {
    println!(" Variations on ANSI colors:");
    println!(" ┌──────┬────────┬─────────────────┬─────────────┐");
    println!(" │ code │ normal │    luminosity   │ saturation  │");
    println!(" ├──────┼────────┼─────────────────┼─────────────┤");
    for code in 1..=255 {
        let ansi = AnsiColor::new(code);
        print!(" │  {:>3} │ ", code);
        for _ in 0..6 {
            print_color(ansi);
        }
        print!(" │ ");
        for i in -7..=7 {
            print_color(ansi.with_luminosity_change((i as f32) * 0.1));
        }
        print!(" │ ");
        for i in -10..=0 {
            print_color(ansi.with_saturation_change((i as f32) * 0.1));
        }
        println!(" │");
    }
    println!(" └──────┴────────┴─────────────────┴─────────────┘");
}
