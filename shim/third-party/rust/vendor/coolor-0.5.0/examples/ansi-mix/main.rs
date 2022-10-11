//! Run this with
//!     cargo run --example ansi-mix
use {
    coolor::*,
    crossterm::style::{self, Stylize},
    rand::Rng,
};

/// number of steps in gradients
const N: usize = 20;

fn rand_ansi() -> AnsiColor {
    AnsiColor::new(rand::thread_rng().gen())
}

fn print_ansi(ansi: AnsiColor) {
    print!("{}", "█".with(style::Color::AnsiValue(ansi.code)));
}

fn bar(n: usize) -> String {
    "─".repeat(n)
}

fn main() {
    const T1: &str = "HSL walk";
    const T2: &str = "RGB walk";
    const T3: &str = "blend";
    let b = bar(N + 2);
    let b = &b;
    println!(
        "\n{:^w$}",
        "Blending random ANSI colors, all variants still ANSI",
        w = 3 * N + 18
    );
    println!(" ┌─────┬─────┬{}┬{}┬{}┐", b, b, b);
    println!(" │ src │ dst │{:^w$}│{:^w$}│{:^w$}│", T1, T2, T3, w = N + 2);
    println!(" ├─────┼─────┼{}┼{}┼{}┤", b, b, b);
    for _ in 0..20 {
        let c1 = rand_ansi();
        let c2 = rand_ansi();
        print!(" │ ");
        for _ in 0..3 {
            print_ansi(c1);
        }
        print!(" │ ");
        for _ in 0..3 {
            print_ansi(c2);
        }
        print!(" │ ");
        let hsl1 = c1.to_hsl();
        let hsl2 = c2.to_hsl();
        let n = N - 1;
        for i in 0..=n {
            let hsl = Hsl::mix(hsl1, (n - i) as f32, hsl2, i as f32);
            print_ansi(hsl.to_ansi());
        }
        print!(" │ ");
        let rgb1 = c1.to_rgb();
        let rgb2 = c2.to_rgb();
        for i in 0..=n {
            let rgb = Rgb::mix(rgb1, (n - i) as f32, rgb2, i as f32);
            print_ansi(rgb.to_ansi());
        }
        print!(" │ ");
        for i in 0..=n {
            let c = Color::blend(c1, (n - i) as f32, c2, i as f32);
            print_ansi(c.ansi());
        }
        print!(" │ ");
        println!();
    }
    println!(" └─────┴─────┴{}┴{}┴{}┘\n", b, b, b);
}
