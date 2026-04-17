//! A tiny binary that exercises both library crates.

use crate_b::WidgetB;

fn main() {
    let w = WidgetB::new("world", 3);
    println!("{} ({})", w.greet(), w.count);
}
