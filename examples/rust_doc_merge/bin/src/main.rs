//! A tiny binary that exercises all three library crates.

use crate_b::WidgetB;

/// Return a resource path from [`buck_resources::get`], demonstrating a
/// cross-crate reference whose docs link out to
/// <https://docs.rs/buck-resources/1.0.0/>.
pub fn resource_path() -> Option<std::path::PathBuf> {
    buck_resources::get("root//some:target").ok()
}

fn main() {
    let w = WidgetB::new("world", 3);
    println!("{} ({})", w.greet(), w.count);
    if let Some(p) = resource_path() {
        println!("resource at {:?}", p);
    }
}
