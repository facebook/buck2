extern crate yansi_term;
use yansi_term::{Colour::*, Style};

// This example prints out the 16 basic colours.

fn main() {
    println!(
        "{}",
        Red.paint_fn(|f| {
            f.write_str("RED")?;
            Style::new().bold().write_prefix(f)?;
            f.write_str("RED_BOLD")?;
            Style::write_reset(f)?;
            Black.write_prefix(f)?;
            f.write_str("BLACK")
        })
    );
}
