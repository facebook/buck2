extern crate terminfo;

use std::io;
use terminfo::{Database, capability as cap};

fn main() {
    let info = Database::from_env().unwrap();

    if let Some(set_attributes) = info.get::<cap::SetAttributes>() {
        let clear = info.get::<cap::ExitAttributeMode>().unwrap();

        set_attributes.expand()
            .bold(true)
            .underline(true)
            .to(io::stdout())
            .unwrap();

        println!("bold and underline");

        clear.expand().to(io::stdout()).unwrap();
    } else {
        println!("The terminal does not support mass-setting attributes");
    }
}
