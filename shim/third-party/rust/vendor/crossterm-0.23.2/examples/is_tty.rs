use crossterm::tty::IsTty;
use std::io::stdin;

pub fn main() {
    if stdin().is_tty() {
        println!("Is TTY");
    } else {
        println!("Is not TTY");
    }
}
