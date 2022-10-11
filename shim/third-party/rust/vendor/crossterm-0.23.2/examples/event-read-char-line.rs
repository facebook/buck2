//! Demonstrates how to block read characters or a full line.
//! Just note that crossterm is not required to do this and can be done with `io::stdin()`.
//!
//! cargo run --example event-read-char-line

use crossterm::{
    event::{self, Event, KeyCode, KeyEvent},
    Result,
};

pub fn read_char() -> Result<char> {
    loop {
        if let Event::Key(KeyEvent {
            code: KeyCode::Char(c),
            ..
        }) = event::read()?
        {
            return Ok(c);
        }
    }
}

pub fn read_line() -> Result<String> {
    let mut line = String::new();
    while let Event::Key(KeyEvent { code, .. }) = event::read()? {
        match code {
            KeyCode::Enter => {
                break;
            }
            KeyCode::Char(c) => {
                line.push(c);
            }
            _ => {}
        }
    }

    Ok(line)
}

fn main() {
    println!("read line:");
    println!("{:?}", read_line());
    println!("read char:");
    println!("{:?}", read_char());
}
