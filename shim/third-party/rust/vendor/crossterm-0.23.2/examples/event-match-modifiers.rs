//! Demonstrates how to match on modifiers like: Control, alt, shift.
//!
//! cargo run --example event-match-modifiers

use crossterm::event::{Event, KeyCode, KeyEvent, KeyModifiers};

fn match_event(read_event: Event) {
    match read_event {
        // Match one one modifier:
        Event::Key(KeyEvent {
            modifiers: KeyModifiers::CONTROL,
            code,
        }) => {
            println!("Control + {:?}", code);
        }
        Event::Key(KeyEvent {
            modifiers: KeyModifiers::SHIFT,
            code,
        }) => {
            println!("Shift + {:?}", code);
        }
        Event::Key(KeyEvent {
            modifiers: KeyModifiers::ALT,
            code,
        }) => {
            println!("Alt + {:?}", code);
        }

        // Match on multiple modifiers:
        Event::Key(KeyEvent { code, modifiers }) => {
            if modifiers == (KeyModifiers::ALT | KeyModifiers::SHIFT) {
                println!("Alt + Shift {:?}", code);
            } else {
                println!("({:?}) with key: {:?}", modifiers, code)
            }
        }

        _ => {}
    }
}

fn main() {
    match_event(Event::Key(KeyEvent {
        modifiers: KeyModifiers::CONTROL,
        code: KeyCode::Char('z'),
    }));
    match_event(Event::Key(KeyEvent {
        modifiers: KeyModifiers::SHIFT,
        code: KeyCode::Left,
    }));
    match_event(Event::Key(KeyEvent {
        modifiers: KeyModifiers::ALT,
        code: KeyCode::Delete,
    }));
    match_event(Event::Key(KeyEvent {
        modifiers: KeyModifiers::ALT | KeyModifiers::SHIFT,
        code: KeyCode::Right,
    }));
    match_event(Event::Key(KeyEvent {
        modifiers: KeyModifiers::ALT | KeyModifiers::CONTROL,
        code: KeyCode::Home,
    }));
}
