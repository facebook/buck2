//! run this example with
//!   cargo run --example render-input-markdown
//!
//! if you want to have a log file generated, run
//!   TERMIMAD_LOG=debug cargo run --example render-input-markdown

#[macro_use]
extern crate cli_log;

mod clipboard;
mod view;

use {
    anyhow::{self},
    crokey::key,
    crossterm::{
        cursor,
        event::{
            DisableMouseCapture, EnableMouseCapture,
        },
        terminal::{
            self,
            EnterAlternateScreen, LeaveAlternateScreen,
        },
        QueueableCommand,
    },
    std::io::{BufWriter, stdout, Write},
    termimad::*,
};

fn main() -> anyhow::Result<()> {
    init_cli_log!();
    let mut w = BufWriter::new(stdout());
    w.queue(EnterAlternateScreen)?;
    terminal::enable_raw_mode()?;
    w.queue(cursor::Hide)?;
    w.queue(EnableMouseCapture)?;
    let res = run_in_alternate(&mut w);
    w.queue(DisableMouseCapture)?;
    w.queue(cursor::Show)?; // we must restore the cursor
    terminal::disable_raw_mode()?;
    w.queue(LeaveAlternateScreen)?;
    w.flush()?;
    res
}

/// run the event loop, in a terminal which must be in alternate
fn run_in_alternate<W: Write>(w: &mut W) -> anyhow::Result<()> {
    let mut view = view::View::new(Area::full_screen());
    view.queue_on(w)?;
    w.flush()?;
    info!("clipboard backend type: {}", terminal_clipboard::get_type());
    let event_source = EventSource::new()?;
    for timed_event in event_source.receiver() {
        let mut quit = false;
        debug!("event: {:?}", timed_event);
        if timed_event.is_key(key!(ctrl-q)) {
            quit = true;
        } else if view.apply_timed_event(timed_event) {
            view.queue_on(w)?;
            w.flush()?;
        }
        event_source.unblock(quit); // Don't forget to unblock the event source
        if quit {
            break;
        }
    }
    Ok(())
}
