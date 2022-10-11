//! This example demonstrates how to run an application on stderr
//!
//! Run this example with
//!   cargo run --example stderr
//!

use crossterm::{
    event::{
        self,
        Event,
        KeyEvent,
        KeyCode::*,
    },
    cursor,
    queue,
    terminal::{self, EnterAlternateScreen, LeaveAlternateScreen},
    style::Color::*,
};
use termimad::*;

const MARKDOWN: &str = r#"
This screen is ran on *stderr*.
And when you quit it, it prints on *stdout*.
This makes it possible to run an application and choose what will be sent to any application calling yours.

For example, assuming you build this example with

    cargo build --example stderr

and then you run it with

    cd "$(target/debug/examples/stderr)"

what the application prints on stdout is used as argument to `cd`.

Try it out.

Hit any key to quit this screen:
* **1** will print `..`
* **2** will print `/`
* **3** will print `~`
* or anything else to print this text (so that you may copy-paste)
"#;

fn run_app<W>(skin: MadSkin, w: &mut W) -> Result<Option<char>, Error>
where
    W: std::io::Write,
{
    queue!(w, EnterAlternateScreen)?;
    terminal::enable_raw_mode()?;
    queue!(w, cursor::Hide)?; // hiding the cursor
    let mut area = Area::full_screen();
    area.pad(1, 1); // let's add some margin
    area.pad_for_max_width(120); // we don't want a too wide text column
    let mut view = MadView::from(MARKDOWN.to_owned(), area, skin);
    let mut user_char = None;
    loop {
        view.write_on(w)?;
        w.flush()?;
        if let Ok(Event::Key(KeyEvent{code, modifiers})) = event::read() {
            if !modifiers.is_empty() {
                continue;
            }
            match code {
                Up => view.try_scroll_lines(-1),
                Down => view.try_scroll_lines(1),
                PageUp => view.try_scroll_pages(-1),
                PageDown => view.try_scroll_pages(1),
                Char(c) => {
                    user_char = Some(c);
                    break;
                }
                _ => break,
            }
        }
    }
    terminal::disable_raw_mode()?;
    queue!(w, cursor::Show)?; // we must restore the cursor
    queue!(w, LeaveAlternateScreen)?;
    w.flush()?;
    Ok(user_char)
}

fn main() {
    let mut skin = MadSkin::default();
    skin.set_headers_fg(AnsiValue(178));
    skin.bold.set_fg(Yellow);
    skin.italic.set_fg(Magenta);
    skin.scrollbar.thumb.set_fg(AnsiValue(178));

    let mut stderr = std::io::stderr();
    match run_app(skin.clone(), &mut stderr).unwrap() {
        Some('1') => print!(".."),
        Some('2') => print!("/"),
        Some('3') => print!("~"),
        _ => skin.print_text(MARKDOWN),
    }
}
