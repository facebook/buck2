//! run this example with
//!   cargo run --example scrollable
//!
use crossterm::{
    cursor::{ Hide, Show},
    event::{
        self,
        Event,
        KeyEvent,
        KeyCode::*,
    },
    queue,
    terminal::{
        self,
        Clear,
        ClearType,
        EnterAlternateScreen,
        LeaveAlternateScreen,
    },
    style::Color::*,
};
use std::io::{stdout, Write};
use termimad::*;

fn view_area() -> Area {
    let mut area = Area::full_screen();
    area.pad_for_max_width(120); // we don't want a too wide text column
    area
}

fn run_app(skin: MadSkin) -> Result<(), Error> {
    let mut w = stdout(); // we could also have used stderr
    queue!(w, EnterAlternateScreen)?;
    terminal::enable_raw_mode()?;
    queue!(w, Hide)?; // hiding the cursor
    let mut view = MadView::from(MD.to_owned(), view_area(), skin);
    loop {
        view.write_on(&mut w)?;
        w.flush()?;
        match event::read() {
            Ok(Event::Key(KeyEvent{code, ..})) => {
                match code {
                    Up => view.try_scroll_lines(-1),
                    Down => view.try_scroll_lines(1),
                    PageUp => view.try_scroll_pages(-1),
                    PageDown => view.try_scroll_pages(1),
                    _ => break,
                }
            }
            Ok(Event::Resize(..)) => {
                queue!(w, Clear(ClearType::All))?;
                view.resize(&view_area());
            }
            _ => {}
        }
    }
    terminal::disable_raw_mode()?;
    queue!(w, Show)?; // we must restore the cursor
    queue!(w, LeaveAlternateScreen)?;
    w.flush()?;
    Ok(())
}

fn make_skin() -> MadSkin {
    let mut skin = MadSkin::default();
    skin.table.align = Alignment::Center;
    skin.set_headers_fg(AnsiValue(178));
    skin.bold.set_fg(Yellow);
    skin.italic.set_fg(Magenta);
    skin.scrollbar.thumb.set_fg(AnsiValue(178));
    skin.code_block.align = Alignment::Center;
    skin
}

fn main() -> Result<(), Error> {
    let skin = make_skin();
    run_app(skin)
}

static MD: &str = r#"# Scrollable Markdown in Termimad

Use the **↓** and **↑** arrow keys to scroll this page.
Use any other key to quit the application.

*Now I'll describe this example with more words than necessary, in order to be sure to demonstrate scrolling (and **wrapping**, too, thanks to long sentences).*

## What's shown

* an **area** fitting the screen (with some side margin to be prettier)
* a markdown text **parsed**, **skinned**, **wrapped** to fit the width
* a **scrollable** view in *raw terminal mode*

## Area

The area specifies the part of the screen where we'll display our markdown. The margin in this example is just here to show that wrapping is handled:

    let mut area = Area::full_screen();
    area.pad(2, 1); // let's add some margin

*(yes the code block centering in this example is a little too much, it's just here to show what's possible)*

## Parsed Markdown

The text is parsed from a string. In this example we directly wrap it for the width of the area:

    let text = skin.area_wrapped_text(markdown, &area);

If we wanted to modify the parsed representation, or modify the area width, we could also have kept the parsed text (*but parsing is cheap*).

## The TextView

It's just a text put in an area, tracking your **scroll** position (and whether you want the scrollbar to be displayed).

    let mut text_view = TextView::from(&area, &text);

## Really Scrolling

Not two applications handle events in the same way. **Termimad** doesn't try to handle this but lets you write it yourself, which is fairly easily done with **Crossterm** for example:

```
let mut events = TerminalInput::new().read_sync();
loop {
    text_view.write()?;
    if let Some(Keyboard(key)) = events.next() {
        match key {
            Up => text_view.try_scroll_lines(-1),
            Down => text_view.try_scroll_lines(1),
            PageUp => text_view.try_scroll_pages(-1),
            PageDown => text_view.try_scroll_pages(1),
            _ => break,
        }
    }
}
```

## Skin

We want *shiny **colors*** (and unreasonnable centering):

    let mut skin = MadSkin::default();
    skin.set_headers_fg(rgb(255, 187, 0));
    skin.bold.set_fg(Yellow);
    skin.italic.set_fgbg(Magenta, rgb(30, 30, 40));
    skin.scrollbar.track.set_fg(Rgb{r:30, g:30, b:40});
    skin.scrollbar.thumb.set_fg(Rgb{r:67, g:51, b:0});
    skin.code_block.align = Alignment::Center;

The scrollbar's colors were also adjusted to be consistent.

## Usage

* **↓** and **↑** arrow keys : scroll this page
* any other key : quit

## And let's just finish by a table

It's a little out of context but it shows how a wide table can be wrapped in a thin terminal.

|feature|supported|details|
|-|:-:|-
| tables | yes | pipe based only, alignement not yet supported
| italic, bold | yes | star based only|
| inline code | yes |
| code bloc | yes |with tabs. Fences not supported
| crossed text |  ~~not yet~~ | wait... now it works!
| phpbb like links | no | (because it's preferable to show an URL in a terminal)

"#;
