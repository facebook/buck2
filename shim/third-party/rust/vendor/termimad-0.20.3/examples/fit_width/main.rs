//! run this example with
//!   cargo run --example fit_width
//!
use {
    crossterm::{
        cursor::{self, Hide, Show},
        event::{self, Event},
        ExecutableCommand,
        terminal::{self, EnterAlternateScreen, LeaveAlternateScreen},
        style::Color::*,
    },
    minimad::Composite,
    std::io::{stderr, Write},
    termimad::*,
};

static TEXTS: &[&str] = &[
    "This demo shows fitting without wrapping, with wide chars support: *一曰道，二曰天*.",
    "Resize the terminal to see how markdown fitting works.",
    "Hit *any key* to quit.",
    "some Code: `fc.fill_width(width, align, skin);`",
    "一曰道，二曰天，三曰地，四曰將，五曰法。",
    "`cp src/displayable_line.rs ../other_thing/src/displayable_stuff/line.rs`",
    "",
];

fn make_line(md: &'static str, skin: &MadSkin, width: usize, align: Alignment) -> FmtComposite<'static> {
    let composite = Composite::from_inline(md);
    let mut fc = FmtComposite::from(composite, skin);
    fc.fill_width(width, align, skin);
    fc
}

fn make_all_lines(skin: &MadSkin, width: usize) -> Vec<FmtComposite<'static>> {
    let mut lines = Vec::new();
    lines.push(make_line("**Left align:**", skin, width, Alignment::Left));
    for md in TEXTS {
        lines.push(make_line(md, skin, width, Alignment::Left));
    }
    lines.push(make_line("**Auto (with internal ellisions):**", skin, width, Alignment::Left));
    for md in TEXTS {
        lines.push(make_line(md, skin, width, Alignment::Unspecified));
    }
    lines.push(make_line("**Right align:**", skin, width, Alignment::Left));
    for md in TEXTS {
        lines.push(make_line(md, skin, width, Alignment::Right));
    }
    lines.push(make_line("**Center align:**", skin, width, Alignment::Left));
    for md in TEXTS {
        lines.push(make_line(md, skin, width, Alignment::Center));
    }
    lines
}

fn run_app(skin: &MadSkin) -> Result<(), Error> {
    let mut w = stderr(); // we could also have used stdout
    let w = &mut w;
    w.execute(EnterAlternateScreen)?;
    terminal::enable_raw_mode()?;
    w.execute(Hide)?; // hiding the cursor
    let (mut width, mut height) = terminal_size();
    loop {
        let mut lines = make_all_lines(skin, width as usize);
        let mut lines = lines.drain(..);
        for y in 0..height {
            w.execute(cursor::MoveTo(0, y))?;
            if let Some(line) = lines.next() {
                write!(w, "{}", FmtInline{
                    skin,
                    composite: line,
                })?
            }
        }
        match event::read() {
            Ok(Event::Key(_)) => {
                break;
            }
            Ok(Event::Resize(w, h)) => {
                width = w;
                height = h;
            }
            _ => {}
        }
    }
    terminal::disable_raw_mode()?;
    w.execute(Show)?; // we must restore the cursor
    w.execute(LeaveAlternateScreen)?;
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
    run_app(&skin)
}

