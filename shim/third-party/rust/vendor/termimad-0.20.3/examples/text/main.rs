use crossterm::{execute, style::Color::*, terminal};
use termimad::*;

static MD: &str = r#"
----

# Markdown Rendering on Terminal

Here's the code to print this markdown block in the terminal:

```
let mut skin = MadSkin::default();
skin.set_headers_fg(rgb(255, 187, 0));
skin.bold.set_fg(Yellow);
skin.italic.set_fgbg(Magenta, rgb(30, 30, 40));
skin.bullet = StyledChar::from_fg_char(Yellow, '⟡');
skin.quote_mark = StyledChar::from_fg_char(Yellow, '▐');
skin.bullet = StyledChar::from_fg_char(Yellow, '⟡');
skin.quote_mark.set_fg(Yellow);
println!("{}", skin.term_text(my_markdown));
```

**Termimad** is built over **Crossterm** and **Minimad**.

----

## Why use Termimad

* *display* static or dynamic *rich* texts
* *separate* your text building code or resources from its styling
* *configure* your colors

## Real use cases

* the help screen of a terminal application
* small snippets of rich text in a bigger application
* terminal app output

## What people say about Termimad

> I find it convenient *[Termimad's author]*

----
"#;

fn print_direct(skin: &MadSkin) {
    skin.print_text(MD);
}

fn print_in_text_view(skin: MadSkin) {
    let mut w = std::io::stdout();
    execute!(w, terminal::Clear(terminal::ClearType::All)).unwrap();
    let mut area = Area::full_screen();
    area.pad(2, 1); // let's add some margin
    let text = skin.area_text(MD, &area);
    let view = TextView::from(&area, &text);
    view.write().unwrap();
}

/// Choose DIRECT = true for a simple writting in stdout,
/// and DIRECT = false for a whole terminal display.
/// Note that this doesn't use an alternate screen. Look
/// at the "scrollable" example to see an alternate screen
/// being used.
const DIRECT: bool = true;

fn main() {
    let mut skin = MadSkin::default();
    skin.set_headers_fg(rgb(255, 187, 0));
    skin.bold.set_fg(Yellow);
    skin.italic.set_fgbg(Magenta, rgb(30, 30, 40));
    skin.bullet = StyledChar::from_fg_char(Yellow, '⟡');
    skin.quote_mark.set_fg(Yellow);
    if DIRECT {
        print_direct(&skin);
    } else {
        print_in_text_view(skin);
    }
}
