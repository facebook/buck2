/*! This crate lets you display simple markdown snippets
or scrollable wrapped markdown texts in the terminal.

In order to use Termimad you typically need
* some *markdown*: a string which you can have loaded or dynamically built
* a *skin*: which defines the colors and style attributes of every parts

Additionnaly, you might define an *area* of the screen in which to draw (and maybe scroll).

# The skin

It's an instance of [`MadSkin`](struct.MadSkin.html) whose fields you customize according
to your tastes or (better) to your application's configuration.


```rust
use crossterm::style::{Color::*, Attribute::*};
use termimad::*;

// start with the default skin
let mut skin = MadSkin::default();
// let's decide bold is in light gray
skin.bold.set_fg(gray(20));
// let's make strikeout not striked out but red, with no specific background, and bold
skin.strikeout = CompoundStyle::new(Some(Red), None, Bold.into());
```

**Beware:**
* you may define colors in full [`rgb`](fn.rgb.html) but this will limit compatibility with old
terminals. It's recommended to stick to [Ansi colors](fn.ansi.html), [gray levels](fn.gray.html), or [Crossterm predefined values](https://docs.rs/crossterm/0.9.6/crossterm/enum.Color.html).
* styles are composed. For example a word may very well be italic, bold and striked out. It might not be wise to have them differ only by their background color for example.

# Display a simple inline snippet


```
# use termimad::*;
// with the default skin, nothing simpler:
termimad::print_inline("value: **52**");
```
# Print a text

A multi-line markdown string can be printed the same way than an *inline* snippet, but you usually want it to be wrapped according to the available terminal width.

```rust,no_run
# use termimad::*;
# let skin = MadSkin::default();
# let my_markdown = "#title\n* item 1\n* item 2";
eprintln!("{}", skin.term_text(my_markdown));
```

[`MadSkin`](struct.MadSkin.html) contains other functions to prepare a text for no specific size or for one which isn't the terminal's width. It also offers several functions to print it either on `stdout` or on a given `Write`.

# Display a text, maybe scroll it

A terminal application often uses an *alternate* screen instead of just dumping its text to stdout, and you often want to display in a specific rect of that screen, with adequate wrapping and not writing outside that rect.

You may also want to display a scrollbar if the text doesn't fit the area. A [`MadView`](struct.MadView.html) makes that simple:

```
# use termimad::*;
# let markdown = String::from("#title\n* item 1\n* item 2");
# let skin = MadSkin::default();
let area = Area::new(0, 0, 10, 12);
let mut view = MadView::from(markdown, area, skin);
view.write().unwrap();
```

If you don't want to give ownership of the skin, markdown and area, you may prefer to use a [`TextView`](struct.TextView.html).

You may see how to write a text viewer responding to key inputs to scroll a markdown text in [the scrollable example](https://github.com/Canop/termimad/blob/master/examples/scrollable/main.rs).

# Templates

In order to separate the rendering format from the content, the `format!` macro is not always a good solution because you may not be sure the content is free of characters which may mess the markdown.

A solution is to use one of the templating functions or macros.

Example:

```
# #[macro_use] extern crate minimad;
# use termimad::*;
# let skin = MadSkin::default();
mad_print_inline!(
    &skin,
    "**$0 formula:** *$1*", // the markdown template, interpreted once
    "Disk",  // fills $0
    "2*π*r", // fills $1. Note that the stars don't mess the markdown
);
```

Main difference with using `print!(format!( ... ))`:
* the markdown parsing and template building are done only once (using `once_cell` internally)
* the given values aren't interpreted as markdown fragments and don't impact the style
* arguments can be omited, repeated, given in any order
* no support for fmt parameters or arguments other than `&str` *(in the current version)*

You'll find more examples and advice in the *templates* example.

# Examples

The repository contains several other examples, which hopefully cover the whole API while being simple enough. It's recommended you start by trying them or at least glance at their code.

*/

mod ask;
mod area;
mod code;
mod color;
mod composite;
mod compound_style;
mod displayable_line;
mod errors;
mod events;
mod fit;
mod inline;
mod line;
mod line_style;
mod macros;
mod rect;
mod scrollbar_style;
mod skin;
mod spacing;
mod styled_char;
mod tbl;
mod text;
mod tokens;
mod views;

pub use {
    ask::*,
    area::{compute_scrollbar, terminal_size, Area},
    color::*,
    composite::FmtComposite,
    compound_style::CompoundStyle,
    errors::Error,
    events::{TimedEvent, EventSource, EventSourceOptions},
    fit::*,
    inline::FmtInline,
    line::FmtLine,
    line_style::LineStyle,
    minimad::Alignment,
    rect::*,
    scrollbar_style::ScrollBarStyle,
    skin::MadSkin,
    spacing::Spacing,
    styled_char::StyledChar,
    text::FmtText,
    views::{
        InputField, ListView, ListViewCell, ListViewColumn,
        MadView, ProgressBar, TextView,
    },
};
pub use minimad;

use tokens::*;

/// Return a reference to the global skin
///
/// If you want a new default skin so that you can set
/// colors or styles, get a separate instance
/// with `Skin::default()` instead.
pub fn get_default_skin() -> &'static MadSkin {
    use minimad::once_cell::sync::Lazy;
    static DEFAULT_SKIN: Lazy<MadSkin> = Lazy::new(MadSkin::default);
    &DEFAULT_SKIN
}

/// Return a formatted line, which implements Display.
///
/// This uses the default skin.
/// Don't use if you expect your markdown to be several lines.
pub fn inline(src: &str) -> FmtInline<'_, '_> {
    get_default_skin().inline(src)
}

/// Return an unwrapped formatted text, implementing Display.
///
/// This uses the default skin and doesn't wrap the lines
///  at all. Most often you'll prefer to use `term_text`
///  which makes a text wrapped for the current terminal.
pub fn text(src: &str) -> FmtText<'_, '_> {
    get_default_skin().text(src, None)
}

/// Return a terminal wrapped formatted text, implementing Display.
///
/// This uses the default skin and the terminal's width
pub fn term_text(src: &str) -> FmtText<'_, '_> {
    get_default_skin().term_text(src)
}

/// Write a string interpreted as markdown with the default skin.
pub fn print_inline(src: &str) {
    get_default_skin().print_inline(src);
}

/// Write a text interpreted as markdown with the default skin.
pub fn print_text(src: &str) {
    get_default_skin().print_text(src);
}
