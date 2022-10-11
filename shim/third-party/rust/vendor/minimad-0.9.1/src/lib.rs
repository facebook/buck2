/*!
This crate provides a *very* simple markdown parser.

It's the basis of the [termimad](https://github.com/Canop/termimad) lib, which displays static and dynamic markdown snippets on a terminal without mixing the skin with the code and wrapping the text and tables as needed.

It can be used on its own:

```rust
use minimad::*;

assert_eq!(
    parse_line("## a header with some **bold**!"),
    Line::new_header(
        2,
        vec![
            Compound::raw_str("a header with some "),
            Compound::raw_str("bold").bold(),
            Compound::raw_str("!"),
        ]
    )
);

assert_eq!(
    parse_inline("*Italic then **bold and italic `and some *code*`** and italic*"),
    Composite::from(vec![
        Compound::raw_str("Italic then ").italic(),
        Compound::raw_str("bold and italic ").bold().italic(),
        Compound::raw_str("and some *code*").bold().italic().code(),
        Compound::raw_str(" and italic").italic(),
    ])
);
```

The [mad_inline] macro is useful for semi-dynamic markdown building: it prevents characters like `'*'` from messing the style:

```
use minimad::*;

let md1 = mad_inline!(
    "**$0 formula:** *$1*", // the markdown template, interpreted only once
    "Disk",  // fills $0
    "2*π*r", // fills $1. Note that the stars don't mess the markdown
);
let md2 = Composite::from(vec![
    Compound::raw_str("Disk formula:").bold(),
    Compound::raw_str(" "),
    Compound::raw_str("2*π*r").italic(),
]);
```

Note that Termimad contains macros and tools to deal with templates. If your goal is to print in the console you should use Termimad's functions.

*/

pub mod clean;
mod line_parser;
mod markdown;
mod template;

pub use {
    clean::*,
    line_parser::*,
    markdown::*,
    template::*,
};

/// reexport so that macros can be used without imports
pub use once_cell;

/// parse a markdown text
pub fn parse_text(md: &str) -> Text {
    Text::from(md)
}

/// parse a line, which is meant to be part of a markdown text.
/// This function shouldn't usually be used: if you don't want
/// a text you probably need `parse_inline`
pub fn parse_line(md: &str) -> Line {
    Line::from(md)
}

/// parse a monoline markdown snippet which isn't from a text.
/// Don't produce some types of line: TableRow, Code, ListItem
///  as they only make sense in a multi-line text.
pub fn parse_inline(md: &str) -> Composite {
    Composite::from_inline(md)
}
