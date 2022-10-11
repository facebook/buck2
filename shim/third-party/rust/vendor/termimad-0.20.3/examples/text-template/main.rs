/*!
This example demonstrates the use of templates for building whole texts.

You execute this example with
     cargo run --example text-template
*/

use {
    crossterm::style::Color::*,
    minimad::{TextTemplate, OwningTemplateExpander},
    termimad::*,
};

static TEMPLATE: &str = r#"
-----------
# ${app-name} v${app-version}
**${app-name}** is *fantastic*!

## Modules in a table

|:-:|:-:|:-:|
|**name**|**path**|**count**|
|-:|:-:|:-:|
${module-rows
|**${module-name}**|`${app-version}/${module-key}`|${module-count}|
}
|-|-|-|

## Modules again (but with a different presentations):

${module-rows
**${module-name}** (*${module-key}*): count: ${module-count}
 ${module-description}

}
## Example of a code block

    ${some-function}

## Fenced Code block with a placeholder

```rust
this_is_some(code);
this_part.is(${dynamic});
```

That's all for now.
-----------
"#;

/// a struct to illustrate several ways to format its information
struct Module {
    name: &'static str,
    key: &'static str,
    count: u64,
    description: &'static str,
}

/// some example data
const MODULES: &'static [Module] = &[
    Module { name: "lazy-regex", key: "lrex", count: 0, description: "eases regexes"},
    Module { name: "termimad", key: "tmd", count: 7, description: "do things on *terminal*" },
    Module { name: "bet", key: "bet", count: 11, description: "do formulas, unlike `S=π*r²`" },
    Module { name: "umask", key: "mod", count: 2, description: "my mask" },
];

fn main() -> Result<(), Error> {
    // fill an expander with data
    let mut expander = OwningTemplateExpander::new();
    expander
        .set("app-name", "MyApp")
        .set("app-version", "42.5.3")
        .set_md("dynamic", "filled_by_**template**"); // works in code too
    for module in MODULES {
        expander.sub("module-rows")
            .set("module-name", module.name)
            .set("module-key", module.key)
            .set("module-count", format!("{}", module.count))
            .set_md("module-description", module.description);
    }
    expander.set_lines("some-function", r#"
        fun test(a rational) {
            irate(a)
        }
        "#);
    // use the data to build the markdown text and print it
    let skin = make_skin();
    let template = TextTemplate::from(TEMPLATE);
    let text = expander.expand(&template);
    let (width, _) = terminal_size();
    let fmt_text = FmtText::from_text(&skin, text, Some(width as usize));
    print!("{}", fmt_text);
    Ok(())
}

fn make_skin() -> MadSkin {
    let mut skin = MadSkin::default();
    skin.set_headers_fg(AnsiValue(178));
    skin.headers[2].set_fg(gray(22));
    skin.bold.set_fg(Yellow);
    skin.italic.set_fg(Magenta);
    skin.scrollbar.thumb.set_fg(AnsiValue(178));
    skin
}
