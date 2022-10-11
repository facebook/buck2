

[![MIT][s2]][l2] [![Latest Version][s1]][l1] [![docs][s3]][l3] [![Chat on Miaou][s4]][l4]

[s1]: https://img.shields.io/crates/v/minimad.svg
[l1]: https://crates.io/crates/minimad

[s2]: https://img.shields.io/badge/license-MIT-blue.svg
[l2]: minimad/LICENSE

[s3]: https://docs.rs/minimad/badge.svg
[l3]: https://docs.rs/minimad/

[s4]: https://miaou.dystroy.org/static/shields/room.svg
[l4]: https://miaou.dystroy.org/3

A simple, non universal purpose, markdown parser.

If you're looking for a Markdown parser, this one is probably *not* the one you want:

Minimad can be used on its own but is first designed for the [termimad](https://github.com/Canop/termimad) lib, which displays static and dynamic markdown snippets on a terminal without mixing the skin with the code. Minimad sports a line-oriented flat structure (i.e. not a tree) which might not suit your needs.

If you still think you might use Minimad directly (not through Temimad), you may contact me on Miaou for advice.

### Usage


```toml
[dependencies]
minimad = "0.7"
```

```rust
assert_eq!(
    Line::from("## a header with some **bold**!"),
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
    Line::from("Hello ~~wolrd~~ **World**. *Code*: `sqrt(π/2)`"),
    Line::new_paragraph(vec![
        Compound::raw_str("Hello "),
        Compound::raw_str("wolrd").strikeout(),
        Compound::raw_str(" "),
        Compound::raw_str("World").bold(),
        Compound::raw_str(". "),
        Compound::raw_str("Code").italic(),
        Compound::raw_str(": "),
        Compound::raw_str("sqrt(π/2)").code(),
    ])
);
```

