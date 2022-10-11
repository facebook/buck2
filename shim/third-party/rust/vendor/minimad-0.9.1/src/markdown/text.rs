use crate::*;

/// a text, that is just a collection of lines
#[derive(Debug, Default, PartialEq, Eq, Clone)]
pub struct Text<'a> {
    pub lines: Vec<Line<'a>>,
}

impl<'s> From<&'s str> for Text<'s> {
    /// build a text from a multi-line string interpreted as markdown
    fn from(md: &str) -> Text<'_> {
        Text::from_md_lines(md.lines())
    }
}

impl<'s> Text<'s> {
    /// parse a text from markdown lines.
    ///
    /// The main reason to use this one is to use
    /// the `clean::lines` function which is useful with
    /// raw literals:
    /// ```
    /// use minimad::{clean, Text};
    /// let md = clean::lines(r#"
    ///     * some bullet item
    ///     some text
    ///         some_code();
    /// "#);
    /// let text = Text::from_md_lines(md.into_iter());
    /// ```
    pub fn from_md_lines<I>(md_lines: I) -> Self
    where
        I: Iterator<Item = &'s str>,
    {
        let mut lines = Vec::new();
        let mut between_fences = false;
        for md_line in md_lines {
            let parser = LineParser::from(md_line);
            let line = if between_fences {
                parser.as_code()
            } else {
                parser.line()
            };
            match line {
                Line::CodeFence(..) => {
                    between_fences = !between_fences;
                }
                _ => {
                    lines.push(line);
                }
            }
        }
        Text { lines }
    }
}

/// Tests of text parsing
#[cfg(test)]
mod tests {
    use crate::*;

    #[test]
    fn indented_code_between_fences() {
        let md = clean::lines(
            r#"
            outside
            ```code
            a
                b
            ```
        "#,
        );
        assert_eq!(
            Text::from_md_lines(md.into_iter()),
            Text {
                lines: vec![
                    Line::new_paragraph(vec![Compound::raw_str("outside")]),
                    Line::new_code(Compound::raw_str("a").code()),
                    Line::new_code(Compound::raw_str("    b").code()),
                ]
            },
        );
    }
}
