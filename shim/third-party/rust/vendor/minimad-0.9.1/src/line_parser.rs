use crate::*;

/// The structure parsing a line or part of a line.
/// A LineParser initialized from a markdown string exposes 2 main methods:
/// * `line` parses a line which is supposed to be part of a markdown text. This
///       method shouln't really be used externally: a text can be parsed in a whole
///       using `Text::from`
/// * `inline` parses a snippet which isn't supposed to be part of a markdown text.
///       Some types of lines aren't produced this ways as they don't make sense out of
///       a text: ListItem, TableRow, Code.
///
/// Normally not used directly but though `line::from(str)`
#[derive(Debug)]
pub struct LineParser<'s> {
    src: &'s str,
    idx: usize, // current index in string, in bytes
    bold: bool,
    italic: bool,
    code: bool,
    strikeout: bool,
}

impl<'s> LineParser<'s> {
    pub fn from(src: &'s str) -> LineParser<'_> {
        LineParser {
            src,
            idx: 0,
            bold: false,
            italic: false,
            code: false,
            strikeout: false,
        }
    }
    fn close_compound(&mut self, end: usize, tag_length: usize, compounds: &mut Vec<Compound<'s>>) {
        if end > self.idx {
            compounds.push(Compound::new(
                self.src,
                self.idx,
                end,
                self.bold,
                self.italic,
                self.code,
                self.strikeout,
            ));
        }
        self.idx = end + tag_length;
    }
    fn code_compound_from_idx(&self, idx: usize) -> Compound<'s> {
        Compound::new(self.src, idx, self.src.len(), false, false, true, false)
    }
    fn parse_compounds(&mut self, stop_on_pipe: bool) -> Vec<Compound<'s>> {
        let mut compounds = Vec::new();
        let mut after_first_star = false;
        let mut after_first_tilde = false;
        let mut after_antislash = false;

        // self.idx tracks byte indices, but str::char_indices returns an
        // iterator over chars, which may be wider than one byte. So we need
        // to skip not self.idx elements, but the number of chars that occur
        // before self.idx
        let chars_to_skip = self.src[..self.idx].chars().count();
        for (idx, char) in self.src.char_indices().skip(chars_to_skip) {
            if self.code {
                // only one thing matters: whether we're closing the inline code
                if char == '`' {
                    self.close_compound(idx, 1, &mut compounds);
                    self.code = false;
                }
                after_antislash = false;
                after_first_star = false;
                continue;
            }

            #[cfg(feature="escaping")]
            if after_antislash {
                after_antislash = false;
                match char {
                    '*' | '~' | '|' | '`' => {
                        self.close_compound(idx-1, 1, &mut compounds);
                        continue;
                    }
                    '\\' => {
                        self.close_compound(idx, 1, &mut compounds);
                        continue;
                    }
                    _ => {} // we don't escape at all normal chars
                }
            } else if char=='\\' {
                after_antislash = true;
                continue;
            }

            if after_first_star {
                match char {
                    '*' => {
                        // this is the second star
                        self.close_compound(idx - 1, 2, &mut compounds);
                        self.bold ^= true;
                    }
                    '~' => {
                        after_first_tilde = true;
                        self.close_compound(idx - 1, 2, &mut compounds);
                        // we don't know yet if it's one or two tildes
                        self.italic ^= true;
                    }
                    '|' if stop_on_pipe => {
                        self.close_compound(idx - 1, 1, &mut compounds);
                        return compounds;
                    }
                    '`' => {
                        self.close_compound(idx - 1, 2, &mut compounds);
                        self.italic ^= true;
                        self.code = true;
                    }
                    _ => {
                        // there was only one star
                        // Note that we don't handle a tag just after a star (except in code)
                        self.close_compound(idx - 1, 1, &mut compounds);
                        self.italic ^= true;
                    }
                }
                after_first_star = false;
            } else if after_first_tilde {
                match char {
                    '*' => {
                        after_first_star = true;
                        // we don't know yet if it's one or two stars
                    }
                    '~' => {
                        // this is the second tilde
                        self.close_compound(idx - 1, 2, &mut compounds);
                        self.strikeout ^= true;
                    }
                    '|' if stop_on_pipe => {
                        self.close_compound(idx - 1, 1, &mut compounds);
                        return compounds;
                    }
                    _ => {
                        // there was only one tilde, which means nothing
                    }
                }
                after_first_tilde = false;
            } else {
                match char {
                    '*' => {
                        after_first_star = true;
                        // we don't know yet if it's one or two stars
                    }
                    '~' => {
                        after_first_tilde = true;
                    }
                    '|' if stop_on_pipe => {
                        self.close_compound(idx, 0, &mut compounds);
                        return compounds;
                    }
                    '`' => {
                        self.close_compound(idx, 1, &mut compounds);
                        self.code = true;
                    }
                    _ => {}
                }
            }
        }
        let mut idx = self.src.len();
        if after_first_star && self.italic {
            idx -= 1;
        }
        if after_first_tilde && self.strikeout {
            idx -= 1;
        }
        self.close_compound(idx, 0, &mut compounds);
        compounds
    }
    fn parse_cells(&mut self) -> Vec<Composite<'s>> {
        let mut cells = Vec::new();
        while self.idx < self.src.len() {
            self.idx += 1;
            let style = if self.src[self.idx..].starts_with("* ") {
                self.idx += 2;
                CompositeStyle::ListItem
            } else if self.src[self.idx..].starts_with("> ") {
                self.idx += 2;
                CompositeStyle::Quote
            } else {
                CompositeStyle::Paragraph
            };
            self.bold = false;
            self.italic = false;
            self.code = false;
            self.strikeout = false;
            let compounds = self.parse_compounds(true);
            let mut composite = Composite { style, compounds };
            composite.trim_spaces();
            cells.push(composite);
        }
        if !cells.is_empty() && cells[cells.len() - 1].compounds.is_empty() {
            cells.pop();
        }
        cells
    }
    pub fn inline(mut self) -> Composite<'s> {
        Composite {
            style: CompositeStyle::Paragraph,
            compounds: self.parse_compounds(false),
        }
    }
    /// should be called when the line must be interpreted as a code part,
    /// for example between code fences
    pub fn as_code(mut self) -> Line<'s> {
        if self.src.starts_with("```") {
            self.idx = 3;
            Line::new_code_fence(self.parse_compounds(false))
        } else {
            Line::new_code(self.code_compound_from_idx(0))
        }
    }
    pub fn line(mut self) -> Line<'s> {
        if self.src.starts_with('|') {
            let tr = TableRow {
                cells: self.parse_cells(),
            };
            return match tr.as_table_alignments() {
                Some(aligns) => Line::TableRule(aligns),
                None => Line::TableRow(tr),
            };
        }
        if self.src.starts_with("    ") {
            return Line::new_code(self.code_compound_from_idx(4));
        }
        if self.src.starts_with('\t') {
            return Line::new_code(self.code_compound_from_idx(1));
        }
        if self.src.starts_with("* ") {
            self.idx = 2;
            return Line::new_list_item(self.parse_compounds(false));
        }
        if self.src == ">" {
            return Line::new_quote(Vec::new());
        }
        if self.src.starts_with("> ") {
            self.idx = 2;
            return Line::new_quote(self.parse_compounds(false));
        }
        if self.src.starts_with("```") {
            self.idx = 3;
            return Line::new_code_fence(self.parse_compounds(false));
        }
        let header_level = header_level(self.src);
        if header_level > 0 {
            self.idx = header_level + 1;
            return Line::new_header(header_level as u8, self.parse_compounds(false));
        }
        let compounds = self.parse_compounds(false);
        if compounds_are_rule(&compounds) {
            Line::HorizontalRule
        } else {
            Line::new_paragraph(compounds)
        }
    }
}

const DASH: u8 = 45;

fn compounds_are_rule(compounds: &[Compound<'_>]) -> bool {
    if compounds.len() != 1 {
        return false;
    }
    let s = compounds[0].as_str();
    if s.len() < 3 {
        return false;
    }
    for c in s.as_bytes() {
        if *c != DASH {
            return false;
        }
    }
    true
}

/// Tests of line parsing
#[cfg(test)]
mod tests {
    use crate::*;

    #[test]
    fn simple_line_parsing() {
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
    }

    #[test]
    fn nested_styles_parsing() {
        assert_eq!(
            Line::from("*Italic then **bold and italic `and some *code*`** and italic*"),
            Line::new_paragraph(vec![
                Compound::raw_str("Italic then ").italic(),
                Compound::raw_str("bold and italic ").bold().italic(),
                Compound::raw_str("and some *code*").bold().italic().code(),
                Compound::raw_str(" and italic").italic(),
            ])
        );
    }

    #[test]
    fn quote() {
        assert_eq!(
            Line::from("> Veni, vidi, *vici*!"),
            Line::new_quote(vec![
                Compound::raw_str("Veni, vidi, "),
                Compound::raw_str("vici").italic(),
                Compound::raw_str("!"),
            ])
        );
    }

    #[test]
    fn code_after_italic() {
        assert_eq!(
            Line::from("*name=*`code`"),
            Line::new_paragraph(vec![
                Compound::raw_str("name=").italic(),
                Compound::raw_str("code").code(),
            ])
        );
    }

    #[test]
    /// this test is borderline. It wouldn't be very problematic to not support this case.
    /// A regression would thus be acceptable here (but I want it to be noticed)
    fn single_star() {
        assert_eq!(
            Line::from("*"),
            Line::new_paragraph(vec![Compound::raw_str("*"),])
        );
    }

    #[test]
    /// this test is borderline. It wouldn't be very problematic to not support it.
    /// A regression would thus be acceptable here (but I want it to be noticed)
    fn single_tilde() {
        assert_eq!(
            Line::from("~"),
            Line::new_paragraph(vec![Compound::raw_str("~"),])
        );
    }

    #[test]
    fn striked_after_italic() {
        assert_eq!(
            Line::from("*italic*~~striked~~"),
            Line::new_paragraph(vec![
                Compound::raw_str("italic").italic(),
                Compound::raw_str("striked").strikeout(),
            ])
        );
    }

    #[test]
    fn tight_sequence() {
        assert_eq!(
            Line::from(
                "*italic*`code`**bold**`code`*italic**italic+bold***`code`*I*~~striked~~*I*"
            ),
            Line::new_paragraph(vec![
                Compound::raw_str("italic").italic(),
                Compound::raw_str("code").code(),
                Compound::raw_str("bold").bold(),
                Compound::raw_str("code").code(),
                Compound::raw_str("italic").italic(),
                Compound::raw_str("italic+bold").italic().bold(),
                Compound::raw_str("code").code(),
                Compound::raw_str("I").italic(),
                Compound::raw_str("striked").strikeout(),
                Compound::raw_str("I").italic(),
            ])
        );
    }

    #[cfg(feature="escaping")]
    #[test]
    fn escapes() {
        assert_eq!(
            Line::from(
                "no \\*italic\\* here"
            ),
            Line::new_paragraph(vec![
                Compound::raw_str("no "),
                Compound::raw_str("*italic"),
                Compound::raw_str("* here"),
            ])
        );
        // check we're not removing chars with the escaping, and that
        // we're not losing the '\' when it's not escaping something
        // (only markdown modifiers can be escaped)
        assert_eq!(
            Line::from(
                "a\\bc\\"
            ),
            Line::new_paragraph(vec![
                Compound::raw_str("a\\bc\\"),
            ])
        );
        assert_eq!(
            Line::from(
                "*italic\\*and\\*still\\*italic*"
            ),
            Line::new_paragraph(vec![
                Compound::raw_str("italic").italic(),
                Compound::raw_str("*and").italic(),
                Compound::raw_str("*still").italic(),
                Compound::raw_str("*italic").italic(),
            ])
        );
        assert_eq!(
            Line::from("\\**Italic then **bold\\\\ and \\`italic `and some *code*`** and italic*\\*"),
            Line::new_paragraph(vec![
                Compound::raw_str("*"),
                Compound::raw_str("Italic then ").italic(),
                Compound::raw_str("bold\\").bold().italic(),
                Compound::raw_str(" and ").bold().italic(),
                Compound::raw_str("`italic ").bold().italic(),
                Compound::raw_str("and some *code*").bold().italic().code(),
                Compound::raw_str(" and italic*").italic(),
            ])
        );
    }

    #[test]
    fn code_fence() {
        assert_eq!(Line::from("```"), Line::new_code_fence(vec![]),);
        assert_eq!(
            Line::from("```rust"),
            Line::new_code_fence(vec![Compound::raw_str("rust"),]),
        );
    }

    #[test]
    fn line_of_code() {
        assert_eq!(
            Line::from("    let r = Math.sin(π/2) * 7"),
            Line::new_code(Compound::raw_str("let r = Math.sin(π/2) * 7").code(),)
        );
    }

    #[test]
    fn standard_header() {
        assert_eq!(
            Line::from("### just a title"),
            Line::new_header(3, vec![Compound::raw_str("just a title"),])
        );
    }

    #[test]
    fn list_item() {
        assert_eq!(
            Line::from("* *list* item"),
            Line::new_list_item(vec![
                Compound::raw_str("list").italic(),
                Compound::raw_str(" item"),
            ])
        );
    }

    #[test]
    fn horizontal_rule() {
        assert_eq!(Line::from("----------"), Line::HorizontalRule,);
    }

    #[test]
    fn styled_header() {
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
    }

    #[test]
    fn table_row() {
        assert_eq!(
            Line::from("| bla |*italic*|hi!|> some quote"),
            Line::new_table_row(vec![
                Composite {
                    style: CompositeStyle::Paragraph,
                    compounds: vec![Compound::raw_str("bla"),],
                },
                Composite {
                    style: CompositeStyle::Paragraph,
                    compounds: vec![Compound::raw_str("italic").italic(),],
                },
                Composite {
                    style: CompositeStyle::Paragraph,
                    compounds: vec![Compound::raw_str("hi!"),],
                },
                Composite {
                    style: CompositeStyle::Quote,
                    compounds: vec![Compound::raw_str("some quote"),],
                }
            ])
        );
    }

    #[test]
    fn table_row_issue_4() {
        assert_eq!(
            Line::from("| 安 | 安 | 安 |"),
            Line::new_table_row(vec![
                Composite {
                    style: CompositeStyle::Paragraph,
                    compounds: vec![Compound::raw_str("安"),],
                },
                Composite {
                    style: CompositeStyle::Paragraph,
                    compounds: vec![Compound::raw_str("安"),],
                },
                Composite {
                    style: CompositeStyle::Paragraph,
                    compounds: vec![Compound::raw_str("安"),],
                },
            ])
        );
    }

    #[test]
    fn table_alignments() {
        assert_eq!(
            Line::from("|-----|:--|:-:|----:"),
            Line::new_table_alignments(vec![
                Alignment::Unspecified,
                Alignment::Left,
                Alignment::Center,
                Alignment::Right,
            ])
        );
    }
}
