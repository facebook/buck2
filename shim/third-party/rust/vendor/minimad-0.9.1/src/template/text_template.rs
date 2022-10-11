use crate::*;

#[derive(Debug, Default)]
struct SubTemplate<'s> {
    start_line_idx: usize,
    line_count: usize,
    name: &'s str,
}

#[derive(Debug, Default, PartialEq, Eq)]
struct CompoundArg<'s> {
    name: &'s str,
    line_idx: usize,      // index in the line in the template's text
    composite_idx: usize, // for when the line is multi-composite (ie it's a tablerow)
    compound_idx: usize,  // index of the compound in the line's composite
}

/// a markdown template allowing you to replace some placeholders with
/// given values, or to expand some sub-templates with repetitions
/// (useful with lists, table rows, etc.)
#[derive(Debug)]
pub struct TextTemplate<'s> {
    pub text: Text<'s>,
    compound_args: Vec<CompoundArg<'s>>, // replacements of compounds
    sub_templates: Vec<SubTemplate<'s>>,
}

#[derive(Debug)]
enum SubTemplateToken<'s> {
    None,
    Start(&'s str),
    End,
}

#[derive(Debug, Clone)]
struct Replacement<'s, 'b> {
    name: &'b str,
    value: &'s str,
}

/// an expander for a sub template. You get it using the `sub` method
/// of the text expander
#[derive(Debug)]
pub struct SubTemplateExpander<'s, 'b> {
    name: &'b str,
    raw_replacements: Vec<Replacement<'s, 'b>>, // replacements which are done as non interpreted compound content
    md_replacements: Vec<Replacement<'s, 'b>>,
}

/// an expander you get from a template. You specify replacements
/// on the expander then you ask it the text using `expand`
pub struct TextTemplateExpander<'s, 'b> {
    template: &'b TextTemplate<'s>,
    text: Text<'s>,
    sub_expansions: Vec<SubTemplateExpander<'s, 'b>>,
    md_replacements: Vec<Replacement<'s, 'b>>,
    lines_to_add: Vec<Vec<Line<'s>>>,
    lines_to_exclude: Vec<bool>, // true when the line must not be copied into the final text
}

//-------------------------------------------------------------------
//                 Template parsing
//-------------------------------------------------------------------

fn is_valid_name_char(c: char) -> bool {
    c.is_ascii_lowercase() || c.is_digit(10) || c == '_' || c == '-'
}

fn read_sub_template_token(md_line: &str) -> SubTemplateToken<'_> {
    let mut chars = md_line.chars();
    match (chars.next(), chars.next()) {
        (Some('$'), Some('{')) => {
            // "${" : maybe a sub-template opening
            let name = &md_line[2..];
            if !name.is_empty() && name.chars().all(is_valid_name_char) {
                SubTemplateToken::Start(name)
            } else {
                SubTemplateToken::None
            }
        }
        (Some('}'), None) => SubTemplateToken::End,
        _ => SubTemplateToken::None,
    }
}

/// find the `${some-name}` arguments in the composite, and add them
/// to args.
fn find_args<'s>(
    composite: &mut Composite<'s>,
    args: &mut Vec<CompoundArg<'s>>,
    line_idx: usize,
    composite_idx: usize,
) {
    let mut compounds = Vec::new();
    for compound in &composite.compounds {
        let mut start = 0;
        let mut iter = compound.as_str().char_indices();
        while let Some((_, c)) = iter.next() {
            if c == '$' {
                if let Some((bridx, c)) = iter.next() {
                    if c == '{' {
                        for (idx, c) in &mut iter {
                            if c == '}' {
                                if idx - bridx > 1 {
                                    if start + 1 < bridx {
                                        compounds.push(compound.sub(start, bridx - 1));
                                    }
                                    args.push(CompoundArg {
                                        name: &compound.as_str()[bridx + 1..idx],
                                        line_idx,
                                        composite_idx,
                                        compound_idx: compounds.len(),
                                    });
                                    compounds.push(compound.sub(bridx - 1, idx + 1)); // placeholder
                                    start = idx + 1;
                                }
                                break;
                            } else if !is_valid_name_char(c) {
                                break;
                            }
                        }
                    }
                }
            }
        }
        let tail = compound.tail(start);
        if !tail.is_empty() {
            compounds.push(tail);
        }
    }
    composite.compounds = compounds;
}

impl<'s> From<&'s str> for TextTemplate<'s> {
    /// build a template from a markdown text with placeholders like ${some-name}
    /// and sub-templates
    fn from(md: &'s str) -> TextTemplate<'s> {
        let mut text = Text { lines: Vec::new() };
        let mut compound_args = Vec::new();
        let mut sub_templates = Vec::new();
        let mut current_sub_template: Option<SubTemplate<'_>> = None;
        let mut between_fences = false;
        for md_line in clean::lines(md) {
            match read_sub_template_token(md_line) {
                SubTemplateToken::Start(name) => {
                    current_sub_template = Some(SubTemplate {
                        start_line_idx: text.lines.len(),
                        line_count: 0,
                        name,
                    });
                    continue; // so to not add the sub-tmpl opening to the text
                }
                SubTemplateToken::End => {
                    if current_sub_template.is_some() {
                        let mut sub_template = current_sub_template.take().unwrap();
                        sub_template.line_count = text.lines.len() - sub_template.start_line_idx;
                        sub_templates.push(sub_template);
                        continue; // so to not add the sub-tmpl closing to the text
                    } else {
                        // we'll assume this `}` isn't part of any templating
                    }
                }
                SubTemplateToken::None => {}
            }
            let line_idx = text.lines.len();
            let parser = LineParser::from(md_line);
            let mut line = if between_fences {
                parser.as_code()
            } else {
                parser.line()
            };
            match &mut line {
                Line::Normal(ref mut composite) => {
                    find_args(composite, &mut compound_args, line_idx, 0);
                    text.lines.push(line);
                }
                Line::TableRow(ref mut table_row) => {
                    for (composite_idx, composite) in table_row.cells.iter_mut().enumerate() {
                        find_args(composite, &mut compound_args, line_idx, composite_idx);
                    }
                    text.lines.push(line);
                }
                Line::CodeFence(..) => {
                    between_fences = !between_fences;
                }
                _ => {
                    text.lines.push(line);
                }
            };
        }
        TextTemplate {
            text,
            compound_args,
            sub_templates,
        }
    }
}

impl<'s> TextTemplate<'s> {
    /// return a new expander for the template
    pub fn expander<'b>(&'b self) -> TextTemplateExpander<'s, 'b> {
        TextTemplateExpander::from(self)
    }

    /// if the line `line_idx` is part of a template, return this
    /// template's index. Return None if it's not part of a template.
    ///
    /// This might be optimized by an internal vec in the future (or
    /// by just having another structure than a Text as internal md
    /// storage)
    fn get_sub_of_line(&self, line_idx: usize) -> Option<usize> {
        for (sub_idx, sub_template) in self.sub_templates.iter().enumerate() {
            if line_idx >= sub_template.start_line_idx
                && line_idx < sub_template.start_line_idx + sub_template.line_count
            {
                return Some(sub_idx);
            }
        }
        None
    }
}

//-------------------------------------------------------------------
//                          Expansion
//-------------------------------------------------------------------

impl<'s, 'b> From<&'b TextTemplate<'s>> for TextTemplateExpander<'s, 'b> {
    /// Build a new expander for the template. The expander stores the additions
    /// done with `set`, `set_md`, `set_lines` or in the `sub` expanders.
    fn from(template: &'b TextTemplate<'s>) -> Self {
        // line insertion (from subtemplates and from set_lines) as well
        // as line removals are postponed until final text building so
        // that line indexes stay valid until that point). We just note
        // what lines to add to or exclude from the final text.
        let line_count = template.text.lines.len();
        let lines_to_add = vec![Vec::new(); line_count];
        let lines_to_exclude = vec![false; line_count];
        Self {
            template,
            text: template.text.clone(),
            sub_expansions: Vec::new(),
            md_replacements: Vec::new(),
            lines_to_add,
            lines_to_exclude,
        }
    }
}

impl<'s, 'b> SubTemplateExpander<'s, 'b> {
    /// replace placeholders with name `name` with the given value, not interpreted as markdown
    pub fn set(&mut self, name: &'b str, value: &'s str) -> &mut SubTemplateExpander<'s, 'b> {
        self.raw_replacements.push(Replacement { name, value });
        self
    }
    /// replace placeholder with name `name` with the given value, interpreted as markdown
    pub fn set_md(&mut self, name: &'b str, value: &'s str) -> &mut SubTemplateExpander<'s, 'b> {
        self.md_replacements.push(Replacement { name, value });
        self
    }
}

fn set_in_line<'s>(line: &mut Line<'s>, compound_arg: &CompoundArg<'s>, value: &'s str) {
    match line {
        Line::Normal(composite) => {
            composite.compounds[compound_arg.compound_idx].set_str(value);
        }
        Line::TableRow(table_row) => {
            table_row.cells[compound_arg.composite_idx].compounds[compound_arg.compound_idx]
                .set_str(value);
        }
        _ => {}
    }
}

fn set_in_text<'s>(
    template: &TextTemplate<'s>,
    text: &mut Text<'s>,
    line_offset: usize,
    name: Option<&str>,
    value: &'s str,
) {
    for compound_arg in &template.compound_args {
        if name.is_none () || name == Some(compound_arg.name) {
            let idx = compound_arg.line_idx;
            if idx < line_offset || idx - line_offset >= text.lines.len() {
                continue; // can happen if a replacement name is present in the outside text
            }
            set_in_line(&mut text.lines[idx - line_offset], compound_arg, value);
        }
    }
}

fn set_all_md_in_text<'s, 'b>(
    template: &TextTemplate<'s>,
    text: &mut Text<'s>,
    line_offset: usize,
    md_replacements: &[Replacement<'s, 'b>],
) {
    if md_replacements.is_empty() {
        return; // no need to iterate over all compound_args
    }
    for compound_arg in template.compound_args.iter().rev() {
        let idx = compound_arg.line_idx;
        if idx < line_offset || idx - line_offset >= text.lines.len() {
            continue;
        }
        for md_repl in md_replacements {
            if md_repl.name == compound_arg.name {
                let replacing_composite = Composite::from_inline(md_repl.value);
                // we replace the compound with the ones of the parsed value
                let patched_line = &mut text.lines[idx - line_offset];
                match patched_line {
                    Line::Normal(ref mut composite) => {
                        replace_compound(
                            composite,
                            compound_arg.compound_idx,
                            replacing_composite.compounds,
                        );
                    }
                    Line::TableRow(ref mut table_row) => {
                        replace_compound(
                            &mut table_row.cells[compound_arg.composite_idx],
                            compound_arg.compound_idx,
                            replacing_composite.compounds,
                        );
                    }
                    _ => {}
                }
                break; // it's not possible to apply two replacements to the compound
            }
        }
    }
}

/// replace a compound with several other ones.
/// Do nothing if the passed compounds vec is empty.
fn replace_compound<'s>(
    composite: &mut Composite<'s>, // composite in which to do the replacement
    mut compound_idx: usize,       // index in the composite of the compound to remove
    mut replacing_compounds: Vec<Compound<'s>>, // the compounds taking the place of the removed one
) {
    let mut replacing_compounds = replacing_compounds.drain(..);
    if let Some(compound) = replacing_compounds.next() {
        composite.compounds[compound_idx] = compound;
        for compound in replacing_compounds {
            compound_idx += 1;
            composite.compounds.insert(compound_idx, compound);
        }
    }
}

impl<'s, 'b> TextTemplateExpander<'s, 'b> {
    /// replace placeholders with name `name` with the given value, non interpreted
    /// (i.e. stars, backquotes, etc. don't mess the styling defined by the template)
    pub fn set(&mut self, name: &str, value: &'s str) -> &mut TextTemplateExpander<'s, 'b> {
        set_in_text(self.template, &mut self.text, 0, Some(name), value);
        self
    }

    /// replace all placeholders with the given value, non interpreted
    /// (i.e. stars, backquotes, etc. don't mess the styling defined by the template).
    /// This can be used at start to have a "default" value.
    pub fn set_all(&mut self, value: &'s str) -> &mut TextTemplateExpander<'s, 'b> {
        set_in_text(self.template, &mut self.text, 0, None, value);
        self
    }

    /// replace placeholders with name `name` with the given value, interpreted as markdown
    pub fn set_md(&mut self, name: &'b str, value: &'s str) -> &mut TextTemplateExpander<'s, 'b> {
        self.md_replacements.push(Replacement { name, value });
        self
    }

    /// replace a placeholder with several lines.
    /// This is mostly useful when the placeholder is a repeatable line (code, list item)
    pub fn set_lines(
        &mut self,
        name: &'b str,
        raw_lines: &'s str,
    ) -> &mut TextTemplateExpander<'s, 'b> {
        for compound_arg in &self.template.compound_args {
            if compound_arg.name == name {
                // the line holding the compound is now considered a template, it's removed
                self.lines_to_exclude[compound_arg.line_idx] = true;
                for value in clean::lines(raw_lines) {
                    let mut line = self.text.lines[compound_arg.line_idx].clone();
                    set_in_line(&mut line, compound_arg, value);
                    self.lines_to_add[compound_arg.line_idx].push(line);
                }
            }
        }
        self
    }

    /// replace a placeholder with several lines interpreted as markdown
    pub fn set_lines_md(
        &mut self,
        name: &'b str,
        md: &'s str,
    ) -> &mut TextTemplateExpander<'s, 'b> {
        for compound_arg in &self.template.compound_args {
            if compound_arg.name == name {
                // the line holding the compound is now considered a template, it's removed
                self.lines_to_exclude[compound_arg.line_idx] = true;
                for line in md.lines().map(|md| LineParser::from(md).line()) {
                    self.lines_to_add[compound_arg.line_idx].push(line);
                }
            }
        }
        self
    }

    /// prepare expansion of a sub template and return a mutable reference to the
    ///  object in which to set compound replacements
    pub fn sub(&mut self, name: &'b str) -> &mut SubTemplateExpander<'s, 'b> {
        let sub = SubTemplateExpander {
            name,
            raw_replacements: Vec::new(),
            md_replacements: Vec::new(),
        };
        let idx = self.sub_expansions.len();
        self.sub_expansions.push(sub);
        &mut self.sub_expansions[idx]
    }

    /// build a text by applying the replacements to the initial template
    pub fn expand(mut self) -> Text<'s> {
        // The simple replacements defined with expander.set(name, value) have
        // already be done at this point.

        // We triage the md_replacements: we can directly apply the ones which
        // are not applied to sub templates and we must defer the other ones
        // to the sub templates expansion phase
        let mut defered_repls: Vec<Vec<Replacement<'_, '_>>> =
            vec![Vec::new(); self.template.sub_templates.len()];
        for compound_arg in self.template.compound_args.iter().rev() {
            let line_idx = compound_arg.line_idx;
            let sub_idx = self.template.get_sub_of_line(line_idx);
            for md_repl in &self.md_replacements {
                if md_repl.name == compound_arg.name {
                    if let Some(sub_idx) = sub_idx {
                        // we must clone because a repl can be applied several times
                        defered_repls[sub_idx].push(md_repl.clone());
                    } else {
                        let replacing_composite = Composite::from_inline(md_repl.value);
                        // we replace the compound with the ones of the parsed value
                        let patched_line = &mut self.text.lines[line_idx];
                        match patched_line {
                            Line::Normal(ref mut composite) => {
                                replace_compound(
                                    composite,
                                    compound_arg.compound_idx,
                                    replacing_composite.compounds,
                                );
                            }
                            Line::TableRow(ref mut table_row) => {
                                replace_compound(
                                    &mut table_row.cells[compound_arg.composite_idx],
                                    compound_arg.compound_idx,
                                    replacing_composite.compounds,
                                );
                            }
                            _ => {}
                        }
                        break; // it's not possible to apply two replacements to the compound
                    }
                }
            }
        }

        for (sub_idx, sub_template) in self.template.sub_templates.iter().enumerate() {
            let start = sub_template.start_line_idx;
            let end = start + sub_template.line_count;
            // we remove the lines of the subtemplate from the main text
            for idx in start..end {
                self.lines_to_exclude[idx] = true;
            }
            for sub_expansion in self.sub_expansions.iter() {
                if sub_expansion.name != sub_template.name {
                    continue;
                }
                let mut sub_text = Text::default();
                for line in &self.text.lines[start..end] {
                    sub_text.lines.push(line.clone());
                }
                for repl in &sub_expansion.raw_replacements {
                    set_in_text(
                        self.template,
                        &mut sub_text,
                        sub_template.start_line_idx,
                        Some(repl.name),
                        repl.value,
                    );
                }
                let mut md_replacements = sub_expansion.md_replacements.clone();
                md_replacements.extend(defered_repls[sub_idx].clone());
                set_all_md_in_text(
                    self.template,
                    &mut sub_text,
                    sub_template.start_line_idx,
                    &md_replacements,
                );
                for line in sub_text.lines.drain(..) {
                    self.lines_to_add[sub_template.start_line_idx].push(line);
                }
            }
        }

        // we now do the removals and insertions we deffered until then.
        let mut lines = Vec::new();
        for (idx, line) in self.text.lines.drain(..).enumerate() {
            if !self.lines_to_exclude[idx] {
                lines.push(line);
            }
            lines.append(&mut self.lines_to_add[idx]);
        }
        Text { lines }
    }
}

