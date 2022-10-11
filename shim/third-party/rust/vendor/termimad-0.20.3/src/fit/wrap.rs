use {
    crate::*,
    minimad::*,
    unicode_width::UnicodeWidthStr,
};

/// build a composite which can be a new line after wrapping.
const fn follow_up_composite<'s>(fc: &FmtComposite<'s>) -> FmtComposite<'s> {
    let style = match fc.composite.style {
        minimad::CompositeStyle::ListItem => CompositeStyle::Paragraph,
        _ => fc.composite.style,
    };
    let visible_length = match style {
        CompositeStyle::Quote => 2,
        _ => 0,
    };
    FmtComposite {
        composite: Composite {
            style,
            compounds: Vec::new(),
        },
        visible_length,
        spacing: fc.spacing,
    }
}

/// return the inherent widths related to the style, the one of the first line (for
/// example with a bullet) and the ones for the next lines (for example with quotes)
pub const fn composite_style_widths(composite_style: CompositeStyle) -> (usize, usize) {
    match composite_style {
        CompositeStyle::Paragraph => (0, 0),
        CompositeStyle::Header(_) => (0, 0),
        CompositeStyle::ListItem => (2, 0),
        CompositeStyle::Code => (0, 0),
        CompositeStyle::Quote => (2, 2),
    }
}

/// cut the passed composite in several composites fitting the given *visible* width
/// (which might be bigger or smaller than the length of the underlying string).
/// width can't be less than 3.
pub fn hard_wrap_composite<'s, 'c>(
    src_composite: &'c FmtComposite<'s>,
    width: usize,
) -> Result<Vec<FmtComposite<'s>>, InsufficientWidthError> {
    if width < 3 {
        return Err(InsufficientWidthError{ available_width: width });
    }
    debug_assert!(src_composite.visible_length > width); // or we shouldn't be called
    let mut composites: Vec<FmtComposite<'s>> = Vec::new();
    let (first_width, _other_widths) = composite_style_widths(src_composite.composite.style);
    let mut dst_composite = FmtComposite {
        composite: Composite {
            style: src_composite.composite.style,
            compounds: Vec::new(),
        },
        visible_length: first_width,
        spacing: src_composite.spacing,
    };

    // Strategy 1:
    // we try to optimize for a quite frequent case: two parts with nothing or just space in
    // between
    let compounds = &src_composite.composite.compounds;
    if
        ( // clean cut of 2
            compounds.len() == 2
            && compounds[0].src.width() + first_width <= width
            && compounds[1].src.width() + _other_widths <= width
        )
        ||
        ( // clean cut of 3
            compounds.len() == 3
            && compounds[0].src.width() + first_width <= width
            && compounds[2].src.width() + _other_widths <= width
            && compounds[1].src.chars().all(char::is_whitespace)
        )
    {
        dst_composite.add_compound(compounds[0].clone());
        let mut new_dst_composite = follow_up_composite(&dst_composite);
        composites.push(dst_composite);
        new_dst_composite.add_compound(compounds[compounds.len()-1].clone());
        composites.push(new_dst_composite);
        return Ok(composites);
    }

    let mut tokens = tokenize(&src_composite.composite, width - first_width);
    // Strategy 2:
    // we try to cut along tokens, using spaces to break
    for token in tokens.drain(..) {
        if dst_composite.visible_length + token.width > width {
            if !token.blank { // we skip blank composite at line change
                let mut repl_composite = follow_up_composite(&dst_composite);
                std::mem::swap(&mut dst_composite, &mut repl_composite);
                composites.push(repl_composite);
                dst_composite.add_compound(token.to_compound());
            }
        } else {
            dst_composite.add_compound(token.to_compound());
        }
    }
    composites.push(dst_composite);
    Ok(composites)
}

/// hard_wrap all normal lines to ensure the text fits the width.
/// Doesn't touch table rows.
/// Consumes the passed array and return a new one (may contain
/// the original lines, avoiding cloning when possible).
/// Return an error if the width is less than 3.
pub fn hard_wrap_lines(
    src_lines: Vec<FmtLine<'_>>,
    width: usize,
) -> Result<Vec<FmtLine<'_>>, InsufficientWidthError> {
    let mut src_lines = src_lines;
    let mut lines = Vec::new();
    for src_line in src_lines.drain(..) {
        if let FmtLine::Normal(fc) = src_line {
            if fc.visible_length <= width {
                lines.push(FmtLine::Normal(fc));
            } else {
                for fc in hard_wrap_composite(&fc, width)? {
                    lines.push(FmtLine::Normal(fc));
                }
            }
        } else {
            lines.push(src_line);
        }
    }
    Ok(lines)
}

/// Tests of hard wrapping
///
/// The print which happens in case of failure isn't really well
/// formatted. A solution if a test fails is to do
///      cargo test -- --nocapture
#[cfg(test)]
mod wrap_tests {

    use {
        crate::{
            displayable_line::DisplayableLine,
            skin::MadSkin,
            fit::wrap::*,
        },
    };

    fn visible_fmt_line_length(skin: &MadSkin, line: &FmtLine<'_>) -> usize {
        match line {
            FmtLine::Normal(fc) => skin.visible_composite_length(&fc.composite),
            _ => 0, // FIXME implement
        }
    }

    /// check that after hard wrap, no line is longer
    ///  that required
    /// check also that no line is empty (the source text
    ///  is assumed to have no empty line)
    fn check_no_overflow(skin: &MadSkin, src: &str, width: usize) {
        let text = skin.text(src, Some(width));
        println!("------- test wrapping with width: {}", width);
        for line in &text.lines {
            let len = visible_fmt_line_length(skin, &line);
            println!(
                "len:{: >4}  | {}",
                len,
                DisplayableLine {
                    skin: &skin,
                    line,
                    width: None,
                }
            );
            assert!(len <= width);
            assert!(len > 0);
        }
    }

    /// check line lenghts are what is expected
    fn check_line_lengths(skin: &MadSkin, src: &str, width: usize, lenghts: Vec<usize>) {
        println!("====\ninput text:\n{}", &src);
        let text = skin.text(src, Some(width));
        assert_eq!(text.lines.len(), lenghts.len(), "same number of lines");
        println!("====\nwrapped text:\n{}", &text);
        for i in 0..lenghts.len() {
            assert_eq!(
                visible_fmt_line_length(skin, &text.lines[i]),
                lenghts[i],
                "expected length for line {} when wrapping at {}",
                i,
                width
            );
        }
    }

    /// check many wrappings of a 4 lines text with 2 list items and
    /// some style
    #[test]
    fn check_hard_wrapping_simple_text() {
        let skin = crate::get_default_skin();
        // build a text and check it
        let src = "This is a *long* line which needs to be **broken**.\n\
                   And the text goes on with a list:\n\
                   * short item\n\
                   * a *somewhat longer item* (with a part in **bold**)";
        for width in 3..50 {
            check_no_overflow(skin, &src, width);
        }
        check_line_lengths(skin, &src, 25, vec![25, 19, 25, 7, 12, 25, 21]);
    }

    #[test]
    fn check_space_removing() {
        let skin = crate::get_default_skin();
        let src = FmtComposite::from(Composite::from_inline("syntax coloring"), &skin);
        println!("input:\n{:?}", &src);
        let wrapped = hard_wrap_composite(&src, 8).unwrap();
        println!("wrapped: {:?}", &wrapped);
        assert_eq!(wrapped.len(), 2);
    }

    fn first_compound(line: FmtLine) -> Option<Compound> {
        match line {
            FmtLine::Normal(mut fc) => fc.composite.compounds.drain(..).next(),
            _ => None,
        }
    }

    #[test]
    /// check the case of a wrapping occuring after a space and at the start of a compound
    /// see https://github.com/Canop/termimad/issues/17
    fn check_issue_17() {
        let skin = crate::get_default_skin();
        let src = "*Now I'll describe this example with more words than necessary, in order to be sure to demonstrate scrolling (and **wrapping**, too, thanks to long sentences).*";
        let text = skin.text(src, Some(120));
        assert_eq!(text.lines.len(), 2);
        assert_eq!(
            first_compound(text.lines.into_iter().nth(1).unwrap()),
            Some(Compound::raw_str("wrapping").bold().italic()),
        );
    }

    #[test]
    /// check that we're not wrapping outside of char boudaries
    fn check_issue_23() {
        let md: &str = "ZA\u{360}\u{321}\u{34a}\u{35d}LGΌ IS\u{36e}\u{302}\u{489}\u{32f}\u{348}\u{355}\u{339}\u{318}\u{331} T</b>O\u{345}\u{347}\u{339}\u{33a}Ɲ\u{334}ȳ\u{333} TH\u{318}<b>E\u{344}\u{309}\u{356} \u{360}P\u{32f}\u{34d}\u{32d}O\u{31a}\u{200b}N\u{310}Y\u{321} H\u{368}\u{34a}\u{33d}\u{305}\u{33e}\u{30e}\u{321}\u{338}\u{32a}\u{32f}E\u{33e}\u{35b}\u{36a}\u{344}\u{300}\u{301}\u{327}\u{358}\u{32c}\u{329} \u{367}\u{33e}\u{36c}\u{327}\u{336}\u{328}\u{331}\u{339}\u{32d}\u{32f}C\u{36d}\u{30f}\u{365}\u{36e}\u{35f}\u{337}\u{319}\u{332}\u{31d}\u{356}O\u{36e}\u{34f}\u{32e}\u{32a}\u{31d}\u{34d}";
        let skin = MadSkin::default();
        for w in 40..60 {
            println!("wrapping on width {}", w);
            let _text = FmtText::from(&skin, md, Some(w));
        }
    }
}
