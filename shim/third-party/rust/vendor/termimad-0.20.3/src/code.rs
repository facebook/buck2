use {
    crate::{
        line::*,
        spacing::Spacing,
    },
    minimad::{Alignment, CompositeStyle},
};

/// a sequence of lines whose line-style is Code
#[derive(Debug)]
pub struct CodeBlock {
    pub start: usize,
    pub height: usize, // number of lines
    pub width: usize,  // length in chars of the widest line
}
impl CodeBlock {
    /// ensure all lines of the block have the same width
    pub fn justify(&self, lines: &mut [FmtLine<'_>]) {
        for line in lines.iter_mut().skip(self.start).take(self.height) {
            if let FmtLine::Normal(ref mut fc) = line {
                fc.spacing = Some(Spacing {
                    width: self.width,
                    align: Alignment::Left,
                });
            }
        }
    }
}

const fn code_line_length(line: &FmtLine<'_>) -> Option<usize> {
    match line {
        FmtLine::Normal(fc) => match fc.composite.style {
            CompositeStyle::Code => Some(fc.visible_length),
            _ => None,
        },
        _ => None,
    }
}

/// find ranges of code lines in a text.
///
/// Warning: the indices in a codeblock are invalid as
/// soon as lines are inserted or removed. This function
/// should normally not be used from another module or lib
pub fn find_blocks(lines: &[FmtLine<'_>]) -> Vec<CodeBlock> {
    let mut blocks: Vec<CodeBlock> = Vec::new();
    let mut current: Option<CodeBlock> = None;
    for (idx, line) in lines.iter().enumerate() {
        if let Some(ll) = code_line_length(line) {
            match current.as_mut() {
                Some(b) => {
                    b.height += 1;
                    b.width = b.width.max(ll);
                }
                None => {
                    current = Some(CodeBlock {
                        start: idx,
                        height: 1,
                        width: ll,
                    });
                }
            }
        } else if let Some(c) = current.take() {
            blocks.push(c);
        }
    }
    if let Some(c) = current.take() {
        blocks.push(c);
    }
    blocks
}

/// ensure the widths of all lines in a code block are
/// the same line.
pub fn justify_blocks(lines: &mut [FmtLine<'_>]) {
    let blocks = find_blocks(lines);
    for b in blocks {
        b.justify(lines);
    }
}
