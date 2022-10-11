use minimad::{Line, TableRule};

use crate::composite::FmtComposite;
use crate::skin::MadSkin;
use crate::tbl::{FmtTableRow, FmtTableRule, RelativePosition};

/// A line in a text. This structure should normally not be
/// used outside of the lib.
#[derive(Debug)]
pub enum FmtLine<'s> {
    Normal(FmtComposite<'s>),
    TableRow(FmtTableRow<'s>),
    TableRule(FmtTableRule),
    HorizontalRule,
}

impl<'s> FmtLine<'s> {
    /// Build a fmtline from a minimad line.
    /// Skin is passed because it might affect the visible size
    /// in the future
    pub fn from(mline: Line<'s>, skin: &MadSkin) -> Self {
        match mline {
            Line::Normal(composite) => FmtLine::Normal(FmtComposite::from(composite, skin)),
            Line::TableRow(table_row) => FmtLine::TableRow(FmtTableRow::from(table_row, skin)),
            Line::TableRule(TableRule { cells }) => FmtLine::TableRule(FmtTableRule {
                position: RelativePosition::Other,
                widths: Vec::new(),
                aligns: cells,
            }),
            Line::HorizontalRule => FmtLine::HorizontalRule,
            Line::CodeFence(..) => FmtLine::HorizontalRule, // we're not supposed to get code fence in clean texts
        }
    }
    pub fn visible_length(&self) -> usize {
        match self {
            FmtLine::Normal(composite) => composite.visible_length,
            FmtLine::TableRow(row) => row.cells.iter().fold(0, |s, c| s + c.visible_length), // Is that right ? no spacing ?
            FmtLine::TableRule(rule) => 1 + rule.widths.iter().fold(0, |s, w| s + w + 1),
            FmtLine::HorizontalRule => 0, // No intrinsic width
        }
    }
}
