use crate::*;

pub const MAX_HEADER_DEPTH: usize = 8;

/// a parsed line
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Line<'a> {
    Normal(Composite<'a>),  //
    TableRow(TableRow<'a>), // a normal table row, with cells having content
    TableRule(TableRule),   // a separator/border in a table, optionally defining alignments
    HorizontalRule,         // an horizontal line dividing the screen
    CodeFence(Composite<'a>),
}

impl Line<'_> {
    pub fn from(md: &str) -> Line<'_> {
        LineParser::from(md).line()
    }
    #[inline(always)]
    pub fn char_length(&self) -> usize {
        match self {
            Line::Normal(composite) => composite.char_length(),
            Line::TableRow(row) => row.cells.iter().fold(0, |s, c| s + c.char_length()),
            _ => 0, // no known char length for table format lines
        }
    }
    pub fn new_paragraph(compounds: Vec<Compound<'_>>) -> Line<'_> {
        Line::Normal(Composite {
            style: CompositeStyle::Paragraph,
            compounds,
        })
    }
    pub fn empty_code_fence() -> Line<'static> {
        Line::CodeFence(Composite {
            style: CompositeStyle::Paragraph,
            compounds: vec![],
        })
    }
    pub fn new_code_fence(compounds: Vec<Compound<'_>>) -> Line<'_> {
        Line::CodeFence(Composite {
            style: CompositeStyle::Paragraph,
            compounds,
        })
    }
    pub fn new_code(compound: Compound<'_>) -> Line<'_> {
        Line::Normal(Composite {
            style: CompositeStyle::Code,
            compounds: vec![compound],
        })
    }
    pub fn new_quote(compounds: Vec<Compound<'_>>) -> Line<'_> {
        Line::Normal(Composite {
            style: CompositeStyle::Quote,
            compounds,
        })
    }
    pub fn new_list_item(compounds: Vec<Compound<'_>>) -> Line<'_> {
        Line::Normal(Composite {
            style: CompositeStyle::ListItem,
            compounds,
        })
    }
    pub fn new_header(level: u8, compounds: Vec<Compound<'_>>) -> Line<'_> {
        Line::Normal(Composite {
            style: CompositeStyle::Header(level),
            compounds,
        })
    }
    pub fn new_table_row(cells: Vec<Composite<'_>>) -> Line<'_> {
        Line::TableRow(TableRow { cells })
    }
    pub fn new_table_alignments(cells: Vec<Alignment>) -> Line<'static> {
        Line::TableRule(TableRule { cells })
    }
    #[inline(always)]
    pub fn is_table_row(&self) -> bool {
        matches!(self, Line::TableRow(_))
    }
    #[inline(always)]
    #[allow(clippy::match_like_matches_macro)]
    pub fn is_table_part(&self) -> bool {
        match self {
            Line::Normal(_) => false,
            _ => true,
        }
    }
    #[inline(always)]
    pub fn is_code(&self) -> bool {
        match self {
            Line::Normal(composite) => composite.is_code(),
            _ => false,
        }
    }
}

#[test]
pub fn count_chars() {
    assert_eq!(Line::from("τ").char_length(), 1);
    assert_eq!(Line::from("τ:`2π`").char_length(), 4);
    assert_eq!(Line::from("* item").char_length(), 4);
}
