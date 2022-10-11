use crate::*;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TableRow<'a> {
    pub cells: Vec<Composite<'a>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TableRule {
    pub cells: Vec<Alignment>,
}

impl<'a> TableRow<'a> {
    /// Try to read the cells as formatting cells
    /// (i.e. like `|:-:|-:|:-----|`
    /// Implementation note:
    ///  it could me more efficiently be tested during initial
    ///  reading but I don't really want to duplicate the code
    ///  of line_parser::parse_compounds until everything is
    ///  stabilized. If it proves necessary I'll do a
    ///  line_parser::parse_cell (and parse_compound won't take
    ///  a bool paramater anymore).
    pub fn as_table_alignments(&self) -> Option<TableRule> {
        let mut formats = TableRule { cells: Vec::new() };
        for cell in &self.cells {
            if cell.compounds.len() != 1 {
                return None;
            }
            let c = &cell.compounds[0].as_str();
            let mut left_colon = false;
            let mut right_colon = false;
            for (idx, char) in c.char_indices() {
                match char {
                    ':' if idx == 0 => left_colon = true,
                    ':' => right_colon = true,
                    '-' => {}
                    _ => return None,
                }
            }
            formats.cells.push(match (left_colon, right_colon) {
                (false, false) => Alignment::Unspecified,
                (true, false) => Alignment::Left,
                (false, true) => Alignment::Right,
                (true, true) => Alignment::Center,
            });
        }
        Some(formats)
    }
}
