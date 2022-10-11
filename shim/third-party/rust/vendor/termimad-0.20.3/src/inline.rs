use std::fmt;

use crate::composite::FmtComposite;
use crate::skin::MadSkin;

/// A directly printable markdown snippet, complete
///  with the reference to a skin so that it can
///  implement the Display trait.
///
/// Use this when you don't have a text but just
///  part of a line
pub struct FmtInline<'k, 's> {
    pub skin: &'k MadSkin,
    pub composite: FmtComposite<'s>,
}

impl fmt::Display for FmtInline<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.skin.write_fmt_composite(f, &self.composite, None, false)
    }
}
