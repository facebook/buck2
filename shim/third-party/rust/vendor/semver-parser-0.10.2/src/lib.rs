struct SemverParser;
#[rustfmt::skip]
mod generated;
use self::generated::*;

mod range_set;
pub use crate::range_set::Compat;
pub use crate::range_set::RangeSet;

mod range;
pub use crate::range::Comparator;
pub use crate::range::Identifier;
pub use crate::range::Op;
pub use crate::range::Range;

// from old lib:
pub mod lexer;
pub mod parser;
// pub mod range;
pub mod version;
