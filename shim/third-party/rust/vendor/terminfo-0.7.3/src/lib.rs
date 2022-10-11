//            DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
//                    Version 2, December 2004
//
// Copyleft (ↄ) meh. <meh@schizofreni.co> | http://meh.schizofreni.co
//
// Everyone is permitted to copy and distribute verbatim or modified
// copies of this license document, and changing it is allowed as long
// as the name is changed.
//
//            DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
//   TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
//
//  0. You just DO WHAT THE FUCK YOU WANT TO.

#[macro_use]
extern crate nom;
extern crate phf;
extern crate fnv;

mod error;
pub use crate::error::{Error, Result};

/// Parsers for various formats.
pub mod parser;

/// String capability expansion.
#[macro_use]
pub mod expand;
pub use crate::expand::Expand;

/// Standard terminal capabilities.
pub mod capability;
pub use crate::capability::{Capability, Value};

mod database;
pub use crate::database::Database;

/// Constants to deal with name differences across terminfo and termcap.
pub mod names;
