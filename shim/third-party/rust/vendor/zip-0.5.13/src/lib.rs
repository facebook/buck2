//! An ergonomic API for reading and writing ZIP files.
//!
//! The current implementation is based on [PKWARE's APPNOTE.TXT v6.3.9](https://pkware.cachefly.net/webdocs/casestudies/APPNOTE.TXT)
// TODO(#184): Decide on the crate's bias: Do we prioritise permissiveness/correctness/speed/ergonomics?

#![warn(missing_docs)]

pub use crate::compression::CompressionMethod;
pub use crate::read::ZipArchive;
pub use crate::types::DateTime;
pub use crate::write::ZipWriter;

mod compression;
mod cp437;
mod crc32;
pub mod read;
pub mod result;
mod spec;
mod types;
pub mod write;
mod zipcrypto;
