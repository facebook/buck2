//! `finl_unicode` is a crate to provide Unicode support for the finl project. This is not necessarily
//! meant to be a comoprehensive Unicode support, although I will consider adding additional use cases
//! as necessary. Unicode 14.0.0 is implemented in the current version.
//!
//! Two features are currently supported:
//! - **Unicode segmentation**. (Specify `clusters` as a feature when importing the crate.) For a peekable iterator of `CharIndices`, we extend that iterator to
//!   include a `next_cluster` method which returns `Option<String>` which will contain the next
//!   grapheme cluster if there is one or `None` if there isn't.
//! - **Character category**. (Specify `categories` as a feature when importing the crate.) Extends the `char` class with methods for testing the
//!   category of the character.
//!
//! The default is to compile all features. Note that the Rust compiler/linker will not automatically
//! link unused code, so you most of the time, there will be no need to remove features.
//!
//! Building the crate runs a build script which connects to unicode.org to download the data files.

#[cfg(feature = "categories")]
pub mod categories;

#[cfg(feature = "grapheme_clusters")]
pub mod grapheme_clusters;

mod data;