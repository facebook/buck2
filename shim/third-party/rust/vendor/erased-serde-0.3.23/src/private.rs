//! Not public API. Used as `$crate::__private` by macros.

pub use core::marker::{Send, Sized, Sync};
pub use core::result::Result;
pub use serde;

pub fn require_erased_serialize_impl<T>()
where
    T: ?Sized + crate::Serialize,
{
}
