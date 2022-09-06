//! should produce a link to [`crate::dep::Struct`] [`Struct`]
//! should produce a link to [`std::time::Duration`]

use fbinit::FacebookInit;

pub use crate::dep::Struct;

mod dep;

/// should produce a link to [`fbinit::FacebookInit`]
pub fn foo(_fb: FacebookInit) {
    let _ = Struct {};
    let _ = std::time::Duration::from_secs(5);
}
