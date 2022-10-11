use crate::expected::Case;
use crate::utils::fmt_syn;
use std::fmt;
use syn::parse_quote;
use syn::{Attribute, Expr};

#[cfg_attr(test, derive(Debug, PartialEq))]
pub struct HamcrestCase {
    value: Box<Expr>,
}

impl fmt::Display for HamcrestCase {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "is {}", fmt_syn(&self.value))
    }
}

impl Case for HamcrestCase {
    fn body(&self) -> Option<Expr> {
        let value = &self.value;
        Some(parse_quote! { assert_that!(_result, #value) })
    }

    fn attr(&self) -> Option<Attribute> {
        None
    }
}

impl HamcrestCase {
    pub fn new(value: Box<Expr>) -> Self {
        Self { value }
    }
}
