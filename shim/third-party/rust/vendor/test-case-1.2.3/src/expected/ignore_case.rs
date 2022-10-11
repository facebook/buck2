use crate::expected::Case;
use crate::utils::fmt_syn;
use std::fmt;
use syn::parse_quote;
use syn::{Attribute, Expr};

#[cfg_attr(test, derive(Debug, PartialEq))]
pub struct IgnoreCase {
    value: Box<Expr>,
}

impl fmt::Display for IgnoreCase {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ignored {}", fmt_syn(&self.value))
    }
}

impl Case for IgnoreCase {
    fn body(&self) -> Option<Expr> {
        None
    }

    fn attr(&self) -> Option<Attribute> {
        Some(parse_quote! { #[ignore] })
    }
}

impl IgnoreCase {
    pub fn new(value: Box<Expr>) -> Self {
        Self { value }
    }
}
