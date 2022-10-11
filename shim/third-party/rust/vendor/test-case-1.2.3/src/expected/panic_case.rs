use crate::expected::Case;
use crate::utils::fmt_syn;
use std::fmt;
use syn::parse_quote;
use syn::{Attribute, Expr, LitStr};

#[cfg_attr(test, derive(Debug, PartialEq))]
pub struct PanicCase {
    value: Option<LitStr>,
}

impl fmt::Display for PanicCase {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "panics {}", fmt_syn(&self.value))
    }
}

impl Case for PanicCase {
    fn body(&self) -> Option<Expr> {
        None
    }

    fn attr(&self) -> Option<Attribute> {
        if let Some(value) = self.value.as_ref() {
            Some(parse_quote! { #[should_panic(expected = #value)] })
        } else {
            Some(parse_quote! { #[should_panic] })
        }
    }
}

impl PanicCase {
    pub fn new(value: Option<LitStr>) -> Self {
        Self { value }
    }
}
