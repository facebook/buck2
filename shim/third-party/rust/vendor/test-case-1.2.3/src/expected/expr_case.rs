use crate::expected::Case;
use crate::utils::fmt_syn;
use std::fmt;
use syn::parse_quote;
use syn::{Attribute, Expr};

#[cfg_attr(test, derive(Debug, PartialEq))]
pub struct ExprCase {
    value: Box<Expr>,
}

impl fmt::Display for ExprCase {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "expects {}", fmt_syn(&self.value))
    }
}

impl Case for ExprCase {
    fn body(&self) -> Option<Expr> {
        let value = &self.value;
        Some(parse_quote! { assert_eq!(#value, _result) })
    }

    fn attr(&self) -> Option<Attribute> {
        None
    }
}

impl ExprCase {
    pub fn new(value: Box<Expr>) -> Self {
        Self { value }
    }
}
