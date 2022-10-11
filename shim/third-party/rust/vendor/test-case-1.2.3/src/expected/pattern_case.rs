use crate::expected::Case;
use crate::utils::fmt_syn;
use quote::quote;
use std::fmt;
use syn::parse_quote;
use syn::{Attribute, Expr, Pat};

#[cfg_attr(test, derive(Debug, PartialEq))]
pub struct PatternCase {
    value: Pat,
}

impl fmt::Display for PatternCase {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "matches {}", fmt_syn(&self.value))
    }
}

impl Case for PatternCase {
    fn body(&self) -> Option<Expr> {
        let value = &self.value;
        let pat_str = format!("{}", quote! { #value });
        Some(parse_quote! {
            match _result {
                #value => (),
                e => panic!("Expected {} found {:?}", #pat_str, e)
            }
        })
    }

    fn attr(&self) -> Option<Attribute> {
        None
    }
}

impl PatternCase {
    pub fn new(value: Pat) -> Self {
        Self { value }
    }
}
