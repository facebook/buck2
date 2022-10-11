pub mod expr_case;
pub mod ignore_case;
pub mod panic_case;
pub mod pattern_case;

#[cfg(any(feature = "hamcrest_assertions", test))]
pub mod hamcrest_case;

use crate::expected::expr_case::ExprCase;
use crate::expected::ignore_case::IgnoreCase;
use crate::expected::panic_case::PanicCase;
use crate::expected::pattern_case::PatternCase;

#[cfg(any(feature = "hamcrest_assertions", test))]
use crate::expected::hamcrest_case::HamcrestCase;

use std::fmt;
use syn::parse::{Parse, ParseStream};
use syn::{Attribute, Error, Expr, LitStr, Pat};

use cfg_if::cfg_if;

mod kw {
    syn::custom_keyword!(matches);
    syn::custom_keyword!(panics);
    syn::custom_keyword!(inconclusive);
    syn::custom_keyword!(is);
    syn::custom_keyword!(it);
}

#[cfg_attr(test, derive(Debug, PartialEq))]
pub enum Expected {
    Pattern(PatternCase),
    Panic(PanicCase),
    Ignore(IgnoreCase),
    Expr(ExprCase),

    #[cfg(any(feature = "hamcrest_assertions", test))]
    Hamcrest(HamcrestCase),
}

pub trait Case {
    fn body(&self) -> Option<Expr>;
    fn attr(&self) -> Option<Attribute>;
}

impl fmt::Display for Expected {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expected::Pattern(v) => write!(f, "{}", v),
            Expected::Panic(v) => write!(f, "{}", v),
            Expected::Ignore(v) => write!(f, "{}", v),
            Expected::Expr(v) => write!(f, "{}", v),

            #[cfg(any(feature = "hamcrest_assertions", test))]
            Expected::Hamcrest(v) => write!(f, "{}", v),
        }
    }
}

impl Parse for Expected {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        if input.peek(kw::matches) {
            let _kw = input.parse::<kw::matches>()?;
            return Ok(Expected::new_pattern(input.parse()?));
        }

        if input.peek(kw::panics) {
            let _kw = input.parse::<kw::panics>()?;

            let value = if input.peek(LitStr) {
                Some(input.parse()?)
            } else {
                None
            };

            return Ok(Expected::new_panic(value));
        }

        if input.peek(kw::inconclusive) {
            let _kw = input.parse::<kw::inconclusive>()?;
            return Ok(Expected::new_ignore(input.parse()?));
        }

        if input.peek(kw::is) {
            let _kw = input.parse::<kw::is>()?;
            cfg_if! {
                if #[cfg(any(feature="hamcrest_assertions", test))] {
                    return Ok(Expected::new_hamcrest(input.parse()?));
                } else {
                    panic!("Hamcrest2 assertions require 'hamcrest_assertions' feature")
                }
            }
        }

        if input.peek(kw::it) {
            let _kw = input.parse::<kw::it>()?;
            cfg_if! {
                if #[cfg(any(feature="hamcrest_assertions", test))] {
                    return Ok(Expected::new_hamcrest(input.parse()?));
                } else {
                    panic!("Hamcrest2 assertions require 'hamcrest_assertions' feature")
                }
            }
        }

        Ok(Expected::new_expr(input.parse()?))
    }
}

impl Expected {
    pub fn new_pattern(pat: Pat) -> Self {
        Expected::Pattern(PatternCase::new(pat))
    }

    pub fn new_panic(lit_str: Option<LitStr>) -> Self {
        Expected::Panic(PanicCase::new(lit_str))
    }

    pub fn new_ignore(expr: Box<Expr>) -> Self {
        Expected::Ignore(IgnoreCase::new(expr))
    }

    pub fn new_expr(expr: Box<Expr>) -> Self {
        Expected::Expr(ExprCase::new(expr))
    }

    #[cfg(any(feature = "hamcrest_assertions", test))]
    pub fn new_hamcrest(expr: Box<Expr>) -> Self {
        Expected::Hamcrest(HamcrestCase::new(expr))
    }

    pub fn case(&self) -> &dyn Case {
        match self {
            Expected::Pattern(e) => e,
            Expected::Panic(e) => e,
            Expected::Ignore(e) => e,
            Expected::Expr(e) => e,

            #[cfg(any(feature = "hamcrest_assertions", test))]
            Expected::Hamcrest(e) => e,
        }
    }
}
