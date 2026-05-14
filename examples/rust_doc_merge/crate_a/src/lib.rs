//! Crate A: a demo crate for testing RFC 3662 merged rustdoc.

use std::fmt;

/// Greet someone from crate A.
///
/// # Examples
///
/// ```
/// assert_eq!(crate_a::greet("world"), "hello world");
/// ```
pub fn greet(who: &str) -> String {
    format!("hello {}", who)
}

/// A simple struct defined in crate A.
#[derive(Debug, Default)]
pub struct WidgetA {
    /// The widget's display name.
    pub name: String,
}

impl WidgetA {
    /// Construct a new [`WidgetA`].
    pub fn new(name: impl Into<String>) -> Self {
        Self { name: name.into() }
    }
}

impl fmt::Display for WidgetA {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "WidgetA({})", self.name)
    }
}

/// A trait for things that can produce a greeting.
///
/// Implemented by [`WidgetA`] here and by `WidgetB` in `crate_b` — the
/// rustdoc merge step has to stitch both impls into the same
/// `trait.impl/crate_a/trait.Greeter.js` file.
pub trait Greeter {
    /// Return a greeting string.
    fn greeting(&self) -> String;
}

impl Greeter for WidgetA {
    fn greeting(&self) -> String {
        greet(&self.name)
    }
}
