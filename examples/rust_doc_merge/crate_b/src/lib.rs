//! Crate B: depends on crate A, re-exports and extends it.

use std::fmt;

use crate_a::Greeter;
use crate_a::WidgetA;

/// A widget that wraps a [`WidgetA`] with extra metadata.
#[derive(Debug, Default)]
pub struct WidgetB {
    /// Inner widget from crate A.
    pub inner: WidgetA,
    /// Count of something.
    pub count: u32,
}

impl WidgetB {
    /// Construct a new [`WidgetB`] wrapping a freshly-made [`WidgetA`].
    pub fn new(name: &str, count: u32) -> Self {
        Self {
            inner: WidgetA::new(name),
            count,
        }
    }

    /// Produce a greeting via [`crate_a::greet`].
    pub fn greet(&self) -> String {
        crate_a::greet(&self.inner.name)
    }
}

impl fmt::Display for WidgetB {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "WidgetB({}, {})", self.inner.name, self.count)
    }
}

impl Greeter for WidgetB {
    fn greeting(&self) -> String {
        format!("{} (x{})", self.inner.greeting(), self.count)
    }
}
