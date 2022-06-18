//!
//! Documentation generation utilities

use starlark::values::docs::DocItem;

/// A starlark object to be documented
pub struct StarlarkObject {
    pub name: &'static str,
    pub module: Box<dyn Fn() -> DocItem>,
}

impl StarlarkObject {
    /// Gets the docitem to this starlark object
    pub fn get_docs(&self) -> DocItem {
        (self.module)()
    }
}
