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
    fn get_docs(&self) -> DocItem {
        (self.module)()
    }

    /// Gets all the documents from registered Starlark objects
    pub fn all_docs() -> impl Iterator<Item = (&'static str, DocItem)> {
        inventory::iter::<StarlarkObject>
            .into_iter()
            .map(|obj| (obj.name, obj.get_docs()))
    }
}

inventory::collect!(StarlarkObject);

/// __derive_refs allows us to reference other crates in buck2_docs_gen_derive without users
/// needing to be aware of those dependencies. We make them public here and then can reference
/// them like `buck2_docs_gen::__derive_refs::foo`.
#[doc(hidden)]
pub mod __derive_refs {
    pub use inventory;
    pub use starlark;
}
