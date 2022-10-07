use std::sync::Arc;

use buck2_core::provider::id::ProviderId;
use gazebo::any::ProvidesStaticType;
use gazebo::dupe::Dupe;
use starlark::values::docs;
use starlark::values::docs::DocItem;
use starlark::values::docs::DocString;
use starlark::values::docs::Type;
use starlark::values::ValueLike;

#[derive(Debug, thiserror::Error)]
enum ProviderCallableError {
    #[error("provider callable did not have a bound id; this is an internal error")]
    ProviderCallableMissingID,
}

pub trait ProviderCallableLike {
    fn id(&self) -> Option<&Arc<ProviderId>>;

    /// Frozen callables should always have this set. It's an error if somehow it doesn't.
    fn require_id(&self) -> anyhow::Result<Arc<ProviderId>> {
        match self.id() {
            Some(id) => Ok(id.dupe()),
            None => Err(ProviderCallableError::ProviderCallableMissingID.into()),
        }
    }

    fn provider_callable_documentation(
        &self,
        docs: &Option<DocString>,
        fields: &[String],
        field_docs: &[Option<DocString>],
        field_types: &[Option<Type>],
    ) -> Option<DocItem> {
        let members = itertools::izip!(fields.iter(), field_docs.iter(), field_types.iter())
            .map(|(name, docs, return_type)| {
                let prop = docs::Member::Property(docs::Property {
                    docs: docs.clone(),
                    typ: return_type.clone(),
                });
                (name.to_owned(), prop)
            })
            .collect();
        Some(DocItem::Object(docs::Object {
            docs: docs.clone(),
            members,
        }))
    }
}

unsafe impl<'v> ProvidesStaticType for &'v dyn ProviderCallableLike {
    type StaticType = &'static dyn ProviderCallableLike;
}

pub trait ValueAsProviderCallableLike<'v> {
    fn as_provider_callable(&self) -> Option<&'v dyn ProviderCallableLike>;
}

impl<'v, V: ValueLike<'v>> ValueAsProviderCallableLike<'v> for V {
    fn as_provider_callable(&self) -> Option<&'v dyn ProviderCallableLike> {
        self.to_value().request_value::<&dyn ProviderCallableLike>()
    }
}
