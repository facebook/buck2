/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{cell::RefCell, collections::HashMap, fmt, fmt::Display, sync::Arc};

use buck2_core::cells::paths::CellPath;
use gazebo::{any::ProvidesStaticType, dupe::Dupe};
use starlark::{
    environment::{GlobalsBuilder, Methods, MethodsBuilder, MethodsStatic},
    eval::{Arguments, Evaluator, ParametersSpec},
    values::{
        docs,
        docs::{DocItem, DocString},
        AllocValue, Freeze, Freezer, FrozenValue, Heap, NoSerialize, StarlarkValue, Trace, Value,
        ValueLike,
    },
};

use crate::interpreter::rule_defs::provider::{
    registration::ProviderRegistration, user::user_provider_creator, ProviderError, ProviderId,
};

pub trait ProviderCallableLike {
    fn id(&self) -> Option<&Arc<ProviderId>>;

    /// Frozen callables should always have this set. It's an error if somehow it doesn't.
    fn require_id(&self) -> anyhow::Result<Arc<ProviderId>> {
        match self.id() {
            Some(id) => Ok(id.dupe()),
            None => Err(ProviderError::ProviderCallableMissingID.into()),
        }
    }

    fn provider_callable_documentation(
        &self,
        docs: &Option<DocString>,
        fields: &[String],
        field_docs: &[Option<DocString>],
    ) -> Option<DocItem> {
        let members = fields
            .iter()
            .zip(field_docs.iter())
            .map(|(name, docs)| {
                let prop = docs::Member::Property(docs::Property {
                    docs: docs.clone(),
                    // TODO(nmj): types are not enforced in providers yet. Add type info when
                    //            we add that
                    typ: None,
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

pub trait ValueAsProviderCallableLike<'v> {
    fn as_provider_callable(&self) -> Option<&'v dyn ProviderCallableLike>;
}

impl<'v, V: ValueLike<'v>> ValueAsProviderCallableLike<'v> for V {
    fn as_provider_callable(&self) -> Option<&'v dyn ProviderCallableLike> {
        if let Some(o) = self.downcast_ref::<FrozenProviderCallable>() {
            return Some(o as &dyn ProviderCallableLike);
        } else if let Some(o) = self.downcast_ref::<ProviderCallable>() {
            return Some(o as &dyn ProviderCallableLike);
        }

        // TODO(cjhopman): May be better to construct a map of type->downcast_fn rather than checking them all.
        let v = self.to_value();
        for registration in inventory::iter::<ProviderRegistration> {
            if let Some(v) = (registration.as_provider_callable)(v) {
                return Some(v);
            }
        }
        None
    }
}

fn create_callable_function_signature(
    function_name: &str,
    fields: &[String],
) -> ParametersSpec<FrozenValue> {
    let mut signature = ParametersSpec::with_capacity(function_name.to_owned(), fields.len());
    // TODO(nmj): Should double check we don't actually need positional args in-repo
    signature.no_more_positional_args();
    for field in fields {
        signature.defaulted(field, FrozenValue::new_none());
    }

    signature.finish()
}

#[derive(Debug)]
enum ProviderCallableImpl {
    Unbound,
    Bound(ParametersSpec<FrozenValue>, Arc<ProviderId>, Vec<String>),
}

impl ProviderCallableImpl {
    fn invoke<'v>(
        &self,
        args: &Arguments<'v, '_>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        match self {
            ProviderCallableImpl::Unbound => Err(ProviderError::NotBound.into()),
            ProviderCallableImpl::Bound(signature, id, fields) => {
                signature.parser(args, eval, |parser, eval| {
                    user_provider_creator(id.dupe(), fields, eval, parser)
                })
            }
        }
    }
}

/// The result of calling `provider()`. This is a callable that accepts the fields
/// provided in the `provider()` call, and generates a Starlark `UserProvider` object.
///
/// This object must be assigned to a variable at the top level of the module before it may be invoked
///
/// Field values default to `None`
#[derive(Debug, ProvidesStaticType, Trace, NoSerialize)]
pub struct ProviderCallable {
    /// The name of this provider, filled in by `export_as()`. This must be set before this
    /// object can be called and Providers created.
    #[trace(unsafe_ignore)]
    id: RefCell<Option<Arc<ProviderId>>>,
    /// The path where this `ProviderCallable` is created and assigned
    #[trace(unsafe_ignore)]
    path: CellPath,
    /// The docstring for this provider
    docs: Option<DocString>,
    /// The docstrings for each field. The length of must be identical to `fields`
    field_docs: Vec<Option<DocString>>,
    /// The names of the fields used in `callable`
    fields: Vec<String>,
    /// The actual callable that creates instances of `UserProvider`
    #[trace(unsafe_ignore)]
    callable: RefCell<ProviderCallableImpl>,
}

impl Display for ProviderCallable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.id() {
            None => write!(f, "unnamed provider"),
            Some(id) => {
                write!(f, "{}(", id.name)?;
                for (i, x) in self.fields.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", x)?;
                }
                write!(f, ")")
            }
        }
    }
}

impl ProviderCallable {
    pub fn new(
        path: CellPath,
        docs: Option<DocString>,
        field_docs: Vec<Option<DocString>>,
        fields: Vec<String>,
    ) -> Self {
        assert_eq!(
            field_docs.len(),
            fields.len(),
            "Expected {} fields, but got docs for {} fields",
            fields.len(),
            field_docs.len()
        );
        Self {
            id: RefCell::new(None),
            path,
            docs,
            field_docs,
            fields,
            callable: RefCell::new(ProviderCallableImpl::Unbound),
        }
    }

    /// Get the documentation for all builtin providers that have been registered with `inventory`
    pub fn builtin_provider_documentation() -> HashMap<String, Option<DocItem>> {
        let mut provider_globals_builder = GlobalsBuilder::new();
        for registration in inventory::iter::<ProviderRegistration> {
            (registration.register_globals)(&mut provider_globals_builder);
        }
        let provider_globals = provider_globals_builder.build();
        provider_globals.member_documentation()
    }
}

impl ProviderCallableLike for ProviderCallable {
    fn id(&self) -> Option<&Arc<ProviderId>> {
        // Safe because once we set id, we never change it
        unsafe { self.id.try_borrow_unguarded().unwrap().as_ref() }
    }
}

impl<'v> AllocValue<'v> for ProviderCallable {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_complex(self)
    }
}

impl Freeze for ProviderCallable {
    type Frozen = FrozenProviderCallable;
    fn freeze(self, _freezer: &Freezer) -> anyhow::Result<Self::Frozen> {
        let callable = self.callable.into_inner();
        let id = match self.id.into_inner() {
            Some(x) => x,
            None => {
                // Unfortunately we have no name or location for the provider at this point,
                // so reproduce the fields so that the provider can be identified.
                return Err(ProviderError::ProviderNotAssigned(self.fields).into());
            }
        };

        Ok(FrozenProviderCallable::new(
            id,
            self.docs,
            self.field_docs,
            self.fields,
            callable,
        ))
    }
}

impl<'v> StarlarkValue<'v> for ProviderCallable {
    starlark_type!("provider_callable");

    fn export_as(&self, variable_name: &str, _eval: &mut Evaluator<'v, '_>) {
        // First export wins
        let mut id = self.id.borrow_mut();
        if id.is_none() {
            let new_id = Arc::new(ProviderId {
                path: Some(self.path.clone()),
                name: variable_name.to_owned(),
            });
            *id = Some(new_id.dupe());
            *self.callable.borrow_mut() = ProviderCallableImpl::Bound(
                create_callable_function_signature(&new_id.name, &self.fields),
                new_id,
                self.fields.clone(),
            );
        }
    }

    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(provider_callable_methods)
    }

    fn invoke(
        &self,
        _me: Value<'v>,
        args: &Arguments<'v, '_>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        self.callable.borrow().invoke(args, eval)
    }

    fn documentation(&self) -> Option<DocItem> {
        self.provider_callable_documentation(&self.docs, &self.fields, &self.field_docs)
    }
}

#[derive(Debug, ProvidesStaticType, NoSerialize)]
pub struct FrozenProviderCallable {
    /// The name of this provider, filled in by `export_as()`. This must be set before this
    /// object can be called and Providers created.
    id: Arc<ProviderId>,
    /// The docstring for this provider
    docs: Option<DocString>,
    /// The docstrings for each field. The length of must be identical to `fields`
    field_docs: Vec<Option<DocString>>,
    /// The names of the fields used in `callable`
    fields: Vec<String>,
    /// The actual callable that creates instances of `UserProvider`
    callable: ProviderCallableImpl,
}
starlark_simple_value!(FrozenProviderCallable);

impl Display for FrozenProviderCallable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}(", self.id.name)?;
        for (i, x) in self.fields.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", x)?;
        }
        write!(f, ")")
    }
}

impl FrozenProviderCallable {
    fn new(
        id: Arc<ProviderId>,
        docs: Option<DocString>,
        field_docs: Vec<Option<DocString>>,
        fields: Vec<String>,
        callable: ProviderCallableImpl,
    ) -> Self {
        assert_eq!(
            field_docs.len(),
            fields.len(),
            "Expected {} fields, but got docs for {} fields",
            fields.len(),
            field_docs.len()
        );
        Self {
            id,
            docs,
            field_docs,
            fields,
            callable,
        }
    }
}

impl ProviderCallableLike for FrozenProviderCallable {
    fn id(&self) -> Option<&Arc<ProviderId>> {
        Some(&self.id)
    }
}

impl<'v> StarlarkValue<'v> for FrozenProviderCallable {
    starlark_type!("provider_callable");

    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(provider_callable_methods)
    }

    fn invoke(
        &self,
        _me: Value<'v>,
        args: &Arguments<'v, '_>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        self.callable.invoke(args, eval)
    }

    fn documentation(&self) -> Option<DocItem> {
        self.provider_callable_documentation(&self.docs, &self.fields, &self.field_docs)
    }
}

#[starlark_module]
fn provider_callable_methods(builder: &mut MethodsBuilder) {
    #[starlark(attribute)]
    fn r#type<'v>(this: Value<'v>, heap: &Heap) -> anyhow::Result<Value<'v>> {
        if let Some(x) = this.downcast_ref::<ProviderCallable>() {
            match &*x.id.borrow() {
                None => Err(ProviderError::ProviderNotAssigned(x.fields.clone()).into()),
                Some(id) => Ok(heap.alloc(id.name.as_str())),
            }
        } else if let Some(x) = this.downcast_ref::<FrozenProviderCallable>() {
            Ok(heap.alloc(x.id.name.as_str()))
        } else {
            unreachable!(
                "This parameter must be one of the types, but got `{}`",
                this.get_type()
            )
        }
    }
}
