/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Providers are the data returned from a rule, and are the only way that information from this
//! rule is available to rules that depend on it. Every rule must return at least the `DefaultInfo`
//! provider, but most will also return either `RunInfo` (because they are executable) or some
//! custom provider (because they are incorporated into something that is ultimately executable).
//!
//! Internal providers (those defined and used by buck itself) can be defined easily using the
//! #[internal_provider(creator_func)] macro. This will generate all the code needed for that
//! provider to be used in starlark and to be treated as a provider in the various rust utilities
//! we have for providers.
//!
//! For an internal provider like:
//! ```skip
//! #[internal_provider(create_my_prov)]
//! #[derive(Clone, Debug, Trace, Coerce)]
//! #[repr(transparent)]
//! pub struct MyProviderGen<V> {
//!    field1: V,
//!    field2: V,
//! }
//!
//! #[starlark_module]
//! fn create_my_prov(globals: &mut GlobalsBuilder) {
//!    fn NameDoesntMatter(
//!        // It's not enforced that the args here match the fields, but it's generally the user expectation that they do.
//!        field1: Value<'v>,
//!        field2: Value<'v>,
//!    ) -> MyProvider<'v> {
//!       // Can do some arg validation or computation here, just need to construct the provider.
//!       Ok(MyProvider {
//!            field1,
//!            field2
//!        })
//!    }
//! }
//! ```
//!
//! This will generate a "ProviderCallable" starlark type named (in starlark) `MyProvider` that acts like
//! the instance returned by a `provider()` call in starlark (so can be used to construct instances of the
//! provider or used in places like `attr.dep(required_providers=[MyProvider]))`.
//!
//! For provider instances, in starlark all of their fields will be accessible by the field name.
//!
//! In rust, a StarlarkValue can be converted to the provider like normal with `MyProvider::from_value()`.
//! Often internally we'd have the analysis result (`FrozenProviderCollection`) and want to get the
//! provider out of their so there's a convenience function for that: `MyProvider::from_providers(collect)`.
// TODO(cjhopman): That last one would be more discoverable if we moved it onto the
// `FrozenProviderCollectionValue` itself so you could do `collection.get::<MyProvider>()`.
use std::{
    cell::RefCell,
    cmp::Ordering,
    collections::HashMap,
    fmt::{self, Debug, Display},
    hash::Hash,
    marker::PhantomData,
    sync::Arc,
};

use anyhow::anyhow;
use buck2_core::{
    cells::paths::CellPath,
    provider::{ConfiguredProvidersLabel, ProviderName},
};
use gazebo::{
    any::{AnyLifetime, ProvidesStaticType},
    coerce::Coerce,
    dupe::Dupe,
};
use serde::{Serialize, Serializer};
use starlark::{
    collections::{Hashed, SmallMap, StarlarkHasher},
    environment::{GlobalsBuilder, Methods, MethodsBuilder, MethodsStatic},
    eval::{Arguments, Evaluator, ParametersParser, ParametersSpec},
    starlark_type,
    values::{
        display::{display_container, display_keyed_container},
        docs,
        docs::{DocItem, DocString},
        list::List,
        AllocValue, Freeze, Freezer, FrozenRef, FrozenValue, Heap, NoSerialize, OwnedFrozenValue,
        OwnedFrozenValueTyped, StarlarkValue, Trace, Tracer, Value, ValueError, ValueLike,
    },
};

use crate::interpreter::rule_defs::{
    label::Label,
    provider::default_info::{DefaultInfo, DefaultInfoCallable, FrozenDefaultInfo},
};

pub mod configuration_info;
pub mod constraint_setting_info;
pub mod constraint_value_info;
pub mod default_info;
pub mod execution_platform_info;
pub mod execution_platform_registration_info;
pub mod external_runner_test_info;
pub mod install_info;
pub mod platform_info;
pub mod run_info;
pub mod template_placeholder_info;

#[derive(Debug, thiserror::Error)]
pub enum ProviderError {
    #[error(
        "The result of `provider()` must be assigned to a top-level variable before it can be called"
    )]
    NotBound,
    #[error("expected a list of Provider objects, got {repr}")]
    CollectionNotAList { repr: String },
    #[error("expected a Provider object, got {repr}")]
    CollectionElementNotAProvider { repr: String },
    #[error("provider of type {provider_name} specified twice ({original_repr} and {new_repr})")]
    CollectionSpecifiedProviderTwice {
        provider_name: String,
        original_repr: String,
        new_repr: String,
    },
    #[error("collection {repr} did not receive a DefaultInfo provider")]
    CollectionMissingDefaultInfo { repr: String },
    #[error("provider callable did not have a bound id; this is an internal error")]
    ProviderCallableMissingID,
    #[error(
        "provider value that should have been `DefaultInfo` was not. It was `{repr}`. This is an internal error."
    )]
    ValueIsNotDefaultInfo { repr: String },
    #[error(
        "requested sub target named `{0}` of target `{1}` is not available. Available subtargets are: `{2:?}`"
    )]
    RequestedInvalidSubTarget(ProviderName, ConfiguredProvidersLabel, Vec<String>),
    #[error(
        "Provider type must be assigned to a variable, e.g. `ProviderInfo = provider(fields = {0:?})`"
    )]
    ProviderNotAssigned(Vec<String>),
}

/// A unique identity for a given provider. Allows correlating `ProviderCallable` objects with `UserProvider` objects.
///
/// For example:
/// ```ignore
/// FooInfo = provider(fields=["foo", "bar"])
///
/// def impl(ctx):
///     # We can guarantee when setting up the context that there
///     # is a provider that came from FooInfo
///     ctx.actions.write("out.txt", ctx.attr.dep[FooInfo].bar)
/// foo_binary = rule(implementation=impl, attrs={"dep": attr.dep(providers=[FooInfo])})
/// ```
#[derive(Debug, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct ProviderId {
    /// This is present for all user-specified providers. This is only None if it is a
    /// native provider, which has no affiliated .bzl file
    path: Option<CellPath>,
    name: String,
}

impl Display for ProviderId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.name)
    }
}

pub struct ProviderIdWithType<T> {
    id: Arc<ProviderId>,
    t: PhantomData<T>,
}

impl<T> ProviderIdWithType<T> {
    pub fn id(&self) -> &Arc<ProviderId> {
        &self.id
    }

    pub fn new(path: Option<CellPath>, name: String) -> Self {
        Self {
            id: Arc::new(ProviderId { path, name }),
            t: Default::default(),
        }
    }
}

impl ProviderId {
    pub fn name(&self) -> &str {
        &self.name
    }
}

struct ProviderRegistration {
    as_provider_callable: fn(Value) -> Option<&dyn ProviderCallableLike>,
    as_provider: fn(Value) -> Option<&dyn ProviderLike>,
    register_globals: fn(&mut GlobalsBuilder),
}

inventory::collect!(ProviderRegistration);

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

/// Creates instances of mutable `UserProvider`s; called from a `NativeFunction`
fn user_provider_creator<'v>(
    id: Arc<ProviderId>,
    fields: &[String],
    eval: &Evaluator<'v, '_>,
    mut param_parser: ParametersParser<'v, '_>,
) -> anyhow::Result<Value<'v>> {
    let heap = eval.heap();
    let values = fields
        .iter()
        .map(|field| {
            let user_value = param_parser.next(field)?;
            Ok((field.to_owned(), user_value))
        })
        .collect::<anyhow::Result<SmallMap<String, Value>>>()?;
    Ok(heap.alloc(UserProvider {
        id,
        attributes: values,
    }))
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
#[derive(Debug, AnyLifetime, Trace, NoSerialize)]
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

    fn get_methods(&self) -> Option<&'static Methods> {
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

#[derive(Debug, AnyLifetime, NoSerialize)]
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

impl<'v> ProviderCallableLike for FrozenProviderCallable {
    fn id(&self) -> Option<&Arc<ProviderId>> {
        Some(&self.id)
    }
}

impl<'v> StarlarkValue<'v> for FrozenProviderCallable {
    starlark_type!("provider_callable");

    fn get_methods(&self) -> Option<&'static Methods> {
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

/// The result of calling the output of `provider()`. This is just a simple data structure of
/// either immediately available values or, later, `FutureValue` types that are resolved
/// asynchronously
#[derive(Debug, Clone, Coerce, Trace, Freeze, AnyLifetime)]
#[repr(C)]
pub struct UserProviderGen<V> {
    #[trace(unsafe_ignore)]
    #[freeze(identity)]
    id: Arc<ProviderId>,
    attributes: SmallMap<String, V>,
}

starlark_complex_value!(pub UserProvider);

impl<V: Display> Display for UserProviderGen<V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        display_keyed_container(
            f,
            &format!("{}(", self.id.name),
            ")",
            "=",
            self.attributes.iter(),
        )
    }
}

impl<'v, V: ValueLike<'v>> StarlarkValue<'v> for UserProviderGen<V>
where
    Self: AnyLifetime<'v> + ProvidesStaticType,
{
    starlark_type!("provider");

    fn matches_type(&self, ty: &str) -> bool {
        ty == "provider" || ty == self.id.name
    }

    fn dir_attr(&self) -> Vec<String> {
        self.attributes.keys().cloned().collect()
    }

    fn has_attr(&self, attribute: &str) -> bool {
        self.attributes.contains_key(attribute)
    }

    fn get_attr(&self, attribute: &str, heap: &'v Heap) -> Option<Value<'v>> {
        self.get_attr_hashed(Hashed::new(attribute), heap)
    }

    fn get_attr_hashed(&self, attribute: Hashed<&str>, _heap: &'v Heap) -> Option<Value<'v>> {
        Some(self.attributes.get_hashed(attribute)?.to_value())
    }

    fn get_methods(&self) -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(provider_methods)
    }

    fn equals(&self, other: Value<'v>) -> anyhow::Result<bool> {
        match other.as_provider() {
            None => Ok(false),
            Some(o) => {
                if self.id != *o.id() {
                    return Ok(false);
                }

                let items = o.items();
                if self.attributes.len() != items.len() {
                    return Ok(false);
                }
                for ((k1, v1), (k2, v2)) in self.attributes.iter().zip(items.iter()) {
                    if k1 != k2 {
                        return Ok(false);
                    }
                    if !v1.equals(*v2)? {
                        return Ok(false);
                    }
                }
                Ok(true)
            }
        }
    }

    fn compare(&self, other: Value<'v>) -> anyhow::Result<Ordering> {
        match other.as_provider() {
            None => ValueError::unsupported_with(self, "compare", other),
            Some(o) => {
                match self.id.cmp(o.id()) {
                    Ordering::Equal => {}
                    v => return Ok(v),
                }
                let items = o.items();
                for ((k1, v1), (k2, v2)) in self.attributes.iter().zip(items.iter()) {
                    match k1.as_str().cmp(k2) {
                        Ordering::Equal => {}
                        v => return Ok(v),
                    }

                    match v1.compare(*v2)? {
                        Ordering::Equal => {}
                        v => return Ok(v),
                    }
                }
                match self.attributes.len().cmp(&items.len()) {
                    Ordering::Equal => {}
                    v => return Ok(v),
                }
                Ok(Ordering::Equal)
            }
        }
    }

    fn write_hash(&self, hasher: &mut StarlarkHasher) -> anyhow::Result<()> {
        self.id.hash(hasher);
        for (k, v) in self.attributes.iter() {
            k.hash(hasher);
            v.write_hash(hasher)?;
        }
        Ok(())
    }
}

impl<'v, V: ValueLike<'v>> serde::Serialize for UserProviderGen<V>
where
    Self: AnyLifetime<'v>,
{
    fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        s.collect_map(self.attributes.iter())
    }
}

trait ProviderLike<'v>: Debug {
    /// The ID. Guaranteed to be set on the `ProviderCallable` before constructing this object
    fn id(&self) -> &Arc<ProviderId>;
    /// Gets the value for a given field.
    fn get_field(&self, name: &str) -> Option<Value<'v>>;
    /// Returns a list of all the keys and values.
    // TODO(cjhopman): I'd rather return an iterator. I couldn't get that to work, though.
    fn items(&self) -> Vec<(&str, Value<'v>)>;
}

/// Common methods on user and builtin providers.
#[starlark_module]
pub(crate) fn provider_methods(builder: &mut MethodsBuilder) {
    fn to_json(this: Value) -> anyhow::Result<String> {
        this.to_json()
    }
}

impl<'v, V: ValueLike<'v>> ProviderLike<'v> for UserProviderGen<V>
where
    UserProviderGen<V>: Debug,
{
    fn id(&self) -> &Arc<ProviderId> {
        &self.id
    }

    fn get_field(&self, name: &str) -> Option<Value<'v>> {
        self.attributes.get(name).map(|v| v.to_value())
    }

    fn items(&self) -> Vec<(&str, Value<'v>)> {
        self.attributes
            .iter()
            .map(|(k, v)| (k.as_str(), v.to_value()))
            .collect()
    }
}

trait ValueAsProviderLike<'v> {
    fn as_provider(&self) -> Option<&'v dyn ProviderLike<'v>>;
}

impl<'v, V: ValueLike<'v>> ValueAsProviderLike<'v> for V {
    fn as_provider(&self) -> Option<&'v dyn ProviderLike<'v>> {
        let v = self.to_value();
        if let Some(o) = v.downcast_ref::<FrozenUserProvider>() {
            return Some(o as &dyn ProviderLike<'v>);
        } else if let Some(o) = v.downcast_ref::<UserProvider>() {
            return Some(o as &dyn ProviderLike<'v>);
        }

        // TODO(cjhopman): May be better to construct a map of type->downcast_fn rather than checking them all.
        let v = self.to_value();
        for registration in inventory::iter::<ProviderRegistration> {
            if let Some(v) = (registration.as_provider)(v) {
                return Some(v);
            }
        }

        None
    }
}

/// Holds a collection of `UserProvider`s. These can be accessed in Starlark by indexing on
/// a `ProviderCallable` object.
///
/// e.g.
/// ```ignore
/// FooInfo = provider(fields=["bar"])
/// ....
/// collection[FooInfo] # None if absent, a FooInfo instance if present
/// ```
///
/// This is the result of all UDR implementation functions
#[derive(Debug, AnyLifetime)]
#[repr(C)]
pub struct ProviderCollectionGen<V> {
    providers: SmallMap<Arc<ProviderId>, V>,
}

// Can't derive this since no instance for Arc
unsafe impl<From: Coerce<To>, To> Coerce<ProviderCollectionGen<To>>
    for ProviderCollectionGen<From>
{
}

starlark_complex_value!(pub ProviderCollection);

impl<V: Display> Display for ProviderCollectionGen<V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        display_container(
            f,
            "Providers([",
            "])",
            self.providers.iter().map(|(_, v)| v),
        )
    }
}

impl<'v, V: ValueLike<'v>> Serialize for ProviderCollectionGen<V> {
    fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        s.collect_map(self.providers.iter().map(|(id, v)| (id.name(), v)))
    }
}

impl<'v, V: ValueLike<'v>> ProviderCollectionGen<V> {
    /// Create most of the collection but don't do final assembly, or validate DefaultInfo here.
    /// This is an internal detail
    fn try_from_value_impl(
        value: Value<'v>,
    ) -> anyhow::Result<SmallMap<Arc<ProviderId>, Value<'v>>> {
        let maybe_list: anyhow::Result<_> = match List::from_value(value) {
            Some(v) => Ok(v),
            None => Err(ProviderError::CollectionNotAList {
                repr: value.to_repr(),
            }
            .into()),
        };
        let list = maybe_list?;

        let mut providers = SmallMap::with_capacity(list.len());
        for value in list.iter() {
            match value.as_provider() {
                Some(provider) => {
                    if let Some(existing_value) = providers.insert(provider.id().dupe(), value) {
                        return Err(ProviderError::CollectionSpecifiedProviderTwice {
                            provider_name: provider.id().name.clone(),
                            original_repr: existing_value.to_repr(),
                            new_repr: value.to_repr(),
                        }
                        .into());
                    };
                }
                None => {
                    return Err(ProviderError::CollectionElementNotAProvider {
                        repr: value.to_repr(),
                    }
                    .into());
                }
            }
        }

        Ok(providers)
    }

    /// Takes a value, e.g. a return from a `rule()` implementation function, and builds a `ProviderCollection` from it.
    ///
    /// An error is returned if:
    ///  - `value` is not a list
    ///  - Two instances of the same provider are provided
    ///  - `DefaultInfo` is not provided
    pub fn try_from_value(value: Value<'v>) -> anyhow::Result<ProviderCollection<'v>> {
        let providers = Self::try_from_value_impl(value)?;
        if !providers.contains_key(DefaultInfoCallable::provider_id()) {
            return Err(ProviderError::CollectionMissingDefaultInfo {
                repr: value.to_repr(),
            }
            .into());
        }

        Ok(ProviderCollection::<'v> { providers })
    }

    /// Takes a value, e.g. a return from a `rule()` implementation function, and builds a `ProviderCollection` from it.
    ///
    /// An error is returned if:
    ///  - `value` is not a list
    ///  - Two instances of the same provider are provided
    ///
    /// `default_info_creator` is only invoked if `DefaultInfo` was not in the collection
    pub fn try_from_value_with_default_info(
        value: Value<'v>,
        default_info_creator: impl FnOnce() -> Value<'v>,
    ) -> anyhow::Result<ProviderCollection<'v>> {
        let mut providers = Self::try_from_value_impl(value)?;

        if !providers.contains_key(DefaultInfoCallable::provider_id()) {
            let di_value = default_info_creator();
            if DefaultInfo::from_value(di_value).is_none() {
                return Err(ProviderError::ValueIsNotDefaultInfo {
                    repr: di_value.to_repr(),
                }
                .into());
            }
            providers.insert(DefaultInfoCallable::provider_id().dupe(), di_value);
        }
        Ok(ProviderCollection::<'v> { providers })
    }
}

impl<'v, V: ValueLike<'v>> StarlarkValue<'v> for ProviderCollectionGen<V>
where
    Self: AnyLifetime<'v> + ProvidesStaticType,
{
    starlark_type!("provider_collection");

    fn at(&self, index: Value<'v>, _heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        match index.as_provider_callable() {
            Some(callable) => match self.providers.get(&callable.require_id()?) {
                Some(v) => Ok(v.to_value()),
                None => Ok(Value::new_none()),
            },
            // TODO(nmj): IncorrectParameterTypeNamed might want to be able to say what
            //                 type was expected
            None => Err(ValueError::IncorrectParameterTypeNamed("index".to_owned()).into()),
        }
    }
}

unsafe impl<'v> Trace<'v> for ProviderCollection<'v> {
    fn trace(&mut self, tracer: &Tracer<'v>) {
        self.providers.values_mut().for_each(|v| tracer.trace(v))
    }
}

impl<'v> Freeze for ProviderCollection<'v> {
    type Frozen = FrozenProviderCollection;
    fn freeze(self, freezer: &Freezer) -> anyhow::Result<Self::Frozen> {
        let providers = self
            .providers
            .into_iter()
            .map(|(k, v)| anyhow::Ok((k, freezer.freeze(v)?)))
            .collect::<anyhow::Result<_>>()?;
        Ok(FrozenProviderCollection { providers })
    }
}

impl FrozenProviderCollection {
    pub fn default_info(&self) -> FrozenRef<'static, FrozenDefaultInfo> {
        self.get_provider(DefaultInfoCallable::provider_id_t())
            .expect("DefaultInfo should always be set")
    }

    pub fn default_info_value(&self) -> FrozenValue {
        *self
            .providers
            .get(DefaultInfoCallable::provider_id())
            .expect("DefaultInfo should always be set")
    }

    pub fn contains_provider(&self, provider_id: &ProviderId) -> bool {
        self.providers.contains_key(provider_id)
    }

    pub fn get_provider<T: StarlarkValue<'static>>(
        &self,
        provider_id: &ProviderIdWithType<T>,
    ) -> Option<FrozenRef<'static, T>> {
        self.providers
            .get(provider_id.id())
            .and_then(|v| v.downcast_frozen_ref::<T>())
    }

    pub fn get_provider_raw(&self, provider_id: &ProviderId) -> Option<&FrozenValue> {
        self.providers.get(provider_id)
    }

    pub fn provider_names(&self) -> Vec<String> {
        self.providers.keys().map(|k| k.name.to_owned()).collect()
    }

    pub fn provider_ids(&self) -> Vec<&ProviderId> {
        self.providers.keys().map(|k| &**k).collect()
    }
}

/// Thin wrapper around `FrozenValue` that can only be constructed if that value is a `FrozenProviderCollection`
#[derive(Debug, Clone, Dupe)]
pub struct FrozenProviderCollectionValue {
    value: OwnedFrozenValueTyped<FrozenProviderCollection>,
}

impl Serialize for FrozenProviderCollectionValue {
    fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        (*self.value).serialize(s)
    }
}

impl FrozenProviderCollectionValue {
    pub fn from_value(value: OwnedFrozenValueTyped<FrozenProviderCollection>) -> Self {
        Self { value }
    }

    pub fn try_from_value(value: OwnedFrozenValue) -> anyhow::Result<Self> {
        Ok(Self {
            value: value
                .downcast()
                .map_err(|value| anyhow!("{:?} was not a FrozenProviderCollection", value))?,
        })
    }

    pub fn value(&self) -> &OwnedFrozenValueTyped<FrozenProviderCollection> {
        &self.value
    }

    pub fn provider_collection(&self) -> &FrozenProviderCollection {
        self.value.as_ref()
    }
}

/// Wraps a dependency's `ProvidersLabel` and the result of analysis together for users' rule implementation functions
///
/// From Starlark, the label is accessible with `.label`, and providers from the underlying
/// `ProviderCollection` are available via `[]` (`get()`)
#[derive(Debug, Trace, Coerce, Freeze, AnyLifetime, NoSerialize)]
#[repr(C)]
pub struct DependencyGen<V> {
    label: V,
    providers_collection: V,
}

starlark_complex_value!(pub Dependency);

impl<V: Display> Display for DependencyGen<V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<dependency ")?;
        self.label.fmt(f)?;
        write!(f, ">")
    }
}

impl<'v> Dependency<'v> {
    pub fn new(
        heap: &'v Heap,
        label: ConfiguredProvidersLabel,
        providers_collection: Value<'v>,
    ) -> Self {
        Dependency {
            label: heap.alloc(Label::new(heap, label)),
            providers_collection,
        }
    }
}

impl<'v, V: ValueLike<'v>> StarlarkValue<'v> for DependencyGen<V>
where
    Self: AnyLifetime<'v> + ProvidesStaticType,
{
    starlark_type!("dependency");

    fn get_methods(&self) -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(dependency_functions)
    }

    fn at(&self, index: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        self.providers_collection.to_value().at(index, heap)
    }
}

#[starlark_module]
fn dependency_functions(builder: &mut MethodsBuilder) {
    #[starlark(attribute)]
    fn label<'v>(this: &Dependency) -> anyhow::Result<Value<'v>> {
        Ok(this.label.to_value())
    }

    #[starlark(attribute)]
    fn providers<'v>(this: &Dependency) -> anyhow::Result<Vec<Value<'v>>> {
        let value = this.providers_collection;
        let provider_collection = ProviderCollection::from_value(value)
            .ok_or_else(|| anyhow::anyhow!("{:?} is not a ProviderCollection", value))?;
        Ok(provider_collection.providers.values().copied().collect())
    }
}

pub fn register_builtin_providers(registry: &mut GlobalsBuilder) {
    for registration in inventory::iter::<ProviderRegistration> {
        (registration.register_globals)(registry);
    }
}

#[cfg(test)]
mod tester {
    use gazebo::prelude::*;
    use starlark::{
        environment::GlobalsBuilder,
        values::{Value, ValueLike},
    };

    use crate::interpreter::rule_defs::provider::{
        FrozenProviderCollection, ProviderCollection, ValueAsProviderCallableLike,
    };

    #[starlark_module]
    pub fn collection_creator(builder: &mut GlobalsBuilder) {
        fn create_collection<'v>(value: Value<'v>) -> anyhow::Result<ProviderCollection<'v>> {
            ProviderCollection::try_from_value(value)
        }

        fn get_default_info_default_outputs<'v>(value: Value<'v>) -> anyhow::Result<Value<'v>> {
            let frozen = value
                .unpack_frozen()
                .expect("a frozen value to fetch DefaultInfo");
            let collection = frozen
                .downcast_ref::<FrozenProviderCollection>()
                .ok_or_else(|| anyhow::anyhow!("{:?} was not a FrozenProviderCollection", value))?;

            let ret = collection.default_info().default_outputs_raw().to_value();
            Ok(ret)
        }

        fn get_default_info_sub_targets<'v>(value: Value<'v>) -> anyhow::Result<Value<'v>> {
            let frozen = value
                .unpack_frozen()
                .expect("a frozen value to fetch DefaultInfo");
            let collection = frozen
                .downcast_ref::<FrozenProviderCollection>()
                .ok_or_else(|| anyhow::anyhow!("{:?} was not a FrozenProviderCollection", value))?;

            let ret = collection.default_info().sub_targets_raw().to_value();
            Ok(ret)
        }

        fn contains_provider<'v>(
            collection: Value<'v>,
            provider: Value<'v>,
        ) -> anyhow::Result<bool> {
            let id = provider
                .as_provider_callable()
                .unwrap()
                .id()
                .unwrap()
                .dupe();

            let res = collection
                .unpack_frozen()
                .expect("a frozen value")
                .downcast_ref::<FrozenProviderCollection>()
                .ok_or_else(|| {
                    anyhow::anyhow!("{:?} was not a FrozenProviderCollection", collection)
                })?
                .contains_provider(&id);

            Ok(res)
        }

        fn providers_list<'v>(collection: Value<'v>) -> anyhow::Result<Vec<String>> {
            Ok(collection
                .unpack_frozen()
                .expect("a frozen value")
                .downcast_ref::<FrozenProviderCollection>()
                .ok_or_else(|| {
                    anyhow::anyhow!("{:?} was not a FrozenProviderCollection", collection)
                })?
                .provider_names())
        }
    }
}

#[cfg(test)]
pub mod testing {
    use buck2_core::cells::paths::CellPath;
    use starlark::environment::{GlobalsBuilder, Module};

    use crate::{
        attrs::testing,
        interpreter::rule_defs::{
            artifact::testing::artifactory,
            provider::{register_builtin_providers, FrozenProviderCollectionValue, ProviderId},
        },
    };

    pub trait ProviderIdExt {
        fn testing_new(path: CellPath, name: &str) -> Self;
    }

    impl ProviderIdExt for ProviderId {
        fn testing_new(path: CellPath, name: &str) -> Self {
            ProviderId {
                path: Some(path),
                name: name.to_owned(),
            }
        }
    }

    pub trait FrozenProviderCollectionValueExt {
        /// Creates a `FrozenProviderCollectionValue` for testing. The given string should be
        /// Starlark code that returns a list of providers. The built in providers are available.
        fn testing_new(providers: &str) -> Self;
    }

    impl FrozenProviderCollectionValueExt for FrozenProviderCollectionValue {
        fn testing_new(providers: &str) -> Self {
            let env = Module::new();
            let globals = GlobalsBuilder::extended()
                .with(buck2_interpreter::build_defs::register_natives)
                .with(register_builtin_providers)
                .with(crate::interpreter::build_defs::register_natives)
                .with(artifactory)
                .build();
            let value = testing::to_value(&env, &globals, providers);
            let res_typed =
                crate::interpreter::rule_defs::provider::ProviderCollection::try_from_value(value)
                    .map_err(|e| anyhow::anyhow!("{:?}", e))
                    .unwrap();

            let provider_env = Module::new();
            let res = provider_env.heap().alloc(res_typed);
            provider_env.set("", res);

            let frozen_env = { provider_env.freeze().expect("should freeze successfully") };
            let res = frozen_env.get("").unwrap();

            FrozenProviderCollectionValue::try_from_value(res)
                .expect("just created this, this shouldn't happen")
        }
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use buck2_build_api_derive::internal_provider;
    use buck2_core::{
        configuration::Configuration, provider::ProvidersLabel, result::SharedResult,
        target::TargetLabel,
    };
    use buck2_interpreter::{
        extra::BuildContext,
        pattern::{ParsedPattern, ProvidersPattern},
    };
    use gazebo::{any::AnyLifetime, coerce::Coerce};
    use indoc::indoc;
    use starlark::{
        environment::GlobalsBuilder,
        values::{Freeze, Trace, Value},
    };

    use crate::interpreter::{
        rule_defs::{
            artifact::testing::artifactory,
            provider::{tester::collection_creator, Dependency, ProviderCollection},
        },
        testing::{expect_error, import, run_starlark_test_expecting_error, Tester},
    };

    #[internal_provider(simple_info_creator)]
    #[derive(Clone, Debug, Trace, Coerce, Freeze, AnyLifetime)]
    #[repr(C)]
    pub struct SimpleInfoGen<V> {
        value1: V,
        value2: V,
    }

    #[starlark_module]
    fn simple_info_creator(globals: &mut GlobalsBuilder) {
        fn ConstraintSettingInfo<'v>(
            value1: Value,
            value2: Value,
        ) -> anyhow::Result<SimpleInfo<'v>> {
            Ok(SimpleInfo { value1, value2 })
        }
    }

    #[test]
    fn creates_providers() -> anyhow::Result<()> {
        // TODO(nmj): Starlark doesn't let you call 'new_invoker()' on is_mutable types.
        //                 Once that's fixed, make sure we can call 'FooInfo' before the module is
        //                 frozen.
        let mut tester = Tester::new()?;
        tester.set_additional_globals(Arc::new(|builder| {
            simple_info_creator(builder);
        }));

        tester.run_starlark_test(indoc!(
            r#"
        FooInfo = provider(fields=["bar", "baz"])
        FooInfo2 = FooInfo
        #frozen_foo_1 = FooInfo(bar="bar_f1", baz="baz_f1")
        #frozen_foo_2 = FooInfo(bar="bar_f2")

        assert_eq("unnamed provider", repr(provider(fields=["f1"])))
        assert_eq("FooInfo(bar, baz)", repr(FooInfo))
        assert_eq("FooInfo(bar, baz)", repr(FooInfo2))

        simple_info_1 = SimpleInfo(value1="value1", value2=3)

        def test():
            assert_eq(FooInfo.type, "FooInfo")
            assert_eq("FooInfo(bar, baz)", repr(FooInfo))
            assert_eq("FooInfo(bar, baz)", repr(FooInfo2))

            #assert_eq("FooInfo(bar=\"bar_f1\", baz=\"baz_f1\")", repr(frozen_foo1))
            #assert_eq("bar_f1", frozen_foo1.bar)
            #assert_eq("baz_f1", frozen_foo1.baz)
            #assert_eq("FooInfo(bar=\"bar_f2\", baz=None)", repr(frozen_foo2))
            #assert_eq("bar_f2", frozen_foo2.bar)
            #assert_eq(None, frozen_foo2.baz)

            foo_1 = FooInfo(bar="bar_1", baz="baz_1")
            foo_2 = FooInfo(bar="bar_2")

            assert_eq("FooInfo(bar, baz)", repr(FooInfo))
            assert_eq("FooInfo(bar=\"bar_1\", baz=\"baz_1\")", repr(foo_1))
            assert_eq("bar_1", foo_1.bar)
            assert_eq("baz_1", foo_1.baz)
            assert_eq("FooInfo(bar=\"bar_2\", baz=None)", repr(foo_2))
            assert_eq("bar_2", foo_2.bar)
            assert_eq(None, foo_2.baz)

            assert_eq("{\"bar\":\"bar_1\",\"baz\":\"baz_1\"}", foo_1.to_json())
            assert_eq("{\"value1\":\"value1\",\"value2\":3}", simple_info_1.to_json())
            assert_eq(struct(value1="value1", value2=3).to_json(), simple_info_1.to_json())
        "#
        ))?;

        run_starlark_test_expecting_error(
            indoc!(
                r#"
        FooInfo = provider(fields=["bar", "baz"])

        def test():
            foo_1 = FooInfo(bar="bar1")
            foo_1.quz
        "#
            ),
            "Object of type `provider` has no attribute `quz`",
        );

        run_starlark_test_expecting_error(
            indoc!(
                r#"
        list = []
        list.append(provider(fields=["bar", "baz"]))
        "#
            ),
            "must be assigned to a variable",
        );

        // Make sure that frozen UserProvider instances work
        let mut tester = Tester::new()?;
        tester.add_import(
            &import("root", "provider", "def1.bzl"),
            indoc!(
                r#"
                FooInfo = provider(fields=["foo"])
                "#
            ),
        )?;
        tester.add_import(
            &import("root", "provider", "def2.bzl"),
            indoc!(
                r#"
                load("//provider:def1.bzl", "FooInfo")
                foo = FooInfo(foo="foo1")
                "#
            ),
        )?;
        tester.run_starlark_test(indoc!(
            r#"
            load("//provider:def2.bzl", "foo")
            def test():
                assert_eq('FooInfo(foo="foo1")', repr(foo))
            "#
        ))?;

        Ok(())
    }

    fn provider_collection_tester() -> SharedResult<Tester> {
        let mut tester = Tester::new()?;
        tester.set_additional_globals(Arc::new(|builder| {
            collection_creator(builder);
            artifactory(builder);
        }));
        tester.add_import(
            &import("root", "provider", "defs1.bzl"),
            indoc!(
                r#"
                    FooInfo = provider(fields=["foo"])
                    BarInfo = provider(fields=["bar"])
                    BazInfo = provider(fields=["baz"])
                    "#
            ),
        )?;
        tester.add_import(
            &import("root", "provider", "defs2.bzl"),
            indoc!(
                r#"
                    load("//provider:defs1.bzl", "FooInfo", "BarInfo", "BazInfo")
                    foo1 = FooInfo(foo="foo1")
                    foo2 = FooInfo(foo="foo2")
                    bar1 = BarInfo(bar="bar1")
                    baz1 = BazInfo(baz="baz1")
                    "#
            ),
        )?;

        Ok(tester)
    }

    #[test]
    fn provider_collection_constructs_properly() -> SharedResult<()> {
        let mut tester = provider_collection_tester()?;
        tester.run_starlark_bzl_test(indoc!(
            r#"
            load("//provider:defs1.bzl", "FooInfo", "BarInfo", "BazInfo")
            load("//provider:defs2.bzl", "foo1", "bar1")
            def test():
                col = create_collection([foo1, bar1, DefaultInfo()])
                assert_eq(None, col[BazInfo])
                assert_eq("foo1", col[FooInfo].foo)
                assert_eq("bar1", col[BarInfo].bar)
                assert_eq([], col[DefaultInfo].default_outputs)
            "#
        ))?;
        Ok(())
    }

    #[test]
    fn provider_collection_fails_to_construct_on_bad_data() -> SharedResult<()> {
        let mut tester = provider_collection_tester()?;
        let not_a_list = indoc!(
            r#"
            load("//provider:defs2.bzl", "foo1")
            def test():
                create_collection(foo1)
            "#
        );
        expect_error(
            tester.run_starlark_bzl_test(not_a_list),
            not_a_list,
            "expected a list of Provider objects",
        );

        let mut tester = provider_collection_tester()?;
        let not_a_provider = indoc!(
            r#"
            load("//provider:defs2.bzl", "foo1", "bar1")
            def test():
                create_collection([foo1, bar1, "not a provider", DefaultInfo()])
            "#
        );
        expect_error(
            tester.run_starlark_bzl_test(not_a_provider),
            not_a_provider,
            "expected a Provider object",
        );

        let mut tester = provider_collection_tester()?;
        let duplicate_provider_types = indoc!(
            r#"
            load("//provider:defs2.bzl", "foo1", "foo2", "bar1")
            def test():
                create_collection([foo1, bar1, foo2, DefaultInfo()])
            "#
        );
        expect_error(
            tester.run_starlark_bzl_test(duplicate_provider_types),
            duplicate_provider_types,
            "specified twice",
        );

        let mut tester = provider_collection_tester()?;
        let missing_default_info = indoc!(
            r#"
            load("//provider:defs2.bzl", "foo1", "bar1")
            def test():
                create_collection([foo1, bar1])
            "#
        );
        expect_error(
            tester.run_starlark_bzl_test(missing_default_info),
            missing_default_info,
            "did not receive a DefaultInfo",
        );
        Ok(())
    }

    #[test]
    fn returns_default_info() -> SharedResult<()> {
        let mut tester = provider_collection_tester()?;
        tester.run_starlark_bzl_test(indoc!(
            r#"
            artifact = source_artifact("foo", "bar.cpp")
            frozen_collection = create_collection([
                DefaultInfo(
                    sub_targets={"foo": []},
                    default_outputs=[artifact]
                )
            ])
            def test():
                di_sub_targets = get_default_info_sub_targets(frozen_collection)
                di_default_outputs = get_default_info_default_outputs(frozen_collection)
                assert_eq([], di_sub_targets["foo"][DefaultInfo].default_outputs)
                assert_eq([artifact], di_default_outputs)
            "#
        ))
    }

    #[starlark_module]
    fn dependency_creator(builder: &mut GlobalsBuilder) {
        fn create_collection<'v>(s: &str, providers: Value<'v>) -> anyhow::Result<Dependency<'v>> {
            let c = BuildContext::from_context(eval)?;
            let label = match ParsedPattern::<ProvidersPattern>::parse_precise(
                c.cell_info().cell_alias_resolver(),
                s,
            ) {
                Ok(ParsedPattern::Target(package, (target_name, providers_name))) => {
                    ProvidersLabel::new(TargetLabel::new(package, target_name), providers_name)
                        .configure(Configuration::testing_new())
                }
                _ => {
                    eprintln!("Expected a target, not {}", s);
                    panic!();
                }
            };
            let collection = heap.alloc(ProviderCollection::try_from_value(providers)?);

            Ok(Dependency::new(heap, label, collection))
        }
    }

    #[test]
    fn dependency_works() -> SharedResult<()> {
        let mut tester = Tester::new()?;
        tester.set_additional_globals(Arc::new(|x| {
            crate::interpreter::rule_defs::register_rule_defs(x);
            dependency_creator(x)
        }));
        tester.run_starlark_bzl_test(indoc!(
            r#"
            frozen = create_collection("root//foo:bar[baz]", [DefaultInfo()])
            def test():
                notfrozen = create_collection("root//foo:bar[baz]", [DefaultInfo()])
                expect = "<dependency root//foo:bar[baz] (<testing>)>"

                assert_eq(expect, repr(notfrozen))
                assert_eq({}, notfrozen[DefaultInfo].sub_targets)
                assert_eq(["baz"], notfrozen.label.sub_target)

                assert_eq(expect, repr(frozen))
                assert_eq({}, frozen[DefaultInfo].sub_targets)
                assert_eq(["baz"], frozen.label.sub_target)
            "#
        ))?;
        Ok(())
    }

    #[test]
    fn provider_collection_contains_methods_work() -> SharedResult<()> {
        let mut tester = Tester::new()?;
        tester.set_additional_globals(Arc::new(collection_creator));
        tester.add_import(
            &import("root", "providers", "defs.bzl"),
            indoc!(
                r#"
                FooInfo = provider(fields=["foo"])
                BarInfo = provider(fields=["bar"])
                "#
            ),
        )?;
        tester.run_starlark_bzl_test(indoc!(
            r#"
            load("//providers:defs.bzl", "FooInfo", "BarInfo")
            c1 = create_collection([DefaultInfo(), FooInfo(foo="f1")])
            c2 = create_collection([DefaultInfo(), FooInfo(foo="f2"), BarInfo(bar="b2")])
            def test():
                assert_eq(True, contains_provider(c1, FooInfo))
                assert_eq(False, contains_provider(c1, BarInfo))
                assert_eq(["DefaultInfo", "FooInfo", "BarInfo"], providers_list(c2))
            "#
        ))
    }

    #[test]
    fn provider_compare() -> SharedResult<()> {
        let mut tester = Tester::new()?;
        tester.add_import(
            &import("root", "providers", "defs.bzl"),
            indoc!(
                r#"
                FooInfo = provider(fields=["foo"])
                BarInfo = provider(fields=["bar"])
                "#
            ),
        )?;
        tester.run_starlark_bzl_test(indoc!(
            r#"
            load("//providers:defs.bzl", DefsFooInfo="FooInfo", DefsBarInfo="BarInfo")
            FooInfo = provider(fields=["foo"])
            BarInfo = provider(fields=["bar"])
            ComplexInfo = provider(fields=["foo", "bar"])
            def assert_less(left, right):
                if not left < right:
                    fail("expected `%s < %s`" % (left, right))
                if right < left:
                    fail("expected `!(%s > %s)`" % (right, left))

            def test():
                assert_less(DefsFooInfo(foo=1), FooInfo(foo=1))
                assert_less(DefsBarInfo(bar=1), BarInfo(bar=1))
                assert_less(BarInfo(bar=1), FooInfo(foo=1))
                assert_less(FooInfo(foo=1), FooInfo(foo=2))
                # ensure that ordering of fields when creating the provider doesn't affect the ordering.
                assert_less(ComplexInfo(foo=1, bar=1), ComplexInfo(bar=2, foo=1))
            "#
        ))
    }
}
