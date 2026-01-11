/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::convert::Infallible;
use std::fmt;
use std::fmt::Display;
use std::mem;
use std::sync::Arc;

use allocative::Allocative;
use buck2_core::provider::id::ProviderId;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::NonDefaultProvidersName;
use buck2_core::provider::label::ProviderName;
use buck2_core::provider::label::ProvidersName;
use buck2_error::BuckErrorContext;
use buck2_interpreter::starlark_promise::StarlarkPromise;
use buck2_interpreter::types::provider::callable::ValueAsProviderCallableLike;
use display_container::fmt_container;
use dupe::Dupe;
use either::Either;
use serde::Serialize;
use serde::Serializer;
use starlark::any::ProvidesStaticType;
use starlark::coerce::Coerce;
use starlark::coerce::coerce;
use starlark::collections::SmallMap;
use starlark::environment::GlobalsBuilder;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::typing::Ty;
use starlark::values::AllocFrozenValue;
use starlark::values::AllocStaticSimple;
use starlark::values::AllocValue;
use starlark::values::Freeze;
use starlark::values::FreezeResult;
use starlark::values::Freezer;
use starlark::values::FrozenHeap;
use starlark::values::FrozenHeapRef;
use starlark::values::FrozenRef;
use starlark::values::FrozenValue;
use starlark::values::FrozenValueTyped;
use starlark::values::Heap;
use starlark::values::OwnedFrozenValue;
use starlark::values::OwnedFrozenValueTyped;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::Tracer;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::ValueLifetimeless;
use starlark::values::ValueLike;
use starlark::values::ValueOfUnchecked;
use starlark::values::list::ListRef;
use starlark::values::none::NoneOr;
use starlark::values::starlark_value;
use starlark::values::starlark_value_as_type::StarlarkValueAsType;
use starlark::values::type_repr::StarlarkTypeRepr;

use crate::interpreter::rule_defs::provider::DefaultInfo;
use crate::interpreter::rule_defs::provider::DefaultInfoCallable;
use crate::interpreter::rule_defs::provider::FrozenBuiltinProviderLike;
use crate::interpreter::rule_defs::provider::FrozenDefaultInfo;
use crate::interpreter::rule_defs::provider::ValueAsProviderLike;
use crate::interpreter::rule_defs::provider::ty::abstract_provider::AbstractProvider;

fn format_provider_keys_for_error(keys: &[String]) -> String {
    format!(
        "[{}]",
        keys.iter()
            .map(|k| format!("`{k}`"))
            .collect::<Vec<_>>()
            .join(", ")
    )
}

#[derive(Debug, buck2_error::Error)]
#[buck2(input)]
enum ProviderCollectionError {
    #[error("expected a list of Provider objects, got {repr}")]
    CollectionNotAList { repr: String },
    #[error("expected a Provider object, got {repr}")]
    CollectionElementNotAProvider { repr: String },
    #[error("provider of type `{provider_name}` specified twice ({original_repr} and {new_repr})")]
    CollectionSpecifiedProviderTwice {
        provider_name: String,
        original_repr: String,
        new_repr: String,
    },
    #[error("collection {repr} did not receive a `DefaultInfo` provider")]
    CollectionMissingDefaultInfo { repr: String },
    #[error(
        "requested sub target named `{0}` of target `{1}` is not available. Available subtargets are: `{2:?}`"
    )]
    RequestedInvalidSubTarget(ProviderName, ConfiguredProvidersLabel, Vec<String>),
    #[error(
        "Cannot handle flavor `{flavor}` on target `{target}`. Most flavors are unsupported in Buck2."
    )]
    UnknownFlavors { target: String, flavor: String },
    #[error(
        "provider collection operation {0} parameter type must be a provider type \
        but not and instance of provider (for example, `RunInfo` or user defined provider type), \
        got `{1}`"
    )]
    AtTypeNotProvider(GetOp, &'static str),
    #[error(
        "provider collection does not have a key `{0}`, available keys are: {}",
        format_provider_keys_for_error(_1)
    )]
    AtNotFound(String, Vec<String>),
}

#[derive(Debug, ProvidesStaticType, Allocative)]
#[repr(C)]
pub struct ProviderCollectionGen<V: ValueLifetimeless> {
    pub(crate) providers: SmallMap<Arc<ProviderId>, V>,
}

pub type ProviderCollection<'v> = ProviderCollectionGen<Value<'v>>;
pub type FrozenProviderCollection = ProviderCollectionGen<FrozenValue>;

// Can't derive this since no instance for Arc
unsafe impl<From: Coerce<To> + ValueLifetimeless, To: ValueLifetimeless>
    Coerce<ProviderCollectionGen<To>> for ProviderCollectionGen<From>
{
}

fn empty_provider_collection_value() -> FrozenValueTyped<'static, FrozenProviderCollection> {
    static EMPTY: AllocStaticSimple<FrozenProviderCollection> =
        AllocStaticSimple::alloc(FrozenProviderCollection {
            providers: SmallMap::new(),
        });
    EMPTY.unpack()
}

impl<'v> AllocValue<'v> for ProviderCollectionGen<Value<'v>> {
    fn alloc_value(self, heap: Heap<'v>) -> Value<'v> {
        if self.providers.is_empty() {
            // Provider collection is immutable, so it's OK to return frozen value here.
            empty_provider_collection_value().to_value()
        } else {
            heap.alloc_complex(self)
        }
    }
}

impl AllocFrozenValue for ProviderCollectionGen<FrozenValue> {
    fn alloc_frozen_value(self, heap: &FrozenHeap) -> FrozenValue {
        if self.providers.is_empty() {
            empty_provider_collection_value().to_frozen_value()
        } else {
            heap.alloc_simple(self)
        }
    }
}

impl<'v> ProviderCollection<'v> {
    #[inline]
    pub fn from_value(x: Value<'v>) -> Option<&'v Self> {
        if let Some(x) = x.unpack_frozen() {
            ValueLike::downcast_ref::<FrozenProviderCollection>(x).map(coerce)
        } else {
            ValueLike::downcast_ref::<ProviderCollection<'v>>(x)
        }
    }
}

impl<'v> StarlarkTypeRepr for &'v ProviderCollection<'v> {
    type Canonical = <ProviderCollection<'v> as StarlarkValue<'v>>::Canonical;

    fn starlark_type_repr() -> Ty {
        <Self::Canonical as StarlarkValue>::get_type_starlark_repr()
    }
}

impl<'v> UnpackValue<'v> for &'v ProviderCollection<'v> {
    type Error = Infallible;

    fn unpack_value_impl(value: Value<'v>) -> Result<Option<Self>, Self::Error> {
        Ok(ProviderCollection::from_value(value))
    }
}

impl<V: ValueLifetimeless> Display for ProviderCollectionGen<V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt_container(
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

/// Provider collection access operator.
#[derive(derive_more::Display, Debug)]
enum GetOp {
    #[display("[]")]
    At,
    #[display("in")]
    In,
    #[display(".get")]
    Get,
}

impl<'v, V: ValueLike<'v>> ProviderCollectionGen<V> {
    /// Create most of the collection but don't do final assembly, or validate DefaultInfo here.
    /// This is an internal detail
    fn try_from_value_impl(
        mut value: Value<'v>,
    ) -> buck2_error::Result<SmallMap<Arc<ProviderId>, Value<'v>>> {
        // Sometimes we might have a resolved promise here, in which case see through that
        value = StarlarkPromise::get_recursive(value);

        let list = match ListRef::from_value(value) {
            Some(v) => v,
            None => {
                return Err(ProviderCollectionError::CollectionNotAList {
                    repr: value.to_repr(),
                }
                .into());
            }
        };

        let mut providers = SmallMap::with_capacity(list.len());
        for value in list.iter() {
            match ValueAsProviderLike::unpack_value(value)? {
                Some(provider) => {
                    if let Some(existing_value) = providers.insert(provider.0.id().dupe(), value) {
                        return Err(ProviderCollectionError::CollectionSpecifiedProviderTwice {
                            provider_name: provider.0.id().name.clone(),
                            original_repr: existing_value.to_repr(),
                            new_repr: value.to_repr(),
                        }
                        .into());
                    };
                }
                None => {
                    return Err(ProviderCollectionError::CollectionElementNotAProvider {
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
    pub fn try_from_value(value: Value<'v>) -> buck2_error::Result<ProviderCollection<'v>> {
        let providers = Self::try_from_value_impl(value)?;
        if !providers.contains_key(DefaultInfoCallable::provider_id()) {
            return Err(ProviderCollectionError::CollectionMissingDefaultInfo {
                repr: value.to_repr(),
            }
            .into());
        }

        Ok(ProviderCollection::<'v> { providers })
    }

    /// Takes a value, e.g. a value passed to `DefaultInfo(subtargets)`, and builds a `ProviderCollection` from it.
    ///
    /// An error is returned if:
    ///  - `value` is not a list
    ///  - Two instances of the same provider are provided
    ///
    /// Should only be used for subtargets, where an empty `DefaultInfo` can be inferred.
    pub fn try_from_value_subtarget(
        value: Value<'v>,
        heap: Heap<'v>,
    ) -> buck2_error::Result<ProviderCollection<'v>> {
        let mut providers = Self::try_from_value_impl(value)?;

        if !providers.contains_key(DefaultInfoCallable::provider_id()) {
            providers.insert(
                DefaultInfoCallable::provider_id().dupe(),
                heap.alloc(DefaultInfo::empty(heap)),
            );
        }
        Ok(ProviderCollection::<'v> { providers })
    }

    /// Takes a value, e.g. a return from a `dynamic_output` function, and builds a `ProviderCollection` from it.
    ///
    /// An error is returned if:
    ///  - `value` is not a list
    ///  - Two instances of the same provider are provided
    pub fn try_from_value_dynamic_output(
        value: Value<'v>,
    ) -> buck2_error::Result<ProviderCollection<'v>> {
        let providers = Self::try_from_value_impl(value)?;

        Ok(ProviderCollection::<'v> { providers })
    }

    /// Common implementation of `[]`, `in`, and `.get`.
    fn get_impl(
        &self,
        index: Value<'v>,
        op: GetOp,
    ) -> buck2_error::Result<Either<Value<'v>, Arc<ProviderId>>> {
        match index.as_provider_callable() {
            Some(callable) => {
                let provider_id = callable.id()?.dupe();
                match self.providers.get(&provider_id) {
                    Some(v) => Ok(Either::Left(v.to_value())),
                    None => Ok(Either::Right(provider_id)),
                }
            }
            None => Err(ProviderCollectionError::AtTypeNotProvider(op, index.get_type()).into()),
        }
    }

    /// `.get` function implementation.
    pub(crate) fn get(
        &self,
        index: Value<'v>,
    ) -> buck2_error::Result<NoneOr<ValueOfUnchecked<'v, AbstractProvider>>> {
        match self.get_impl(index, GetOp::Get)? {
            Either::Left(v) => Ok(NoneOr::Other(ValueOfUnchecked::new(v))),
            Either::Right(_) => Ok(NoneOr::None),
        }
    }
}

impl FrozenProviderCollection {
    pub fn testing_new_default(
        heap: &FrozenHeap,
    ) -> FrozenValueTyped<'static, FrozenProviderCollection> {
        FrozenValueTyped::new_err(heap.alloc(FrozenProviderCollection {
            providers: SmallMap::from_iter([(
                DefaultInfoCallable::provider_id().dupe(),
                FrozenDefaultInfo::testing_empty(heap).to_frozen_value(),
            )]),
        }))
        .unwrap()
    }
}

/// Holds a set of providers.
///
/// Accessed by indexing with a provider type, e.g.
///
/// ```ignore
/// FooInfo = provider(fields=["bar"])
/// ....
/// collection.get(FooInfo) # None if absent, a FooInfo instance if present
/// ```
#[starlark_module]
fn provider_collection_methods(builder: &mut MethodsBuilder) {
    fn get<'v>(
        this: &ProviderCollection<'v>,
        index: Value<'v>,
    ) -> starlark::Result<NoneOr<ValueOfUnchecked<'v, AbstractProvider>>> {
        Ok(this.get(index)?)
    }
}

#[starlark_value(type = "ProviderCollection")]
impl<'v, V: ValueLike<'v>> StarlarkValue<'v> for ProviderCollectionGen<V>
where
    Self: ProvidesStaticType<'v>,
{
    fn at(&self, index: Value<'v>, _heap: Heap<'v>) -> starlark::Result<Value<'v>> {
        match self.get_impl(index, GetOp::At)? {
            Either::Left(v) => Ok(v),
            Either::Right(provider_id) => Err(buck2_error::Error::from(
                ProviderCollectionError::AtNotFound(
                    provider_id.name.clone(),
                    self.providers.keys().map(|k| k.name.clone()).collect(),
                ),
            )
            .into()),
        }
    }

    fn is_in(&self, other: Value<'v>) -> starlark::Result<bool> {
        Ok(self.get_impl(other, GetOp::In)?.is_left())
    }

    fn get_methods() -> Option<&'static Methods>
    where
        Self: Sized,
    {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(provider_collection_methods)
    }
}

unsafe impl<'v> Trace<'v> for ProviderCollection<'v> {
    fn trace(&mut self, tracer: &Tracer<'v>) {
        self.providers.values_mut().for_each(|v| tracer.trace(v))
    }
}

impl<'v> Freeze for ProviderCollection<'v> {
    type Frozen = FrozenProviderCollection;
    fn freeze(self, freezer: &Freezer) -> FreezeResult<Self::Frozen> {
        let providers = self
            .providers
            .into_iter()
            .map(|(k, v)| Ok((k, freezer.freeze(v)?)))
            .collect::<FreezeResult<_>>()?;
        Ok(FrozenProviderCollection { providers })
    }
}

impl FrozenProviderCollection {
    pub fn default_info<'a>(&'a self) -> buck2_error::Result<FrozenRef<'a, FrozenDefaultInfo>> {
        self.builtin_provider().internal_error(
            "DefaultInfo should always be set for providers returned from rule function",
        )
    }

    pub fn contains_provider(&self, provider_id: &ProviderId) -> bool {
        self.providers.contains_key(provider_id)
    }

    pub fn builtin_provider<'a, T: FrozenBuiltinProviderLike>(
        &'a self,
    ) -> Option<FrozenRef<'a, T>> {
        self.builtin_provider_value::<T>()
            .map(|v| v.to_frozen_value().downcast_frozen_ref().unwrap())
    }

    pub fn builtin_provider_value<'a, T: FrozenBuiltinProviderLike>(
        &'a self,
    ) -> Option<FrozenValueTyped<'a, T>> {
        let provider: FrozenValue = *self.providers.get(T::builtin_provider_id())?;
        Some(FrozenValueTyped::new(provider).expect("Incorrect provider type"))
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
#[derive(Debug, Clone, Dupe, Allocative)]
pub struct FrozenProviderCollectionValue {
    #[allocative(skip)] // TODO(nga): do not skip.
    pub value: OwnedFrozenValueTyped<FrozenProviderCollection>,
}

#[derive(Clone, Copy, Dupe)]
pub struct FrozenProviderCollectionValueRef<'f> {
    /// Heap that owns the value.
    heap: &'f FrozenHeapRef,
    value: FrozenValueTyped<'f, FrozenProviderCollection>,
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

    pub fn try_from_value(value: OwnedFrozenValue) -> buck2_error::Result<Self> {
        Ok(Self {
            value: value.downcast_starlark()?,
        })
    }

    pub fn value(&self) -> &OwnedFrozenValueTyped<FrozenProviderCollection> {
        &self.value
    }

    pub fn provider_collection(&self) -> &FrozenProviderCollection {
        self.value.as_ref()
    }

    pub fn as_ref(&self) -> FrozenProviderCollectionValueRef<'_> {
        FrozenProviderCollectionValueRef {
            heap: self.value.owner(),
            value: unsafe { self.value.value_typed() },
        }
    }

    pub fn add_heap_ref<'v>(
        &self,
        heap: &'v FrozenHeap,
    ) -> FrozenValueTyped<'v, FrozenProviderCollection> {
        self.as_ref().add_heap_ref(heap)
    }

    pub fn add_heap_ref_static(
        &self,
        heap: &FrozenHeap,
    ) -> FrozenValueTyped<'static, FrozenProviderCollection> {
        unsafe {
            mem::transmute::<
                FrozenValueTyped<'_, FrozenProviderCollection>,
                FrozenValueTyped<'_, FrozenProviderCollection>,
            >(self.add_heap_ref(heap))
        }
    }

    pub fn lookup_inner<'f>(
        &'f self,
        label: &ConfiguredProvidersLabel,
    ) -> buck2_error::Result<FrozenProviderCollectionValueRef<'f>> {
        self.as_ref().lookup_inner(label)
    }
}

impl<'f> FrozenProviderCollectionValueRef<'f> {
    pub unsafe fn new(
        heap: &'f FrozenHeapRef,
        value: FrozenValueTyped<'f, FrozenProviderCollection>,
    ) -> Self {
        FrozenProviderCollectionValueRef { heap, value }
    }

    pub fn value(self) -> FrozenValueTyped<'f, FrozenProviderCollection> {
        self.value
    }

    pub fn owner(self) -> &'f FrozenHeapRef {
        self.heap
    }

    pub fn to_owned(self) -> FrozenProviderCollectionValue {
        unsafe {
            // Cast lifetime.
            let value = mem::transmute::<
                FrozenValueTyped<FrozenProviderCollection>,
                FrozenValueTyped<FrozenProviderCollection>,
            >(self.value);
            FrozenProviderCollectionValue {
                value: OwnedFrozenValueTyped::new(self.heap.dupe(), value),
            }
        }
    }

    pub fn add_heap_ref<'v>(
        self,
        heap: &'v FrozenHeap,
    ) -> FrozenValueTyped<'v, FrozenProviderCollection> {
        heap.add_reference(self.heap);
        unsafe {
            mem::transmute::<
                FrozenValueTyped<'_, FrozenProviderCollection>,
                FrozenValueTyped<'_, FrozenProviderCollection>,
            >(self.value)
        }
    }

    pub fn lookup_inner(
        self,
        label: &ConfiguredProvidersLabel,
    ) -> buck2_error::Result<FrozenProviderCollectionValueRef<'f>> {
        match label.name() {
            ProvidersName::Default => buck2_error::Ok(self),
            ProvidersName::NonDefault(flavor) => match flavor.as_ref() {
                NonDefaultProvidersName::Named(provider_names) => {
                    let mut collection_value = self.value;

                    for provider_name in &**provider_names {
                        let maybe_di = collection_value
                            .default_info()?
                            .get_sub_target_providers(provider_name.as_str());

                        match maybe_di {
                            // The inner values should all be frozen if in a frozen provider collection
                            Some(inner) => {
                                collection_value = inner;
                            }
                            None => {
                                return Err(ProviderCollectionError::RequestedInvalidSubTarget(
                                    provider_name.clone(),
                                    label.dupe(),
                                    collection_value
                                        .default_info()?
                                        .sub_targets()
                                        .keys()
                                        .map(|s| (*s).to_owned())
                                        .collect(),
                                )
                                .into());
                            }
                        }
                    }
                    Ok(FrozenProviderCollectionValueRef {
                        heap: self.heap,
                        value: collection_value,
                    })
                }
                NonDefaultProvidersName::UnrecognizedFlavor(flavor) => {
                    Err(ProviderCollectionError::UnknownFlavors {
                        target: label.unconfigured().to_string(),
                        flavor: (**flavor).to_owned(),
                    }
                    .into())
                }
            },
        }
    }
}

pub mod tester {
    use buck2_interpreter::types::provider::callable::ValueAsProviderCallableLike;
    use dupe::Dupe;
    use starlark::environment::GlobalsBuilder;
    use starlark::values::Value;
    use starlark::values::ValueLike;

    use crate::interpreter::rule_defs::provider::ProviderCollection;
    use crate::interpreter::rule_defs::provider::collection::FrozenProviderCollection;

    #[starlark_module]
    pub fn collection_creator(builder: &mut GlobalsBuilder) {
        fn create_collection<'v>(value: Value<'v>) -> starlark::Result<ProviderCollection<'v>> {
            Ok(ProviderCollection::try_from_value(value)?)
        }

        fn get_default_info_default_outputs<'v>(value: Value<'v>) -> starlark::Result<Value<'v>> {
            let frozen = value
                .unpack_frozen()
                .expect("a frozen value to fetch DefaultInfo");
            let collection = frozen
                .downcast_ref::<FrozenProviderCollection>()
                .ok_or_else(|| {
                    buck2_error::buck2_error!(
                        buck2_error::ErrorTag::StarlarkError,
                        "{:?} was not a FrozenProviderCollection",
                        value
                    )
                })?;

            let ret = collection.default_info()?.default_outputs_raw().to_value();
            Ok(ret)
        }

        fn get_default_info_sub_targets<'v>(value: Value<'v>) -> starlark::Result<Value<'v>> {
            let frozen = value
                .unpack_frozen()
                .expect("a frozen value to fetch DefaultInfo");
            let collection = frozen
                .downcast_ref::<FrozenProviderCollection>()
                .ok_or_else(|| {
                    buck2_error::buck2_error!(
                        buck2_error::ErrorTag::StarlarkError,
                        "{:?} was not a FrozenProviderCollection",
                        value
                    )
                })?;

            let ret = collection.default_info()?.sub_targets_raw().to_value();
            Ok(ret)
        }

        fn contains_provider<'v>(
            collection: Value<'v>,
            provider: Value<'v>,
        ) -> starlark::Result<bool> {
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
                    buck2_error::buck2_error!(
                        buck2_error::ErrorTag::StarlarkError,
                        "{:?} was not a FrozenProviderCollection",
                        collection
                    )
                })?
                .contains_provider(&id);

            Ok(res)
        }

        fn providers_list<'v>(collection: Value<'v>) -> starlark::Result<Vec<String>> {
            Ok(collection
                .unpack_frozen()
                .expect("a frozen value")
                .downcast_ref::<FrozenProviderCollection>()
                .ok_or_else(|| {
                    buck2_error::buck2_error!(
                        buck2_error::ErrorTag::StarlarkError,
                        "{:?} was not a FrozenProviderCollection",
                        collection
                    )
                })?
                .provider_names())
        }
    }
}

#[starlark_module]
pub(crate) fn register_provider_collection(globals: &mut GlobalsBuilder) {
    const ProviderCollection: StarlarkValueAsType<ProviderCollectionGen<Value>> =
        StarlarkValueAsType::new();
}
