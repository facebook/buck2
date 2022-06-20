/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{fmt, fmt::Display, sync::Arc};

use gazebo::{any::ProvidesStaticType, coerce::Coerce, dupe::Dupe};
use serde::{Serialize, Serializer};
use starlark::{
    collections::SmallMap,
    values::{
        display::display_container, list::List, Freeze, Freezer, FrozenRef, FrozenValue, Heap,
        OwnedFrozenValue, OwnedFrozenValueTyped, StarlarkValue, Trace, Tracer, Value, ValueError,
        ValueLike,
    },
};

use crate::interpreter::rule_defs::provider::{
    callable::ValueAsProviderCallableLike,
    id::{ProviderId, ProviderIdWithType},
    DefaultInfo, DefaultInfoCallable, FrozenDefaultInfo, ProviderError, ValueAsProviderLike,
};

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
#[derive(Debug, ProvidesStaticType)]
#[repr(C)]
pub struct ProviderCollectionGen<V> {
    pub(crate) providers: SmallMap<Arc<ProviderId>, V>,
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

impl<'v, V: ValueLike<'v> + 'v> StarlarkValue<'v> for ProviderCollectionGen<V>
where
    Self: ProvidesStaticType,
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
            value: value.downcast().map_err(|value| {
                anyhow::anyhow!("{:?} was not a FrozenProviderCollection", value)
            })?,
        })
    }

    pub fn value(&self) -> &OwnedFrozenValueTyped<FrozenProviderCollection> {
        &self.value
    }

    pub fn provider_collection(&self) -> &FrozenProviderCollection {
        self.value.as_ref()
    }
}

#[cfg(test)]
pub(crate) mod tester {
    use gazebo::dupe::Dupe;
    use starlark::{
        environment::GlobalsBuilder,
        values::{Value, ValueLike},
    };

    use crate::interpreter::rule_defs::provider::{
        callable::ValueAsProviderCallableLike, collection::FrozenProviderCollection,
        ProviderCollection,
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
mod tests {
    use std::sync::Arc;

    use buck2_core::result::SharedResult;
    use indoc::indoc;

    use crate::interpreter::{
        rule_defs::{
            artifact::testing::artifactory, provider::collection::tester::collection_creator,
        },
        testing::{expect_error, import, Tester},
    };

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
}
