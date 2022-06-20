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
    fmt::{self, Debug, Display},
    hash::Hash,
    marker::PhantomData,
    sync::Arc,
};

use buck2_core::{cells::paths::CellPath, provider::ConfiguredProvidersLabel};
use buck2_interpreter::types::label::Label;
use gazebo::{any::ProvidesStaticType, coerce::Coerce};
use starlark::{
    environment::{Methods, MethodsBuilder, MethodsStatic},
    starlark_type,
    values::{Freeze, Heap, NoSerialize, StarlarkValue, Trace, Value, ValueLike},
};

use crate::interpreter::rule_defs::provider::{
    collection::ProviderCollection,
    default_info::{DefaultInfo, DefaultInfoCallable, FrozenDefaultInfo},
    registration::ProviderRegistration,
    user::{FrozenUserProvider, UserProvider},
};

pub mod callable;
pub mod collection;
pub mod configuration_info;
pub mod constraint_setting_info;
pub mod constraint_value_info;
pub mod default_info;
pub mod execution_platform_info;
pub mod execution_platform_registration_info;
pub mod external_runner_test_info;
pub mod install_info;
pub mod platform_info;
pub mod registration;
pub mod run_info;
pub mod template_placeholder_info;
pub(crate) mod user;

#[derive(Debug, thiserror::Error)]
enum ProviderError {
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

pub(crate) trait ProviderLike<'v>: Debug {
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

/// Wraps a dependency's `ProvidersLabel` and the result of analysis together for users' rule implementation functions
///
/// From Starlark, the label is accessible with `.label`, and providers from the underlying
/// `ProviderCollection` are available via `[]` (`get()`)
#[derive(Debug, Trace, Coerce, Freeze, ProvidesStaticType, NoSerialize)]
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

impl<'v, V: ValueLike<'v> + 'v> StarlarkValue<'v> for DependencyGen<V>
where
    Self: ProvidesStaticType,
{
    starlark_type!("dependency");

    fn get_methods() -> Option<&'static Methods> {
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

#[cfg(test)]
pub mod testing {
    use buck2_core::cells::paths::CellPath;
    use starlark::environment::{GlobalsBuilder, Module};

    use crate::{
        attrs::testing,
        interpreter::rule_defs::{
            artifact::testing::artifactory,
            provider::{
                collection::FrozenProviderCollectionValue,
                registration::register_builtin_providers, ProviderId,
            },
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
    use gazebo::{any::ProvidesStaticType, coerce::Coerce};
    use indoc::indoc;
    use starlark::{
        environment::GlobalsBuilder,
        eval::Evaluator,
        values::{Freeze, Trace, Value},
    };

    use crate::interpreter::{
        rule_defs::provider::{Dependency, ProviderCollection},
        testing::{import, run_starlark_test_expecting_error, Tester},
    };

    #[internal_provider(simple_info_creator)]
    #[derive(Clone, Debug, Trace, Coerce, Freeze, ProvidesStaticType)]
    #[repr(C)]
    pub struct SimpleInfoGen<V> {
        value1: V,
        value2: V,
    }

    #[starlark_module]
    fn simple_info_creator(globals: &mut GlobalsBuilder) {
        fn ConstraintSettingInfo<'v>(
            value1: Value<'v>,
            value2: Value<'v>,
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
            assert_eq(json.encode(struct(value1="value1", value2=3)), simple_info_1.to_json())
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

    #[starlark_module]
    fn dependency_creator(builder: &mut GlobalsBuilder) {
        fn create_collection<'v>(
            s: &str,
            providers: Value<'v>,
            eval: &mut Evaluator<'v, '_>,
        ) -> anyhow::Result<Dependency<'v>> {
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
            let collection = eval
                .heap()
                .alloc(ProviderCollection::try_from_value(providers)?);

            Ok(Dependency::new(eval.heap(), label, collection))
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
