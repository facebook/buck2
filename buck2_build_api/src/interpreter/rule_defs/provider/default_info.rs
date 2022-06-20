/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{fmt::Debug, ptr};

use anyhow::Context;
use buck2_build_api_derive::internal_provider;
use gazebo::{
    any::ProvidesStaticType,
    coerce::{coerce, Coerce},
};
use starlark::{
    collections::SmallMap,
    environment::GlobalsBuilder,
    eval::Evaluator,
    values::{
        dict::{Dict, FrozenDict},
        list::{FrozenList, List},
        Freeze, FrozenRef, FrozenValue, FrozenValueTyped, Trace, Value, ValueError, ValueLike,
    },
};

use crate::{
    actions::artifact::Artifact,
    artifact_groups::ArtifactGroup,
    interpreter::rule_defs::{
        artifact::{StarlarkArtifact, StarlarkArtifactLike, ValueAsArtifactLike},
        cmd_args::{CommandLineArgLike, SimpleCommandLineArtifactVisitor, ValueAsCommandLineLike},
        provider::{collection::FrozenProviderCollection, ProviderCollection},
    },
};

/// A provider that all rules' implementations must return
///
/// Fields:
///  - sub_targets: A mapping of names to `ProviderCollection`s. The keys are used when
///             resolving the `ProviderName` portion of a `ProvidersLabel`. These collections
///             can contain, and actually /must/ contain a `DefaultInfo` provider. However,
///             nested label syntax is not supported. That is, `cell//foo:bar[baz]` is valid,
///             `cell//foo:bar[baz][quz]` is not.
///  - default_outputs: A list of `Artifact`s that are built by default if this rule is requested
///                     explicitly, or depended on as as a "source".
///  - other_outputs: A list of `ArtifactTraversable`. The underlying `Artifact`s they define will
///                   be built by default if this rule is requested, but _not_ when it's depended
///                   on as as a "source". `ArtifactTraversable` can be an `Artifact` (which yields
///                   itself), or `cmd_args`, which expand to all their inputs.
///
/// In many simple cases, this can be inferred for the user.
///
/// Example of a rule's implementation function and how these fields are used by the framework:
///
/// ```starlark
/// # //foo_binary.bzl
/// def impl(ctx):
///     ctx.action.run([ctx.attr._cc[RunInfo], "-o", ctx.attr.out.as_output()] + ctx.attr.srcs)
///     ctx.action.run([
///         ctx.attr._strip[RunInfo],
///         "--binary",
///         ctx.attr.out,
///         "--stripped-out",
///         ctx.attr.stripped.as_output(),
///         "--debug-symbols-out",
///         ctx.attr.debug_info.as_output(),
///     ])
///     return [
///         DefaultInfo(
///             sub_targets = {
///                 "stripped": [
///                     DefaultInfo(default_outputs = [ctx.attr.stripped, ctx.attr.debug_info]),
///                 ],
///             },
///             default_outputs = [ctx.attr.out],
///     ]
///
/// foo_binary = rule(
///     implementation=impl,
///     attrs={
///         "srcs": attr.list(attr.source()),
///         "out": attr.output(),
///         "stripped": attr.output(),
///         "debug_info": attr.output(),
///         "_cc": attr.dep(default="//tools:cc", providers=[RunInfo]),
///         "_strip_script": attr.dep(default="//tools:strip", providers=[RunInfo])
/// )
///
/// def foo_binary_wrapper(name, srcs):
///     foo_binary(
///         name = name,
///         srcs = src,
///         out = name,
///         stripped = name + ".stripped",
///         debug_info = name + ".debug_info",
///     )
///
/// # //subdir/BUCK
/// load("//:foo_binary.bzl", "foo_binary_wrapper")
///
/// genrule(name = "gen_stuff", ...., default_outs = ["foo.cpp"])
///
/// # ":gen_stuff" pulls the default_outputs for //subdir:gen_stuff
/// foo_binary_wrapper(name = "foo", srcs = glob(["*.cpp"]) + [":gen_stuff"])
///
/// # Builds just 'foo' binary. The strip command is never invoked.
/// $ buck build //subdir:foo
///
/// # builds the 'foo' binary, because it is needed by the 'strip' command. Ensures that
/// # both the stripped binary and the debug symbols are built.
/// $ buck build //subdir:foo[stripped]
/// ```
#[internal_provider(default_info_creator)]
#[derive(Clone, Debug, Freeze, Trace, Coerce, ProvidesStaticType)]
#[freeze(validator = check_max_one_list, bounds = "V: ValueLike<'freeze>")]
#[repr(C)]
pub struct DefaultInfoGen<V> {
    // Dict[str, ProviderCollection]
    sub_targets: V,
    // List[Artifact]
    default_outputs: V,
    // List[Artifact]
    other_outputs: V,
}

fn check_max_one_list<'v, V>(info: &DefaultInfoGen<V>) -> anyhow::Result<()>
where
    V: ValueLike<'v>,
{
    let default_output_list = List::from_value(info.default_outputs.to_value())
        .ok_or_else(|| anyhow::anyhow!("default_output should be a list"))?; // todo(bobyf) support single item
    if default_output_list.len() > 1 {
        tracing::info!("DefaultInfo.default_output should only have a maximum of 1 item.");
        // TODO use soft_error when landed
        // TODO error rather than soft warning
        // return Err(anyhow::anyhow!(
        //     "DefaultInfo.default_output can only have a maximum of 1 item."
        // ));
    }

    Ok(())
}

impl FrozenDefaultInfo {
    pub fn get_sub_target_providers(
        &self,
        name: &str,
    ) -> Option<FrozenValueTyped<'static, FrozenProviderCollection>> {
        FrozenDict::from_frozen_value(&self.sub_targets)
            .expect("sub_targets should be a dict-like object")
            .get_str(name)
            .map(|v| {
                FrozenValueTyped::new(v).expect(
                    "Values inside of a frozen provider should be frozen provider collection",
                )
            })
    }

    pub fn default_outputs<'a>(&'a self) -> Vec<FrozenRef<'static, StarlarkArtifact>> {
        FrozenList::from_frozen_value(&self.default_outputs)
            .expect("Should be list of artifacts")
            .iter()
            .map(|v| {
                v.unpack_frozen()
                    .expect("should be frozen")
                    .downcast_frozen_ref::<StarlarkArtifact>()
                    .expect("Should be list of artifacts")
            })
            .collect()
    }

    pub fn default_outputs_raw(&self) -> FrozenValue {
        self.default_outputs
    }

    pub fn sub_targets(&self) -> SmallMap<&str, FrozenRef<'static, FrozenProviderCollection>> {
        FrozenDict::from_frozen_value(&self.sub_targets)
            .expect("sub_targets should be a dict-like object")
            .iter()
            .map(|(k, v)| {
                (
                    k.to_value()
                        .unpack_str()
                        .expect("sub_targets should have string keys"),
                    v.downcast_frozen_ref::<FrozenProviderCollection>().expect(
                        "Values inside of a frozen provider should be frozen provider collection",
                    ),
                )
            })
            .collect()
    }

    pub fn sub_targets_raw(&self) -> FrozenValue {
        self.sub_targets
    }

    pub fn for_each_default_output(
        &self,
        processor: &mut dyn FnMut(Artifact) -> anyhow::Result<()>,
    ) -> anyhow::Result<()> {
        self.for_each_in_list(self.default_outputs, |value| {
            processor(
                value
                    .as_artifact()
                    .ok_or_else(|| anyhow::anyhow!("not an artifact"))?
                    .get_bound()?,
            )
        })
    }

    pub fn for_each_other_output(
        &self,
        processor: &mut dyn FnMut(ArtifactGroup) -> anyhow::Result<()>,
    ) -> anyhow::Result<()> {
        self.for_each_in_list(self.other_outputs, |value| {
            value
                .as_artifact_traversable()
                .with_context(|| format!("Expected artifact traversable, got: {:?}", value))?
                .traverse(processor)
        })
    }

    pub fn for_each_output(
        &self,
        processor: &mut dyn FnMut(ArtifactGroup) -> anyhow::Result<()>,
    ) -> anyhow::Result<()> {
        self.for_each_default_output(&mut |a| processor(ArtifactGroup::Artifact(a)))?;
        self.for_each_other_output(processor)
    }

    fn for_each_in_list(
        &self,
        value: FrozenValue,
        mut processor: impl FnMut(Value) -> anyhow::Result<()>,
    ) -> anyhow::Result<()> {
        let outputs_list = FrozenList::from_frozen_value(&value)
            .unwrap_or_else(|| panic!("expected list, got `{:?}` from info `{:?}`", value, self));

        for value in outputs_list.iter() {
            processor(value)?;
        }

        Ok(())
    }
}

impl PartialEq for FrozenDefaultInfo {
    // frozen default infos can be compared by ptr for a simple equality
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(self, other)
    }
}

trait ArtifactTraversable {
    fn traverse(
        &self,
        processor: &mut dyn FnMut(ArtifactGroup) -> anyhow::Result<()>,
    ) -> anyhow::Result<()>;
}

// TODO: This is a hack. We need a way to express "the inputs of that other thing", but at the
// moment we don't have one, so we allow adding a command line (which is often the input container
// we care about) as an "other" output on DefaultInfo. We could use a better abstraction for this.
impl ArtifactTraversable for &dyn CommandLineArgLike {
    fn traverse(
        &self,
        processor: &mut dyn FnMut(ArtifactGroup) -> anyhow::Result<()>,
    ) -> anyhow::Result<()> {
        let mut acc = SimpleCommandLineArtifactVisitor::new();
        CommandLineArgLike::visit_artifacts(*self, &mut acc)?;
        for input in acc.inputs {
            processor(input)?;
        }
        Ok(())
    }
}

impl ArtifactTraversable for &dyn StarlarkArtifactLike {
    fn traverse(
        &self,
        processor: &mut dyn FnMut(ArtifactGroup) -> anyhow::Result<()>,
    ) -> anyhow::Result<()> {
        processor(ArtifactGroup::Artifact(self.get_bound()?))?;
        Ok(())
    }
}

trait ValueAsArtifactTraversable<'v> {
    fn as_artifact_traversable(&self) -> Option<Box<dyn ArtifactTraversable + 'v>>;
}

impl<'v, V: ValueLike<'v>> ValueAsArtifactTraversable<'v> for V {
    fn as_artifact_traversable(&self) -> Option<Box<dyn ArtifactTraversable + 'v>> {
        if let Some(artifact) = self.as_artifact() {
            return Some(box artifact);
        }

        if let Some(cli) = self.to_value().as_command_line() {
            return Some(box cli);
        }

        None
    }
}

#[starlark_module]
fn default_info_creator(builder: &mut GlobalsBuilder) {
    #[starlark(type = "DefaultInfo")]
    fn DefaultInfo<'v>(
        #[starlark(default = FrozenList::empty())] default_outputs: Value<'v>,
        #[starlark(default = FrozenList::empty())] other_outputs: Value<'v>,
        #[starlark(default = SmallMap::new())] sub_targets: SmallMap<String, Value<'v>>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<DefaultInfo<'v>> {
        let heap = eval.heap();
        let default_info_creator = || {
            let default_outputs = heap.alloc_list(&[]);
            let other_outputs = heap.alloc_list(&[]);
            let sub_targets = heap.alloc(Dict::default());
            heap.alloc(DefaultInfo {
                sub_targets,
                default_outputs,
                other_outputs,
            })
        };

        let valid_default_outputs = match List::from_value(default_outputs) {
            Some(list) => {
                if list.iter().all(|v| v.as_artifact().is_some()) {
                    Ok(default_outputs)
                } else {
                    Err(())
                }
            }
            None => Err(()),
        }
        .map_err(|_| ValueError::IncorrectParameterTypeNamed("default_outputs".to_owned()))?;

        let valid_other_outputs = match List::from_value(other_outputs) {
            Some(list) => {
                if list.iter().all(|v| v.as_artifact_traversable().is_some()) {
                    Ok(other_outputs)
                } else {
                    Err(())
                }
            }
            None => Err(()),
        }
        .map_err(|_| ValueError::IncorrectParameterTypeNamed("other_outputs".to_owned()))?;

        let valid_sub_targets = sub_targets
            .into_iter()
            .map(|(k, v)| {
                let as_provider_collection =
                    ProviderCollection::try_from_value_with_default_info(v, &default_info_creator)?;
                Ok((
                    coerce(heap.alloc_str(&k).get_hashed()),
                    heap.alloc(as_provider_collection),
                ))
            })
            .collect::<anyhow::Result<SmallMap<Value<'v>, Value<'v>>>>()?;

        Ok(DefaultInfo {
            default_outputs: valid_default_outputs,
            other_outputs: valid_other_outputs,
            sub_targets: heap.alloc(Dict::new(valid_sub_targets)),
        })
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use buck2_core::result::SharedResult;
    use indoc::indoc;

    use crate::interpreter::{
        rule_defs::artifact::testing::artifactory,
        testing::{import, run_starlark_bzl_test, run_starlark_bzl_test_expecting_error, Tester},
    };

    #[test]
    fn default_info_is_available() -> SharedResult<()> {
        let mut tester = Tester::new()?;
        tester.set_additional_globals(Arc::new(artifactory));
        tester.add_import(
            &import("root", "foo", "defs.bzl"),
            r#"BarInfo = provider(fields=["bar"])"#,
        )?;
        tester.run_starlark_bzl_test(indoc!(
            r#"
            load("//foo:defs.bzl", "BarInfo")

            def test():
                b1 = BarInfo(bar="bar1")
                artifact = source_artifact("foo", "bar.cpp")
                default_defaults = DefaultInfo()
                default1 = DefaultInfo(sub_targets={"foo": [b1]}, default_outputs=[artifact])
                default2 = DefaultInfo(sub_targets={"foo": [default1]}, default_outputs=[])

                assert_eq("DefaultInfo(sub_targets={}, default_outputs=[], other_outputs=[])", repr(default_defaults))
                assert_eq([], default_defaults.default_outputs);
                assert_eq({}, default_defaults.sub_targets);

                r = repr(artifact)
                expected_repr = (
                    'DefaultInfo(sub_targets={"foo": Providers([BarInfo(bar="bar1"), ' +
                     repr(default_defaults) + '])}, default_outputs=[' + r + '], other_outputs=[])'
                )
                assert_eq(expected_repr, repr(default1))
                assert_eq([artifact], default1.default_outputs);
                assert_eq([], default1.sub_targets["foo"][DefaultInfo].default_outputs);
                assert_eq("bar1", default1.sub_targets["foo"][BarInfo].bar);

                expected_repr = (
                    'DefaultInfo(sub_targets={"foo": Providers([' + repr(default1) + '])}, ' +
                    'default_outputs=[], other_outputs=[])'
                )
                assert_eq(expected_repr, repr(default2))
                assert_eq([], default2.default_outputs);
                assert_eq([artifact], default2.sub_targets["foo"][DefaultInfo].default_outputs);
                assert_eq("bar1", default2.sub_targets["foo"][DefaultInfo].sub_targets["foo"][BarInfo].bar);
            "#
        ))
    }

    #[test]
    fn default_info_validates_types() -> SharedResult<()> {
        // TODO(nmj): More complex types
        run_starlark_bzl_test_expecting_error(
            indoc!(
                r#"
            def test():
                default_defaults = DefaultInfo()
                DefaultInfo(sub_targets={"foo": []}, default_outputs=1)
            "#
            ),
            "Type of parameter",
        );

        run_starlark_bzl_test_expecting_error(
            indoc!(
                r#"
            def test():
                default_defaults = DefaultInfo()
                DefaultInfo(sub_targets=[], default_outputs=["foo"])
            "#
            ),
            "Type of parameter",
        );

        run_starlark_bzl_test(indoc!(
            r#"
            def test():
                assert_eq(DefaultInfo.type, "DefaultInfo")
            "#
        ))
    }
}
