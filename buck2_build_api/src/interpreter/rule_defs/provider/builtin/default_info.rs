/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Debug;
use std::ptr;

use anyhow::Context;
use buck2_build_api_derive::internal_provider;
use gazebo::any::ProvidesStaticType;
use gazebo::coerce::Coerce;
use gazebo::dupe::Dupe;
use starlark::collections::SmallMap;
use starlark::environment::GlobalsBuilder;
use starlark::eval::Evaluator;
use starlark::values::dict::Dict;
use starlark::values::dict::FrozenDict;
use starlark::values::list::FrozenList;
use starlark::values::list::List;
use starlark::values::none::NoneType;
use starlark::values::type_repr::DictType;
use starlark::values::Freeze;
use starlark::values::FrozenRef;
use starlark::values::FrozenValue;
use starlark::values::FrozenValueTyped;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::values::ValueError;
use starlark::values::ValueLike;
use thiserror::Error;

use crate::actions::artifact::Artifact;
use crate::artifact_groups::ArtifactGroup;
use crate::interpreter::rule_defs::artifact::StarlarkArtifact;
use crate::interpreter::rule_defs::artifact::StarlarkArtifactLike;
use crate::interpreter::rule_defs::artifact::ValueAsArtifactLike;
use crate::interpreter::rule_defs::cmd_args::CommandLineArgLike;
use crate::interpreter::rule_defs::cmd_args::SimpleCommandLineArtifactVisitor;
use crate::interpreter::rule_defs::cmd_args::ValueAsCommandLineLike;
use crate::interpreter::rule_defs::provider::collection::FrozenProviderCollection;
use crate::interpreter::rule_defs::provider::ProviderCollection;

/// A provider that all rules' implementations must return
///
/// In many simple cases, this can be inferred for the user.
///
/// Example of a rule's implementation function and how these fields are used by the framework:
///
/// ```starlark
/// # //foo_binary.bzl
/// def impl(ctx):
///     ctx.action.run([ctx.attrs._cc[RunInfo], "-o", ctx.attrs.out.as_output()] + ctx.attrs.srcs)
///     ctx.action.run([
///         ctx.attrs._strip[RunInfo],
///         "--binary",
///         ctx.attrs.out,
///         "--stripped-out",
///         ctx.attrs.stripped.as_output(),
///         "--debug-symbols-out",
///         ctx.attrs.debug_info.as_output(),
///     ])
///     return [
///         DefaultInfo(
///             sub_targets = {
///                 "stripped": [
///                     DefaultInfo(default_outputs = [ctx.attrs.stripped, ctx.attrs.debug_info]),
///                 ],
///             },
///             default_outputs = [ctx.attrs.out],
///     ]
///
/// foo_binary = rule(
///     impl=impl,
///     attrs={
///         "srcs": attrs.list(attrs.source()),
///         "out": attrs.output(),
///         "stripped": attrs.output(),
///         "debug_info": attrs.output(),
///         "_cc": attrs.dep(default="//tools:cc", providers=[RunInfo]),
///         "_strip_script": attrs.dep(default="//tools:strip", providers=[RunInfo])
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
    /// A mapping of names to `ProviderCollection`s. The keys are used when
    /// resolving the `ProviderName` portion of a `ProvidersLabel`. These collections
    /// can contain, and actually /must/ contain a `DefaultInfo` provider. However,
    /// nested label syntax is not supported. That is, `cell//foo:bar[baz]` is valid,
    /// `cell//foo:bar[baz][quz]` is not.
    #[provider(field_type = "DictType<String, ProviderCollection>")]
    sub_targets: V,
    /// A list of `Artifact`s that are built by default if this rule is requested
    /// explicitly, or depended on as as a "source".
    #[provider(field_type = "Vec<StarlarkArtifact>")]
    default_outputs: V,
    /// A list of `ArtifactTraversable`. The underlying `Artifact`s they define will
    /// be built by default if this rule is requested, but _not_ when it's depended
    /// on as as a "source". `ArtifactTraversable` can be an `Artifact` (which yields
    /// itself), or `cmd_args`, which expand to all their inputs.
    #[provider(field_type = "Vec<StarlarkArtifact>")]
    other_outputs: V,
}

fn check_max_one_list<'v, V>(info: &DefaultInfoGen<V>) -> anyhow::Result<()>
where
    V: ValueLike<'v>,
{
    let default_output_list = List::from_value(info.default_outputs.to_value())
        .expect("should be a list from constructor");
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

    pub fn for_each_default_output_artifact_only(
        &self,
        processor: &mut dyn FnMut(Artifact) -> anyhow::Result<()>,
    ) -> anyhow::Result<()> {
        self.for_each_in_list(self.default_outputs, |value| {
            processor(
                value
                    .as_artifact()
                    .ok_or_else(|| anyhow::anyhow!("not an artifact"))?
                    .get_bound_artifact()?,
            )
        })
    }

    pub fn for_each_default_output_other_artifacts_only(
        &self,
        processor: &mut dyn FnMut(ArtifactGroup) -> anyhow::Result<()>,
    ) -> anyhow::Result<()> {
        self.for_each_in_list(self.default_outputs, |value| {
            let (_, others) = value
                .as_artifact()
                .ok_or_else(|| anyhow::anyhow!("not an artifact"))?
                .get_bound_artifact_and_associated_artifacts()?;
            others
                .iter()
                .for_each(|other| processor(other.dupe()).unwrap());
            Ok(())
        })
    }

    // TODO(marwhal): We can remove this once we migrate all other outputs to be handled with Artifacts directly
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
        self.for_each_default_output_artifact_only(&mut |a| processor(ArtifactGroup::Artifact(a)))?;
        self.for_each_default_output_other_artifacts_only(processor)?;
        // TODO(marwhal): We can remove this once we migrate all other outputs to be handled with Artifacts directly
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
        processor(ArtifactGroup::Artifact(self.get_bound_artifact()?))?;
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

#[derive(Debug, Error)]
enum DefaultOutputError {
    #[error("Cannot specify both `default_output` and `default_outputs`.")]
    ConflictingArguments,
}

#[starlark_module]
fn default_info_creator(builder: &mut GlobalsBuilder) {
    #[starlark(type = "DefaultInfo")]
    fn DefaultInfo<'v>(
        #[starlark(default = NoneType)] default_output: Value<'v>,
        #[starlark(default = NoneType)] default_outputs: Value<'v>,
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

        // support both list and singular options for now until we migrate all the rules.
        let valid_default_outputs = if !default_outputs.is_none() {
            match List::from_value(default_outputs) {
                Some(list) => {
                    if !default_output.is_none() {
                        return Err(anyhow::anyhow!(DefaultOutputError::ConflictingArguments));
                    }

                    if list.iter().all(|v| v.as_artifact().is_some()) {
                        default_outputs
                    } else {
                        return Err(anyhow::anyhow!(ValueError::IncorrectParameterTypeNamed(
                            "default_outputs".to_owned()
                        )));
                    }
                }
                None => {
                    return Err(anyhow::anyhow!(ValueError::IncorrectParameterTypeNamed(
                        "default_outputs".to_owned()
                    )));
                }
            }
        } else {
            // handle where we didn't specify `default_outputs`, which means we should use the new
            // `default_output`.
            if default_output.is_none() {
                eval.heap().alloc_list(&[])
            } else if default_output.as_artifact().is_some() {
                eval.heap().alloc_list(&[default_output])
            } else {
                return Err(anyhow::anyhow!(ValueError::IncorrectParameterTypeNamed(
                    "default_output".to_owned()
                )));
            }
        };

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
                    heap.alloc_str(&k).get_hashed_value(),
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
    use buck2_common::result::SharedResult;
    use indoc::indoc;

    use crate::interpreter::rule_defs::artifact::testing::artifactory;
    use crate::interpreter::testing::import;
    use crate::interpreter::testing::run_starlark_bzl_test;
    use crate::interpreter::testing::run_starlark_bzl_test_expecting_error;
    use crate::interpreter::testing::Tester;

    #[test]
    fn default_info_is_available() -> SharedResult<()> {
        let mut tester = Tester::new()?;
        tester.set_additional_globals(artifactory);
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
