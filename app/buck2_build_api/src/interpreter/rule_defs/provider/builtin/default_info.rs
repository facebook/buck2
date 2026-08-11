/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fmt::Debug;
use std::iter;
use std::ptr;

use allocative::Allocative;
use buck2_artifact::artifact::artifact_type::Artifact;
use buck2_artifact::artifact::artifact_type::OutputArtifact;
use buck2_build_api_derive::internal_provider;
use buck2_error::BuckErrorContext;
use buck2_error::internal_error;
use dupe::Dupe;
use starlark::any::ProvidesStaticType;
use starlark::collections::SmallMap;
use starlark::environment::GlobalsBuilder;
use starlark::eval::Evaluator;
use starlark::values::FreezeBranded;
use starlark::values::FreezeError;
use starlark::values::FrozenHeap;
use starlark::values::FrozenValue;
use starlark::values::FrozenValueTyped;
use starlark::values::Heap;
use starlark::values::StarlarkPagable;
use starlark::values::StringValue;
use starlark::values::Trace;
use starlark::values::UnpackAndDiscard;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::ValueLike;
use starlark::values::ValueOf;
use starlark::values::ValueOfUnchecked;
use starlark::values::dict::AllocDict;
use starlark::values::dict::DictType;
use starlark::values::dict::FrozenDictRef;
use starlark::values::dict::UnpackDictEntries;
use starlark::values::list::AllocList;
use starlark::values::list::ListRef;
use starlark::values::list::ListType;
use starlark::values::list::UnpackList;
use starlark::values::none::NoneOr;

use crate as buck2_build_api;
use crate::artifact_groups::ArtifactGroup;
use crate::interpreter::rule_defs::artifact::starlark_artifact::StarlarkArtifact;
use crate::interpreter::rule_defs::artifact::starlark_artifact_like::ValueAsInputArtifactLike;
use crate::interpreter::rule_defs::artifact::starlark_artifact_like::ValueIsInputArtifactAnnotation;
use crate::interpreter::rule_defs::artifact_tagging::ArtifactTag;
use crate::interpreter::rule_defs::cmd_args::CommandLineArtifactVisitor;
use crate::interpreter::rule_defs::cmd_args::value_as::ValueAsCommandLineLike;
use crate::interpreter::rule_defs::provider::ProviderCollection;
use crate::interpreter::rule_defs::provider::collection::FrozenProviderCollection;

/// A provider that all rules' implementations must return
///
/// In many simple cases, this can be inferred for the user.
///
/// Example of a rule's implementation function and how these fields are used by the framework:
///
/// ```python
/// # //foo_binary.bzl
/// def impl(ctx):
///     out = ctx.actions.declare_output("out")
///     ctx.actions.run([ctx.attrs._cc[RunInfo], "-o", out.as_output()] + ctx.attrs.srcs)
///     stripped_out = ctx.actions.declare_output("stripped")
///     debug_symbols_out = ctx.actions.declare_output("debug_info")
///     ctx.actions.run([
///         ctx.attrs._strip[RunInfo],
///         "--binary",
///         out,
///         "--stripped-out",
///         stripped_out.as_output(),
///         "--debug-symbols-out",
///         debug_symbols_out.as_output(),
///     ])
///     return [
///         DefaultInfo(
///             sub_targets = {
///                 "stripped": [
///                     DefaultInfo(default_outputs = [stripped_out, debug_symbols_out]),
///                 ],
///             },
///             default_output = out,
///         ),
///     ]
///
/// foo_binary = rule(
///     impl = impl,
///     attrs = {
///         "srcs": attrs.list(attrs.source()),
///         "_cc": attrs.dep(default = "//tools:cc", providers = [RunInfo]),
///         "_strip_script": attrs.dep(default = "//tools:strip", providers = [RunInfo]),
///     },
/// )
///
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
#[derive(
    Clone,
    Debug,
    FreezeBranded,
    Trace,
    ProvidesStaticType,
    Allocative,
    StarlarkPagable
)]
#[freeze_branded(validator = validate_default_info)]
#[repr(C)]
pub struct DefaultInfo<'v> {
    /// A mapping of names to `ProviderCollection`s. The keys are used when resolving the
    /// `ProviderName` portion of a `ProvidersLabel` in order to access the providers for a
    /// subtarget, such as when doing `buck2 build cell//foo:bar[baz]`. Just like any
    /// `ProviderCollection`, this collection must include at least a `DefaultInfo` provider. The
    /// subtargets can have their own subtargets as well, which can be accessed by chaining them,
    /// e.g.: `buck2 build cell//foo:bar[baz][qux]`.
    sub_targets: ValueOfUnchecked<'v, DictType<String, FrozenProviderCollection>>,
    /// A list of `Artifact`s that are built by default if this rule is requested
    /// explicitly (via CLI or `$(location)` etc), or depended on as as a "source"
    /// (i.e., `attrs.source()`).
    default_outputs: ValueOfUnchecked<'v, ListType<ValueIsInputArtifactAnnotation>>,
    /// A list of `ArtifactTraversable`. The underlying `Artifact`s they define will
    /// be built by default if this rule is requested (via CLI or `$(location)` etc),
    /// but _not_ when it's depended on as as a "source" (i.e., `attrs.source()`).
    /// `ArtifactTraversable` can be an `Artifact` (which yields itself), or
    /// `cmd_args`, which expand to all their inputs.
    other_outputs: ValueOfUnchecked<'v, ListType<ValueAsCommandLineLike<'static>>>,
}

fn validate_default_info<'v>(info: &DefaultInfo<'v>) -> buck2_error::Result<()> {
    // Check length of default outputs
    let default_output_list =
        ListRef::from_value(info.default_outputs.get()).expect("should be a list from constructor");
    if default_output_list.len() > 1 {
        tracing::info!("DefaultInfo.default_output should only have a maximum of 1 item.");
        // TODO use soft_error when landed
        // TODO error rather than soft warning
        // return Err(buck2_error::buck2_error!(
        //     "DefaultInfo.default_output can only have a maximum of 1 item."
        // ));
    }

    // Check mutable data hasn't been modified.
    for output in info.default_outputs_impl()? {
        output?;
    }
    for sub_target in info.sub_targets_impl()? {
        sub_target?;
    }

    Ok(())
}

impl<'v> DefaultInfo<'v> {
    pub fn empty(heap: Heap<'v>) -> Self {
        let sub_targets = ValueOfUnchecked::<DictType<_, _>>::new(heap.alloc(AllocDict::EMPTY));
        let default_outputs = ValueOfUnchecked::<ListType<_>>::new(heap.alloc(AllocList::EMPTY));
        let other_outputs = ValueOfUnchecked::<ListType<_>>::new(heap.alloc(AllocList::EMPTY));
        DefaultInfo {
            sub_targets,
            default_outputs,
            other_outputs,
        }
    }

    pub(crate) fn testing_empty(heap: &'v FrozenHeap) -> FrozenValueTyped<'v, DefaultInfo<'v>> {
        let sub_targets = heap
            .alloc_typed_unchecked(AllocDict(
                iter::empty::<(String, FrozenProviderCollection)>(),
            ))
            .cast()
            .to_value();
        let default_outputs =
            ValueOfUnchecked::<ListType<_>>::new(heap.alloc(AllocList::EMPTY).to_value());
        let other_outputs =
            ValueOfUnchecked::<ListType<_>>::new(heap.alloc(AllocList::EMPTY).to_value());
        heap.alloc_simple_typed(DefaultInfo {
            sub_targets,
            default_outputs,
            other_outputs,
        })
    }

    fn get_sub_target_providers_impl(
        &self,
        name: &str,
    ) -> buck2_error::Result<Option<FrozenValueTyped<'v, ProviderCollection<'v>>>> {
        FrozenDictRef::from_frozen_value(
            self.sub_targets
                .get()
                .unpack_frozen()
                .expect("default info is frozen"),
        )
        .ok_or_else(|| internal_error!("sub_targets should be a dict-like object"))?
        .get_str(name)
        .map(|v| {
            FrozenValueTyped::new_err(v).buck_error_context(
                "Values inside of a frozen provider should be frozen provider collection",
            )
        })
        .transpose()
    }

    /// Panics when called on an unfrozen instance.
    pub fn get_sub_target_providers(
        &self,
        name: &str,
    ) -> Option<FrozenValueTyped<'v, ProviderCollection<'v>>> {
        self.get_sub_target_providers_impl(name).unwrap()
    }

    fn default_outputs_impl(
        &self,
    ) -> buck2_error::Result<impl Iterator<Item = buck2_error::Result<StarlarkArtifact>> + '_> {
        let list = ListRef::from_value(self.default_outputs.get())
            .ok_or_else(|| internal_error!("Should be list of artifacts"))?;

        Ok(list.iter().map(|v| {
            let frozen_value = v
                .unpack_frozen()
                .ok_or_else(|| internal_error!("should be frozen"))?;

            Ok(
                if let Some(starlark_artifact) = frozen_value.downcast_ref::<StarlarkArtifact>() {
                    starlark_artifact.dupe()
                } else {
                    // This code path is for StarlarkPromiseArtifact. We have to create a `StarlarkArtifact` object here.
                    let artifact_like =
                        ValueAsInputArtifactLike::unpack_value(frozen_value.to_value())?
                            .ok_or_else(|| internal_error!("Should be list of artifacts"))?;
                    artifact_like.0.get_bound_starlark_artifact()?
                },
            )
        }))
    }

    /// Panics when called on an unfrozen instance.
    pub fn default_outputs(&self) -> Vec<StarlarkArtifact> {
        self.default_outputs_impl()
            .unwrap()
            .collect::<Result<_, _>>()
            .unwrap()
    }

    pub fn default_outputs_raw(&self) -> Value<'v> {
        self.default_outputs.get()
    }

    fn sub_targets_impl(
        &self,
    ) -> buck2_error::Result<
        impl Iterator<
            Item = buck2_error::Result<(&'v str, FrozenValueTyped<'v, ProviderCollection<'v>>)>,
        > + '_,
    > {
        let sub_targets = FrozenDictRef::from_frozen_value(
            self.sub_targets
                .get()
                .unpack_frozen()
                .expect("default info is frozen"),
        )
        .ok_or_else(|| internal_error!("sub_targets should be a dict-like object"))?;

        Ok(sub_targets.iter().map(|(k, v)| {
            buck2_error::Ok((
                k.to_value()
                    .unpack_str()
                    .ok_or_else(|| internal_error!("sub_targets should have string keys"))?,
                FrozenValueTyped::new(v).ok_or_else(|| {
                    internal_error!(
                        "Values inside of a frozen provider should be frozen provider collection",
                    )
                })?,
            ))
        }))
    }

    /// Panics when called on an unfrozen instance.
    pub fn sub_targets(&self) -> SmallMap<&'v str, FrozenValueTyped<'v, ProviderCollection<'v>>> {
        self.sub_targets_impl()
            .unwrap()
            .collect::<Result<_, _>>()
            .unwrap()
    }

    pub fn sub_targets_raw(&self) -> Value<'v> {
        self.sub_targets.get()
    }

    pub fn for_each_default_output_artifact_only(
        &self,
        processor: &mut dyn FnMut(Artifact),
    ) -> buck2_error::Result<()> {
        self.for_each_in_list(self.default_outputs.get(), |value| {
            processor(
                ValueAsInputArtifactLike::unpack_value_err(value)?
                    .0
                    .get_bound_artifact()?,
            );
            Ok(())
        })
    }

    pub fn for_each_default_output_other_artifacts_only(
        &self,
        processor: &mut dyn FnMut(ArtifactGroup),
    ) -> buck2_error::Result<()> {
        self.for_each_in_list(self.default_outputs.get(), |value| {
            let others = ValueAsInputArtifactLike::unpack_value_err(value)?
                .0
                .get_associated_artifacts();
            others
                .iter()
                .flat_map(|v| v.iter())
                .for_each(|other| processor(other.dupe()));
            Ok(())
        })
    }

    pub fn for_each_other_output(
        &self,
        processor: &mut dyn FnMut(ArtifactGroup),
    ) -> buck2_error::Result<()> {
        struct Visitor<'x>(&'x mut dyn FnMut(ArtifactGroup));

        impl<'v> CommandLineArtifactVisitor<'v> for Visitor<'_> {
            fn visit_input(&mut self, input: ArtifactGroup, _: Vec<&ArtifactTag>) {
                (self.0)(input);
            }

            fn visit_declared_output(
                &mut self,
                _artifact: OutputArtifact<'v>,
                _tags: Vec<&ArtifactTag>,
            ) {
            }

            fn visit_frozen_output(&mut self, _artifact: Artifact, _tags: Vec<&ArtifactTag>) {}
        }

        self.for_each_in_list(self.other_outputs.get(), |value| {
            let arg_like = ValueAsCommandLineLike::unpack_value_err(value)?.0;
            arg_like.visit_artifacts(&mut Visitor(processor))?;
            Ok(())
        })
    }

    pub fn for_each_output(
        &self,
        processor: &mut dyn FnMut(ArtifactGroup),
    ) -> buck2_error::Result<()> {
        self.for_each_default_output_artifact_only(&mut |a| processor(ArtifactGroup::Artifact(a)))?;
        self.for_each_default_output_other_artifacts_only(processor)?;
        self.for_each_other_output(processor)
    }

    fn for_each_in_list(
        &self,
        value: Value<'v>,
        mut processor: impl FnMut(Value<'v>) -> buck2_error::Result<()>,
    ) -> buck2_error::Result<()> {
        let outputs_list = ListRef::from_value(value)
            .unwrap_or_else(|| panic!("expected list, got `{value:?}` from info `{self:?}`"));

        for value in outputs_list.iter() {
            processor(value)?;
        }

        Ok(())
    }
}

impl<'v> PartialEq for DefaultInfo<'v> {
    // frozen default infos can be compared by ptr for a simple equality
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(self, other)
    }
}

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
enum DefaultOutputError {
    #[error("Cannot specify both `default_output` and `default_outputs`.")]
    ConflictingArguments,
}

#[starlark_module]
fn default_info_creator(builder: &mut GlobalsBuilder) {
    #[starlark(as_type = FrozenDefaultInfo)]
    fn DefaultInfo<'v>(
        // TODO(nga): parameters must be named only.
        #[starlark(default = NoneOr::None)] default_output: NoneOr<
            ValueOf<'v, ValueIsInputArtifactAnnotation>,
        >,
        #[starlark(default = NoneOr::None)] default_outputs: NoneOr<
            ValueOf<'v, UnpackList<UnpackAndDiscard<ValueIsInputArtifactAnnotation>>>,
        >,
        #[starlark(default = ValueOf { value: FrozenValue::new_empty_list().to_value(), typed: UnpackList::default()})]
        other_outputs: ValueOf<
            'v,
            UnpackList<UnpackAndDiscard<ValueAsCommandLineLike<'v>>>,
        >,
        #[starlark(default = UnpackDictEntries::default())] sub_targets: UnpackDictEntries<
            StringValue<'v>,
            Value<'v>,
        >,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> starlark::Result<DefaultInfo<'v>> {
        let heap = eval.heap();

        // support both list and singular options for now until we migrate all the rules.
        let valid_default_outputs: ValueOfUnchecked<ListType<ValueIsInputArtifactAnnotation>> =
            match (default_outputs.into_option(), default_output.into_option()) {
                (Some(list), None) => list.as_unchecked().cast(),
                (None, Some(default_output)) => {
                    // handle where we didn't specify `default_outputs`, which means we should use the new
                    // `default_output`.
                    eval.heap()
                        .alloc_typed_unchecked(AllocList([default_output.as_unchecked()]))
                        .cast()
                }
                (None, None) => {
                    ValueOfUnchecked::<ListType<_>>::new(eval.heap().alloc(AllocList::EMPTY))
                }
                (Some(_), Some(_)) => {
                    return Err(
                        buck2_error::Error::from(DefaultOutputError::ConflictingArguments).into(),
                    );
                }
            };

        let valid_sub_targets = sub_targets
            .entries
            .into_iter()
            .map(|(k, v)| {
                let as_provider_collection = ProviderCollection::try_from_value_subtarget(v, heap)?;
                Ok((
                    k,
                    ValueOfUnchecked::<FrozenProviderCollection>::new(
                        heap.alloc(as_provider_collection),
                    ),
                ))
            })
            .collect::<buck2_error::Result<Vec<(StringValue<'v>, _)>>>()?;

        Ok(DefaultInfo {
            default_outputs: valid_default_outputs,
            other_outputs: other_outputs.as_unchecked().cast(),
            sub_targets: heap
                .alloc_typed_unchecked(AllocDict(valid_sub_targets))
                .cast(),
        })
    }
}
