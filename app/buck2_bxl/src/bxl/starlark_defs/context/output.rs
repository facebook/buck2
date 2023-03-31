/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cell::RefCell;
use std::fmt::Display;
use std::io::Write;
use std::ops::DerefMut;

use allocative::Allocative;
use anyhow::Context;
use buck2_build_api::artifact_groups::ArtifactGroup;
use buck2_build_api::bxl::build_result::BxlBuildResult;
use buck2_build_api::interpreter::rule_defs::artifact::StarlarkArtifact;
use buck2_build_api::interpreter::rule_defs::cmd_args::CommandLineArgLike;
use buck2_build_api::interpreter::rule_defs::cmd_args::SimpleCommandLineArtifactVisitor;
use buck2_build_api::interpreter::rule_defs::cmd_args::StarlarkCommandLineInputs;
use buck2_build_api::interpreter::rule_defs::cmd_args::ValueAsCommandLineLike;
use buck2_core::fs::artifact_path_resolver::ArtifactFs;
use buck2_core::fs::project::ProjectRoot;
use buck2_execute::path::artifact_path::ArtifactPath;
use derivative::Derivative;
use derive_more::Display;
use dupe::Dupe;
use gazebo::prelude::SliceExt;
use serde::ser::SerializeSeq;
use serde::Serialize;
use serde::Serializer;
use starlark::any::ProvidesStaticType;
use starlark::collections::SmallSet;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::starlark_module;
use starlark::starlark_type;
use starlark::values::dict::Dict;
use starlark::values::dict::DictRef;
use starlark::values::list::ListRef;
use starlark::values::none::NoneType;
use starlark::values::record::Record;
use starlark::values::structs::StructRef;
use starlark::values::tuple::TupleRef;
use starlark::values::type_repr::StarlarkTypeRepr;
use starlark::values::AllocValue;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::ValueError;
use starlark::values::ValueLike;
use starlark::StarlarkDocs;

use super::starlark_async::BxlSafeDiceComputations;
use crate::bxl::starlark_defs::artifacts::EnsuredArtifact;
use crate::bxl::starlark_defs::artifacts::EnsuredArtifactGroup;
use crate::bxl::starlark_defs::build_result::StarlarkBxlBuildResult;
use crate::bxl::starlark_defs::context::build::StarlarkProvidersArtifactIterable;

#[derive(
    ProvidesStaticType,
    Derivative,
    Display,
    Trace,
    NoSerialize,
    StarlarkDocs,
    Allocative
)]
#[display(fmt = "{:?}", self)]
#[starlark_docs(directory = "bxl")]
#[derivative(Debug)]
pub struct OutputStream<'v> {
    #[derivative(Debug = "ignore")]
    #[trace(unsafe_ignore)]
    #[allocative(skip)]
    pub(crate) sink: RefCell<Box<dyn Write>>,
    #[trace(unsafe_ignore)]
    artifacts_to_ensure: RefCell<Option<SmallSet<EnsuredArtifactOrGroup>>>,
    #[derivative(Debug = "ignore")]
    pub(crate) project_fs: ProjectRoot,
    #[derivative(Debug = "ignore")]
    pub(crate) artifact_fs: ArtifactFs,
    #[trace(unsafe_ignore)]
    #[derivative(Debug = "ignore")]
    #[allocative(skip)]
    pub(crate) async_ctx: BxlSafeDiceComputations<'v>,
}

/// We can ensure either an `Artifact` or an `ArtifactGroup`. When we want to ensure a `CommandLineArgLike` object,
/// the result of visiting its artifacts is a list of `ArtifactGroup`s. It's convenient to preserve the group rather
/// than extract the individual `Artifact`s from it, for perf/memory optimizations.
#[derive(Display, Debug, Allocative, Hash, Eq, PartialEq)]
pub enum EnsuredArtifactOrGroup {
    Artifact(EnsuredArtifact),
    ArtifactGroup(ArtifactGroup),
}

impl<'v> OutputStream<'v> {
    pub fn new(
        project_fs: ProjectRoot,
        artifact_fs: ArtifactFs,
        sink: RefCell<Box<dyn Write>>,
        async_ctx: BxlSafeDiceComputations<'v>,
    ) -> Self {
        Self {
            sink,
            artifacts_to_ensure: RefCell::new(Some(Default::default())),
            project_fs,
            artifact_fs,
            async_ctx,
        }
    }

    pub fn take_artifacts(&self) -> SmallSet<EnsuredArtifactOrGroup> {
        self.artifacts_to_ensure.borrow_mut().take().unwrap()
    }
}

impl<'v> StarlarkTypeRepr for &'v OutputStream<'v> {
    fn starlark_type_repr() -> String {
        OutputStream::get_type_starlark_repr()
    }
}

impl<'v> UnpackValue<'v> for &'v OutputStream<'v> {
    fn unpack_value(x: Value<'v>) -> Option<&'v OutputStream> {
        x.downcast_ref()
    }
}

impl<'v> StarlarkValue<'v> for OutputStream<'v> {
    starlark_type!("bxl_output_stream");

    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(register_output_stream)
    }
}

impl<'v> AllocValue<'v> for OutputStream<'v> {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_complex_no_freeze(self)
    }
}

/// The output stream for bxl to print values to the console as their result
#[starlark_module]
fn register_output_stream(builder: &mut MethodsBuilder) {
    /// Outputs results to the console via stdout. These outputs are considered to be the results
    /// of a bxl script, which will be displayed to stdout by buck2 even when the script is cached.
    /// Accepts an optional separator that defaults to " ".
    ///
    /// Prints that are not result of the bxl should be printed via stderr via the stdlib `print`
    /// and `pprint`. Note that `ctx.output.print()` is intended for simple outputs. For more complex
    /// outputs, the recommendation would be to write them to a file.
    ///
    /// Sample usage:
    /// ```text
    /// def _impl_print(ctx):
    ///     ctx.output.print("test")
    /// ```
    fn print<'v>(
        this: &'v OutputStream<'v>,
        #[starlark(args)] args: Vec<Value<'v>>,
        #[starlark(default = " ")] sep: &'v str,
    ) -> anyhow::Result<NoneType> {
        let mut first = true;
        let mut write = |d: &dyn Display| -> anyhow::Result<()> {
            if !first {
                write!(this.sink.borrow_mut(), "{}{}", sep, d)?;
            } else {
                write!(this.sink.borrow_mut(), "{}", d)?;
                first = false;
            }
            Ok(())
        };

        for arg in args {
            if let Some(ensured) = <&EnsuredArtifact>::unpack_value(arg) {
                let path = get_artifact_path_display(
                    ensured.as_artifact().get_artifact_path(),
                    ensured.abs(),
                    &this.project_fs,
                    &this.artifact_fs,
                )?;
                write(&path)?;
            } else if let Some(ensured) = <&EnsuredArtifactGroup>::unpack_value(arg) {
                this.async_ctx.via_dice(|ctx| {
                    ensured.visit_artifact_path_without_associated_deduped(
                        |artifact_path, abs| {
                            let path = get_artifact_path_display(
                                artifact_path,
                                abs,
                                &this.project_fs,
                                &this.artifact_fs,
                            )?;
                            write(&path)
                        },
                        ctx,
                    )
                })?;
            } else {
                write(&arg.to_str())?;
            }
        }

        writeln!(this.sink.borrow_mut())?;

        Ok(NoneType)
    }

    /// Outputs results to the console via stdout as a json.
    /// These outputs are considered to be the results of a bxl script, which will be displayed to
    /// stdout by buck2 even when the script is cached.
    ///
    /// Prints that are not result of the bxl should be printed via stderr via the stdlib `print`
    /// and `pprint`.
    ///
    /// Sample usage:
    /// ```text
    /// def _impl_print_json(ctx):
    ///     outputs = {}
    ///     outputs.update({"foo": bar})
    ///     ctx.output.print_json("test")
    /// ```
    fn print_json<'v>(this: &'v OutputStream<'v>, value: Value<'v>) -> anyhow::Result<NoneType> {
        /// A wrapper with a Serialize instance so we can pass down the necessary context.
        struct SerializeValue<'a, 'v> {
            value: Value<'v>,
            artifact_fs: &'a ArtifactFs,
            project_fs: &'a ProjectRoot,
            async_ctx: &'v BxlSafeDiceComputations<'v>,
        }

        impl<'a, 'v> SerializeValue<'a, 'v> {
            fn with_value(&self, x: Value<'v>) -> Self {
                Self {
                    value: x,
                    artifact_fs: self.artifact_fs,
                    project_fs: self.project_fs,
                    async_ctx: self.async_ctx,
                }
            }
        }

        impl<'a, 'v> Serialize for SerializeValue<'a, 'v> {
            fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where
                S: Serializer,
            {
                if let Some(ensured) = <&EnsuredArtifact>::unpack_value(self.value) {
                    let path = get_artifact_path_display(
                        ensured.as_artifact().get_artifact_path(),
                        ensured.abs(),
                        self.project_fs,
                        self.artifact_fs,
                    )
                    .map_err(|err| serde::ser::Error::custom(format!("{:#}", err)))?;
                    serializer.serialize_str(&path)
                } else if let Some(ensured) = <&EnsuredArtifactGroup>::unpack_value(self.value) {
                    let mut seq_ser = serializer.serialize_seq(None)?;

                    self.async_ctx
                        .via_dice(|ctx| {
                            ensured.visit_artifact_path_without_associated_deduped(
                                |artifact_path, abs| {
                                    let path = get_artifact_path_display(
                                        artifact_path,
                                        abs,
                                        self.project_fs,
                                        self.artifact_fs,
                                    )?;
                                    seq_ser
                                        .serialize_element(&path)
                                        .map_err(|err| anyhow::anyhow!(format!("{:#}", err)))?;
                                    Ok(())
                                },
                                ctx,
                            )
                        })
                        .map_err(|err| serde::ser::Error::custom(format!("{:#}", err)))?;
                    seq_ser.end()
                } else if let Some(x) = ListRef::from_value(self.value) {
                    serializer.collect_seq(x.iter().map(|v| self.with_value(v)))
                } else if let Some(x) = TupleRef::from_value(self.value) {
                    serializer.collect_seq(x.iter().map(|v| self.with_value(v)))
                } else if let Some(x) = DictRef::from_value(self.value) {
                    serializer.collect_map(
                        x.iter()
                            .map(|(k, v)| (self.with_value(k), self.with_value(v))),
                    )
                } else if let Some(x) = StructRef::from_value(self.value) {
                    serializer.collect_map(x.iter().map(|(k, v)| (k, self.with_value(v))))
                } else if let Some(x) = Record::from_value(self.value) {
                    serializer.collect_map(x.iter().map(|(k, v)| (k, self.with_value(v))))
                } else {
                    self.value.serialize(serializer)
                }
            }
        }

        serde_json::to_writer_pretty(
            this.sink.borrow_mut().deref_mut(),
            &SerializeValue {
                value,
                artifact_fs: &this.artifact_fs,
                project_fs: &this.project_fs,
                async_ctx: &this.async_ctx,
            },
        )
        .context("Error writing to JSON for `write_json`")?;
        writeln!(this.sink.borrow_mut())?;

        Ok(NoneType)
    }

    /// Marks the artifact as an artifact that should be available to the users at the end of
    /// the bxl invocation. Any artifacts that do not get registered via this call is not
    /// accessible by users at the end of bxl script.
    ///
    /// This function returns an `ensured_artifact` type that can be printed via `ctx.output.print()`
    /// to print its actual path on disk.
    ///
    /// Sample usage:
    /// ```text
    /// def _impl_ensure(ctx):
    ///     actions = ctx.bxl_actions.action_factory()
    ///     output = actions.write("my_output", "my_content")
    ///     ensured = ctx.output.ensure(output)
    ///     ctx.output.print(ensured)
    /// ```
    fn ensure<'v>(this: &OutputStream, artifact: Value<'v>) -> anyhow::Result<EnsuredArtifact> {
        let artifact = EnsuredArtifact::new(artifact)?;
        populate_ensured_artifacts(this, EnsuredArtifactOrGroup::Artifact(artifact.clone()))?;

        Ok(artifact)
    }

    /// Same as `ensure`, but for multiple artifacts. Will preserve the shape of the inputs (i.e. if the resulting
    /// `Dict` of a `ctx.build()` is passed in, the output will be a `Dict` where the key is preserved,
    /// and the values are converted to `EnsuredArtifact`s).
    ///
    /// Note that is slower to loop through objects and ensure them one by one than it is to call `ensure_multiple()`
    /// on all the objects at once (if possible).
    /// So, it is suggested to use this method when you are only ensuring a few individual artifacts that are not stored in an iterable.
    ///
    /// Sample usage:
    /// ```text
    /// def _impl_ensure_multiple(ctx):
    ///     outputs = {}
    ///     for target, value in ctx.build(ctx.cli_args.target).items():
    ///     outputs.update({target.raw_target(): ctx.output.ensure_multiple(value.artifacts())})
    ///     ctx.output.print_json(outputs)
    /// ```
    fn ensure_multiple<'v>(
        this: &'v OutputStream<'v>,
        artifacts: Value<'v>,
        heap: &'v Heap,
    ) -> anyhow::Result<Value<'v>> {
        if artifacts.is_none() {
            Ok(heap.alloc(Vec::<EnsuredArtifact>::new()))
        } else if let Some(list) = <&ListRef>::unpack_value(artifacts) {
            let artifacts: Vec<EnsuredArtifact> = list.content().try_map(|artifact| {
                let artifact = EnsuredArtifact::new(*artifact)?;

                populate_ensured_artifacts(
                    this,
                    EnsuredArtifactOrGroup::Artifact(artifact.clone()),
                )?;

                Ok::<EnsuredArtifact, anyhow::Error>(artifact)
            })?;

            Ok(heap.alloc(artifacts))
        } else if let Some(artifact_gen) =
            <&StarlarkProvidersArtifactIterable>::unpack_value(artifacts)
        {
            Ok(heap.alloc(get_artifacts_from_bxl_build_result(
                artifact_gen
                    .0
                    .downcast_ref::<StarlarkBxlBuildResult>()
                    .unwrap(),
                this,
            )?))
        } else if let Some(bxl_build_result) = <&StarlarkBxlBuildResult>::unpack_value(artifacts) {
            Ok(heap.alloc(get_artifacts_from_bxl_build_result(bxl_build_result, this)?))
        } else if let Some(build_result_dict) = <DictRef>::unpack_value(artifacts) {
            Ok(heap.alloc(Dict::new(
                build_result_dict
                    .iter()
                    .map(|(label, value)| {
                        if let Some(bxl_build_result) =
                            <&StarlarkBxlBuildResult>::unpack_value(value)
                        {
                            Ok((
                                label.get_hashed()?,
                                heap.alloc(get_artifacts_from_bxl_build_result(
                                    bxl_build_result,
                                    this,
                                )?),
                            ))
                        } else {
                            Err(anyhow::anyhow!(incorrect_parameter_type_error(artifacts)))
                        }
                    })
                    .collect::<anyhow::Result<_>>()?,
            )))
        } else if let Some(cmd_line) = artifacts.as_command_line() {
            let inputs = get_cmd_line_inputs(cmd_line)?;
            let mut result = Vec::new();

            for artifact_group in &inputs.inputs {
                populate_ensured_artifacts(
                    this,
                    EnsuredArtifactOrGroup::ArtifactGroup(artifact_group.dupe()),
                )?;
                result.push(artifact_group.dupe());
            }

            Ok(heap.alloc(EnsuredArtifactGroup::new(result, false, heap)))
        } else {
            Err(anyhow::anyhow!(incorrect_parameter_type_error(artifacts)))
        }
    }
}

pub(crate) fn get_cmd_line_inputs<'v>(
    cmd_line: &'v dyn CommandLineArgLike,
) -> anyhow::Result<StarlarkCommandLineInputs> {
    let mut visitor = SimpleCommandLineArtifactVisitor::new();
    cmd_line.visit_artifacts(&mut visitor)?;
    let inputs = StarlarkCommandLineInputs {
        inputs: visitor.inputs,
    };
    Ok(inputs)
}

pub(crate) fn get_artifact_path_display(
    artifact_path: ArtifactPath,
    abs: bool,
    project_fs: &ProjectRoot,
    artifact_fs: &ArtifactFs,
) -> anyhow::Result<String> {
    let resolved = artifact_path.resolve(artifact_fs)?;
    Ok(if abs {
        project_fs.resolve(&resolved).to_string()
    } else {
        resolved.to_string()
    })
}

fn incorrect_parameter_type_error(artifacts: Value) -> ValueError {
    ValueError::IncorrectParameterTypeWithExpected(
        "list of artifacts, bxl_built_artifacts_iterable, or command-line-arg-like".to_owned(),
        artifacts.get_type().to_owned(),
    )
}

fn populate_ensured_artifacts(
    output_stream: &OutputStream,
    ensured: EnsuredArtifactOrGroup,
) -> anyhow::Result<()> {
    output_stream
        .artifacts_to_ensure
        .borrow_mut()
        .as_mut()
        .expect("should not have been taken")
        .insert(ensured);
    Ok(())
}

fn get_artifacts_from_bxl_build_result(
    bxl_build_result: &StarlarkBxlBuildResult,
    output_stream: &OutputStream,
) -> anyhow::Result<Vec<EnsuredArtifact>> {
    match &bxl_build_result.0 {
        BxlBuildResult::None => Ok(Vec::new()),
        BxlBuildResult::Built(result) => result
            .outputs
            .iter()
            .filter_map(|built| {
                built.as_ref().ok().map(|artifacts| {
                    artifacts
                        .values
                        .iter()
                        .map(|(artifact, _)| EnsuredArtifact::Artifact {
                            artifact: StarlarkArtifact::new(artifact.dupe()),
                            abs: false,
                        })
                })
            })
            .flatten()
            .map(|artifact| try {
                populate_ensured_artifacts(
                    output_stream,
                    EnsuredArtifactOrGroup::Artifact(artifact.clone()),
                )?;
                artifact
            })
            .collect::<anyhow::Result<_>>(),
    }
}
