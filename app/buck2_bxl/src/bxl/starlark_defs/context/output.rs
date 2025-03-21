/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cell::RefCell;
use std::io::Write;
use std::iter;
use std::ops::DerefMut;
use std::rc::Rc;

use allocative::Allocative;
use buck2_build_api::artifact_groups::ArtifactGroup;
use buck2_build_api::bxl::build_result::BxlBuildResult;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_artifact::StarlarkArtifact;
use buck2_build_api::interpreter::rule_defs::cmd_args::value_as::ValueAsCommandLineLike;
use buck2_build_api::interpreter::rule_defs::cmd_args::CommandLineArgLike;
use buck2_build_api::interpreter::rule_defs::cmd_args::SimpleCommandLineArtifactVisitor;
use buck2_build_api::interpreter::rule_defs::cmd_args::StarlarkCommandLineInputs;
use buck2_core::fs::artifact_path_resolver::ArtifactFs;
use buck2_core::fs::project::ProjectRoot;
use buck2_error::buck2_error;
use buck2_error::starlark_error::from_starlark_with_options;
use buck2_error::BuckErrorContext;
use buck2_execute::path::artifact_path::ArtifactPath;
use derivative::Derivative;
use derive_more::Display;
use dupe::Dupe;
use futures::FutureExt;
use gazebo::prelude::VecExt;
use serde::ser::SerializeSeq;
use serde::Serialize;
use serde::Serializer;
use starlark::any::ProvidesStaticType;
use starlark::collections::SmallSet;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::eval::Evaluator;
use starlark::starlark_module;
use starlark::values::dict::Dict;
use starlark::values::dict::DictRef;
use starlark::values::dict::UnpackDictEntries;
use starlark::values::list::ListRef;
use starlark::values::list::UnpackList;
use starlark::values::none::NoneType;
use starlark::values::record::Record;
use starlark::values::starlark_value;
use starlark::values::structs::StructRef;
use starlark::values::tuple::TupleRef;
use starlark::values::tuple::UnpackTuple;
use starlark::values::type_repr::StarlarkTypeRepr;
use starlark::values::AllocValue;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::ValueLike;

use crate::bxl::starlark_defs::artifacts::ArtifactArg;
use crate::bxl::starlark_defs::artifacts::EnsuredArtifact;
use crate::bxl::starlark_defs::artifacts::EnsuredArtifactGroup;
use crate::bxl::starlark_defs::build_result::StarlarkBxlBuildResult;
use crate::bxl::starlark_defs::context::build::StarlarkProvidersArtifactIterable;
use crate::bxl::starlark_defs::context::starlark_async::BxlDiceComputations;
use crate::bxl::starlark_defs::eval_extra::BxlEvalExtra;

#[derive(
    ProvidesStaticType,
    Derivative,
    Display,
    Trace,
    NoSerialize,
    Allocative
)]
#[display("{:?}", self)]
#[derivative(Debug)]
pub(crate) struct OutputStream {
    #[derivative(Debug = "ignore")]
    #[trace(unsafe_ignore)]
    #[allocative(skip)]
    pub(crate) sink: Rc<RefCell<dyn Write>>,
    #[derivative(Debug = "ignore")]
    #[trace(unsafe_ignore)]
    #[allocative(skip)]
    pub(crate) error_sink: Rc<RefCell<dyn Write>>,
    #[trace(unsafe_ignore)]
    artifacts_to_ensure: RefCell<Option<SmallSet<EnsuredArtifactOrGroup>>>,
    #[derivative(Debug = "ignore")]
    pub(crate) project_fs: ProjectRoot,
    #[derivative(Debug = "ignore")]
    pub(crate) artifact_fs: ArtifactFs,
}

/// We can ensure either an `Artifact` or an `ArtifactGroup`. When we want to ensure a `CommandLineArgLike` object,
/// the result of visiting its artifacts is a list of `ArtifactGroup`s. It's convenient to preserve the group rather
/// than extract the individual `Artifact`s from it, for perf/memory optimizations.
#[derive(Display, Debug, Allocative, Hash, Eq, PartialEq)]
pub(crate) enum EnsuredArtifactOrGroup {
    Artifact(EnsuredArtifact),
    ArtifactGroup(ArtifactGroup),
}

impl EnsuredArtifactOrGroup {
    pub(crate) fn into_artifact_groups(self) -> buck2_error::Result<Vec<ArtifactGroup>> {
        match self {
            EnsuredArtifactOrGroup::Artifact(artifact) => {
                let as_artifact = artifact.as_artifact();
                let bound_artifact = as_artifact.get_bound_artifact()?;
                let associated_artifacts = as_artifact.get_associated_artifacts();

                Ok(associated_artifacts
                    .iter()
                    .flat_map(|v| v.iter())
                    .cloned()
                    .chain(iter::once(ArtifactGroup::Artifact(bound_artifact)))
                    .collect::<Vec<_>>())
            }
            EnsuredArtifactOrGroup::ArtifactGroup(group) => Ok(vec![group]),
        }
    }
}

impl OutputStream {
    pub(crate) fn new(
        project_fs: ProjectRoot,
        artifact_fs: ArtifactFs,
        sink: Rc<RefCell<dyn Write>>,
        error_sink: Rc<RefCell<dyn Write>>,
    ) -> Self {
        Self {
            sink,
            error_sink,
            artifacts_to_ensure: RefCell::new(Some(Default::default())),
            project_fs,
            artifact_fs,
        }
    }

    pub(crate) fn take_artifacts(&self) -> SmallSet<EnsuredArtifactOrGroup> {
        self.artifacts_to_ensure.borrow_mut().take().unwrap()
    }

    fn print<'v>(
        &self,
        args: impl Iterator<Item = Value<'v>>,
        sep: &'v str,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> buck2_error::Result<()> {
        let mut first = true;
        let mut write = |d: &dyn Display| -> buck2_error::Result<()> {
            if !first {
                write!(self.sink.borrow_mut(), "{}{}", sep, d)?;
            } else {
                write!(self.sink.borrow_mut(), "{}", d)?;
                first = false;
            }
            Ok(())
        };

        for arg in args {
            if let Some(ensured) = <&EnsuredArtifact>::unpack_value(arg)? {
                let path = get_artifact_path_display(
                    ensured.get_artifact_path(),
                    ensured.abs(),
                    &self.project_fs,
                    &self.artifact_fs,
                )?;
                write(&path)?;
            } else if let Some(ensured) = <&EnsuredArtifactGroup>::unpack_value(arg)? {
                BxlEvalExtra::from_context(eval)?
                    .dice
                    .borrow_mut()
                    .via(|dice| {
                        ensured
                            .visit_artifact_path_without_associated_deduped(
                                |artifact_path, abs| {
                                    let path = get_artifact_path_display(
                                        artifact_path,
                                        abs,
                                        &self.project_fs,
                                        &self.artifact_fs,
                                    )?;
                                    write(&path)
                                },
                                dice,
                            )
                            .boxed_local()
                    })?;
            } else {
                write(&arg.to_str())?;
            }
        }

        writeln!(self.sink.borrow_mut())?;

        Ok(())
    }

    fn print_json<'v>(
        &self,
        value: Value<'v>,
        pretty: bool,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> buck2_error::Result<()> {
        /// A wrapper with a Serialize instance so we can pass down the necessary context.
        struct SerializeValue<'a, 'v, 'd> {
            value: Value<'v>,
            artifact_fs: &'a ArtifactFs,
            project_fs: &'a ProjectRoot,
            async_ctx: &'a Rc<RefCell<dyn BxlDiceComputations + 'd>>,
        }

        impl<'v> SerializeValue<'_, 'v, '_> {
            fn with_value(&self, x: Value<'v>) -> Self {
                Self {
                    value: x,
                    artifact_fs: self.artifact_fs,
                    project_fs: self.project_fs,
                    async_ctx: self.async_ctx,
                }
            }
        }

        impl Serialize for SerializeValue<'_, '_, '_> {
            fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where
                S: Serializer,
            {
                if let Some(ensured) = <&EnsuredArtifact>::unpack_value(self.value)
                    .map_err(|e| serde::ser::Error::custom(format!("{:#}", e)))?
                {
                    let path = get_artifact_path_display(
                        ensured.get_artifact_path(),
                        ensured.abs(),
                        self.project_fs,
                        self.artifact_fs,
                    )
                    .map_err(|err| serde::ser::Error::custom(format!("{:#}", err)))?;
                    serializer.serialize_str(&path)
                } else if let Some(ensured) = <&EnsuredArtifactGroup>::unpack_value(self.value)
                    .map_err(|e| serde::ser::Error::custom(format!("{:#}", e)))?
                {
                    let mut seq_ser = serializer.serialize_seq(None)?;

                    self.async_ctx
                        .borrow_mut()
                        .via(|dice| {
                            ensured
                                .visit_artifact_path_without_associated_deduped(
                                    |artifact_path, abs| {
                                        let path = get_artifact_path_display(
                                            artifact_path,
                                            abs,
                                            self.project_fs,
                                            self.artifact_fs,
                                        )?;
                                        seq_ser.serialize_element(&path).map_err(|err| {
                                            buck2_error!(
                                                buck2_error::ErrorTag::Tier0,
                                                "{}",
                                                format!("{:#}", err)
                                            )
                                        })?;
                                        Ok(())
                                    },
                                    dice,
                                )
                                .boxed_local()
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

        let writer = if pretty {
            serde_json::to_writer_pretty
        } else {
            serde_json::to_writer
        };
        writer(
            self.sink.borrow_mut().deref_mut(),
            &SerializeValue {
                value,
                artifact_fs: &self.artifact_fs,
                project_fs: &self.project_fs,
                async_ctx: &BxlEvalExtra::from_context(eval)?.dice,
            },
        )
        .buck_error_context("Error writing to JSON for `write_json`")?;

        writeln!(self.sink.borrow_mut())?;

        Ok(())
    }
}

#[starlark_value(type = "bxl.OutputStream", StarlarkTypeRepr, UnpackValue)]
impl<'v> StarlarkValue<'v> for OutputStream {
    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(output_stream_methods)
    }
}

impl<'v> AllocValue<'v> for OutputStream {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_complex_no_freeze(self)
    }
}

#[derive(StarlarkTypeRepr, UnpackValue)]
enum EnsureMultipleArtifactsArg<'v> {
    None(NoneType),
    EnsuredArtifactArgs(UnpackList<ArtifactArg<'v>>),
    ProvidersArtifactIterable(&'v StarlarkProvidersArtifactIterable<'v>),
    BxlBuildResult(&'v StarlarkBxlBuildResult),
    Dict(UnpackDictEntries<Value<'v>, &'v StarlarkBxlBuildResult>),
    CmdLine(ValueAsCommandLineLike<'v>),
}

/// The output stream for bxl to print values to the console as their result
#[starlark_module]
fn output_stream_methods(builder: &mut MethodsBuilder) {
    /// Outputs results to the console via stdout. These outputs are considered to be the results
    /// of a bxl script, which will be displayed to stdout by buck2 even when the script is cached.
    /// Accepts an optional separator that defaults to " ".
    ///
    /// Prints that are not result of the bxl should be printed via stderr via the stdlib `print`
    /// and `pprint`. Note that `ctx.output.print()` is intended for simple outputs. For more complex
    /// outputs, the recommendation would be to write them to a file.
    ///
    /// Sample usage:
    /// ```python
    /// def _impl_print(ctx):
    ///     ctx.output.print("test")
    /// ```
    fn print<'v>(
        this: &'v OutputStream,
        #[starlark(args)] args: UnpackTuple<Value<'v>>,
        #[starlark(default = " ")] sep: &'v str,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> starlark::Result<NoneType> {
        this.print(args.into_iter(), sep, eval)?;

        Ok(NoneType)
    }

    /// Outputs results to the console via stdout as pretty-printed json. Pretty
    /// printing can be turned off by the `pretty` keyword-only parameter.
    /// These outputs are considered to be the results of a bxl script, which will be displayed to
    /// stdout by buck2 even when the script is cached.
    ///
    /// Prints that are not result of the bxl should be printed via stderr via the stdlib `print`
    /// and `pprint`.
    ///
    /// Sample usage:
    /// ```python
    /// def _impl_print_json(ctx):
    ///     outputs = {}
    ///     outputs.update({"foo": bar})
    ///     ctx.output.print_json("test")
    /// ```
    fn print_json<'v>(
        this: &'v OutputStream,
        value: Value<'v>,
        #[starlark(require=named, default=true)] pretty: bool,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> starlark::Result<NoneType> {
        this.print_json(value, pretty, eval)?;

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
    /// ```python
    /// def _impl_ensure(ctx):
    ///     actions = ctx.bxl_actions().actions
    ///     output = actions.write("my_output", "my_content")
    ///     ensured = ctx.output.ensure(output)
    ///     ctx.output.print(ensured)
    /// ```
    fn ensure<'v>(
        this: &OutputStream,
        artifact: ArtifactArg<'v>,
    ) -> starlark::Result<EnsuredArtifact> {
        let artifact = artifact.into_ensured_artifact();
        populate_ensured_artifacts(this, EnsuredArtifactOrGroup::Artifact(artifact.clone()))?;

        Ok(artifact)
    }

    /// Same as `ensure`, but for multiple artifacts. Will preserve the shape of the inputs (i.e. if the resulting
    /// `Dict` of a `ctx.build()` is passed in, the output will be a `Dict` where the key is preserved,
    /// and the values are converted to `ensured_artifact`s).
    ///
    /// Note that is slower to loop through objects and ensure them one by one than it is to call `ensure_multiple()`
    /// on all the objects at once (if possible).
    /// So, it is suggested to use this method when you are only ensuring a few individual artifacts that are not stored in an iterable.
    ///
    /// Sample usage:
    /// ```python
    /// def _impl_ensure_multiple(ctx):
    ///     outputs = {}
    ///     for target, value in ctx.build(ctx.cli_args.target).items():
    ///     outputs.update({target.raw_target(): ctx.output.ensure_multiple(value.artifacts())})
    ///     ctx.output.print_json(outputs)
    /// ```
    fn ensure_multiple<'v>(
        this: &'v OutputStream,
        // TODO(nga): must be either positional or named.
        artifacts: EnsureMultipleArtifactsArg<'v>,
        heap: &'v Heap,
    ) -> starlark::Result<Value<'v>> {
        match artifacts {
            EnsureMultipleArtifactsArg::None(_) => Ok(heap.alloc(Vec::<EnsuredArtifact>::new())),
            EnsureMultipleArtifactsArg::EnsuredArtifactArgs(list) => {
                let artifacts: Vec<EnsuredArtifact> = list.items.into_try_map(|artifact| {
                    let artifact = artifact.into_ensured_artifact();
                    populate_ensured_artifacts(
                        this,
                        EnsuredArtifactOrGroup::Artifact(artifact.clone()),
                    )?;

                    Ok::<EnsuredArtifact, buck2_error::Error>(artifact)
                })?;

                Ok(heap.alloc(artifacts))
            }
            EnsureMultipleArtifactsArg::ProvidersArtifactIterable(artifact_gen) => {
                Ok(heap.alloc(get_artifacts_from_bxl_build_result(
                    artifact_gen
                        .0
                        .downcast_ref::<StarlarkBxlBuildResult>()
                        .unwrap(),
                    this,
                )?))
            }
            EnsureMultipleArtifactsArg::BxlBuildResult(bxl_build_result) => {
                Ok(heap.alloc(get_artifacts_from_bxl_build_result(bxl_build_result, this)?))
            }
            EnsureMultipleArtifactsArg::Dict(build_result_dict) => Ok(heap.alloc(Dict::new(
                build_result_dict
                    .entries
                    .into_iter()
                    .map(|(label, bxl_build_result)| {
                        Ok((
                            label.get_hashed().map_err(|e| {
                                from_starlark_with_options(
                                    e,
                                    buck2_error::starlark_error::NativeErrorHandling::Unknown,
                                    false,
                                )
                            })?,
                            heap.alloc(get_artifacts_from_bxl_build_result(
                                bxl_build_result,
                                this,
                            )?),
                        ))
                    })
                    .collect::<buck2_error::Result<_>>()?,
            ))),
            EnsureMultipleArtifactsArg::CmdLine(cmd_line) => {
                // TODO(nga): we should not be doing that here.
                //   If we pass random string to this function,
                //   it will be interpreted as a command line without inputs,
                //   and this function will return empty `EnsuredArtifactGroup`.
                let inputs = get_cmd_line_inputs(cmd_line.0)?;
                let mut result = Vec::new();

                for artifact_group in &inputs.inputs {
                    populate_ensured_artifacts(
                        this,
                        EnsuredArtifactOrGroup::ArtifactGroup(artifact_group.dupe()),
                    )?;
                    result.push(artifact_group.dupe());
                }

                Ok(heap.alloc(EnsuredArtifactGroup::new(result, false, heap)))
            }
        }
    }
}

pub(crate) fn get_cmd_line_inputs<'v>(
    cmd_line: &'v dyn CommandLineArgLike,
) -> buck2_error::Result<StarlarkCommandLineInputs> {
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
) -> buck2_error::Result<String> {
    let resolved = artifact_path.resolve(artifact_fs)?;
    Ok(if abs {
        project_fs.resolve(&resolved).to_string()
    } else {
        resolved.to_string()
    })
}

fn populate_ensured_artifacts(
    output_stream: &OutputStream,
    ensured: EnsuredArtifactOrGroup,
) -> buck2_error::Result<()> {
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
) -> buck2_error::Result<Vec<EnsuredArtifact>> {
    match &bxl_build_result.0 {
        BxlBuildResult::None => Ok(Vec::new()),
        BxlBuildResult::Built { result, .. } => result
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
            .collect::<buck2_error::Result<_>>(),
    }
}
