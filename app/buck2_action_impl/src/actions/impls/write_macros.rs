/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::borrow::Cow;
use std::time::Instant;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_artifact::artifact::build_artifact::BuildArtifact;
use buck2_build_api::actions::Action;
use buck2_build_api::actions::ActionExecutionCtx;
use buck2_build_api::actions::UnregisteredAction;
use buck2_build_api::actions::execute::action_executor::ActionExecutionKind;
use buck2_build_api::actions::execute::action_executor::ActionExecutionMetadata;
use buck2_build_api::actions::execute::action_executor::ActionOutputs;
use buck2_build_api::actions::execute::error::ExecuteError;
use buck2_build_api::artifact_groups::ArtifactGroup;
use buck2_build_api::interpreter::rule_defs::cmd_args::ArtifactPathMapper;
use buck2_build_api::interpreter::rule_defs::cmd_args::CommandLineContext;
use buck2_build_api::interpreter::rule_defs::cmd_args::CommandLineLocation;
use buck2_build_api::interpreter::rule_defs::cmd_args::DefaultCommandLineContext;
use buck2_build_api::interpreter::rule_defs::cmd_args::WriteToFileMacroVisitor;
use buck2_build_api::interpreter::rule_defs::cmd_args::arg_builder::ArgBuilder;
use buck2_build_api::interpreter::rule_defs::cmd_args::value_as::ValueAsCommandLineLike;
use buck2_build_api::interpreter::rule_defs::resolved_macro::ResolvedMacro;
use buck2_build_signals::env::WaitingData;
use buck2_common::file_ops::metadata::TrackedFileDigest;
use buck2_core::category::CategoryRef;
use buck2_core::content_hash::ContentBasedPathHash;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_error::internal_error;
use buck2_execute::artifact::fs::ExecutorFs;
use buck2_execute::execute::command_executor::ActionExecutionTimingData;
use buck2_execute::materialize::materializer::WriteRequest;
use buck2_fs::paths::RelativePathBuf;
use dupe::Dupe;
use indexmap::IndexSet;
use starlark::values::OwnedFrozenValue;
use starlark::values::UnpackValue;

use crate::actions::impls::run::DepFilesPlaceholderArtifactPathMapper;
use crate::actions::impls::write::CommandLineContentBasedInputVisitor;

#[derive(Debug, Allocative)]
pub(crate) struct UnregisteredWriteMacrosToFileAction {
    identifier: String,
    use_dep_files_placeholder_for_content_based_paths: bool,
}

impl UnregisteredWriteMacrosToFileAction {
    pub(crate) fn new(
        identifier: String,
        use_dep_files_placeholder_for_content_based_paths: bool,
    ) -> Self {
        Self {
            identifier,
            use_dep_files_placeholder_for_content_based_paths,
        }
    }
}

impl UnregisteredAction for UnregisteredWriteMacrosToFileAction {
    fn register(
        self: Box<Self>,
        outputs: IndexSet<BuildArtifact>,
        starlark_data: Option<OwnedFrozenValue>,
        _error_handler: Option<OwnedFrozenValue>,
    ) -> buck2_error::Result<Box<dyn Action>> {
        let contents = starlark_data.expect("Action data should be present");

        let action = WriteMacrosToFileAction::new(contents, outputs, *self)?;

        Ok(Box::new(action))
    }
}

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
enum WriteMacrosActionValidationError {
    #[error("At least one output file must be specified for a write macros action")]
    NoOutputsSpecified,
    #[error("Expected command line value, got {0}")]
    ContentsNotCommandLineValue(String),
    #[error(
        "Number of write-to-file macro artifacts during analysis time should be consistent with when those artifacts are created"
    )]
    InconsistentNumberOfMacroArtifacts,
}

#[derive(Debug, Allocative)]
struct WriteMacrosToFileAction {
    contents: OwnedFrozenValue, // StarlarkCmdArgs
    outputs: Box<[BuildArtifact]>,
    inner: UnregisteredWriteMacrosToFileAction,
}

impl WriteMacrosToFileAction {
    fn new(
        contents: OwnedFrozenValue,
        outputs: IndexSet<BuildArtifact>,
        inner: UnregisteredWriteMacrosToFileAction,
    ) -> buck2_error::Result<Self> {
        if outputs.is_empty() {
            Err(WriteMacrosActionValidationError::NoOutputsSpecified.into())
        } else if ValueAsCommandLineLike::unpack_value(contents.value())?.is_none() {
            Err(
                WriteMacrosActionValidationError::ContentsNotCommandLineValue(
                    contents.value().to_repr(),
                )
                .into(),
            )
        } else {
            Ok(Self {
                contents,
                outputs: outputs.into_iter().collect(),
                inner,
            })
        }
    }
}

#[async_trait]
impl Action for WriteMacrosToFileAction {
    fn kind(&self) -> buck2_data::ActionKind {
        buck2_data::ActionKind::WriteMacrosToFile
    }

    fn inputs(&self) -> buck2_error::Result<Cow<'_, [ArtifactGroup]>> {
        if self.inner.use_dep_files_placeholder_for_content_based_paths {
            return Ok(Cow::Borrowed(&[]));
        }

        let mut visitor = CommandLineContentBasedInputVisitor::new();
        ValueAsCommandLineLike::unpack_value(self.contents.value())?
            .unwrap()
            .0
            .visit_artifacts(&mut visitor)?;
        Ok(Cow::Owned(
            visitor.content_based_inputs.into_iter().collect(),
        ))
    }

    fn outputs(&self) -> Cow<'_, [BuildArtifact]> {
        Cow::Borrowed(&self.outputs)
    }

    fn first_output(&self) -> &BuildArtifact {
        // Required to have outputs on construction
        &self.outputs[0]
    }

    fn category(&self) -> CategoryRef<'_> {
        CategoryRef::unchecked_new("write_macros_to_file")
    }

    fn identifier(&self) -> Option<&str> {
        Some(&self.inner.identifier)
    }

    async fn execute(
        &self,
        ctx: &mut dyn ActionExecutionCtx,
        waiting_data: WaitingData,
    ) -> Result<(ActionOutputs, ActionExecutionMetadata), ExecuteError> {
        let mut execution_start = None;

        let values = ctx
            .materializer()
            .declare_write(Box::new(|| {
                execution_start = Some(Instant::now());

                let fs = ctx.executor_fs();

                let mut output_contents = Vec::with_capacity(self.outputs.len());
                let mut macro_writer = MacroToFileWriter::new(&fs, &mut output_contents);

                let command_line = ValueAsCommandLineLike::unpack_value(self.contents.value())?
                    .unwrap()
                    .0;

                if self.inner.use_dep_files_placeholder_for_content_based_paths {
                    command_line.visit_write_to_file_macros(
                        &mut macro_writer,
                        &DepFilesPlaceholderArtifactPathMapper {},
                    )?;
                } else {
                    command_line.visit_write_to_file_macros(
                        &mut macro_writer,
                        &ctx.artifact_path_mapping(None),
                    )?;
                }

                if self.outputs.len() != output_contents.len() {
                    return Err(
                        WriteMacrosActionValidationError::InconsistentNumberOfMacroArtifacts.into(),
                    );
                }

                std::iter::zip(self.outputs.iter(), output_contents.into_iter())
                    .map(|(output, content)| {
                        let content = content.into_bytes();
                        let path = if output.get_path().is_content_based_path() {
                            let digest = TrackedFileDigest::from_content(
                                &content,
                                ctx.digest_config().cas_digest_config(),
                            );
                            fs.fs().resolve_build(
                                output.get_path(),
                                Some(&ContentBasedPathHash::new(digest.raw_digest().as_bytes())?),
                            )?
                        } else {
                            fs.fs().resolve_build(output.get_path(), None)?
                        };
                        Ok(WriteRequest {
                            path,
                            content,
                            is_executable: false,
                        })
                    })
                    .collect::<buck2_error::Result<_>>()
            }))
            .await?;

        let wall_time = Instant::now()
            - execution_start
                .ok_or_else(|| internal_error!("Action did not set execution_start"))?;

        let output_values = std::iter::zip(self.outputs.iter(), values)
            .map(|(output, value)| (output.get_path().dupe(), value))
            .collect();

        Ok((
            ActionOutputs::new(output_values),
            ActionExecutionMetadata {
                execution_kind: ActionExecutionKind::Simple,
                timing: ActionExecutionTimingData { wall_time },
                input_files_bytes: None,
                waiting_data,
            },
        ))
    }
}

struct MacroToFileWriter<'a> {
    fs: &'a ExecutorFs<'a>,
    outputs: &'a mut Vec<String>,
    relative_to_path: Option<RelativePathBuf>,
}

impl<'a> MacroToFileWriter<'a> {
    fn new(fs: &'a ExecutorFs<'a>, outputs: &'a mut Vec<String>) -> Self {
        Self {
            fs,
            outputs,
            relative_to_path: None,
        }
    }
}

impl WriteToFileMacroVisitor for MacroToFileWriter<'_> {
    fn visit_write_to_file_macro(
        &mut self,
        resolved_macro: &ResolvedMacro,
        artifact_path_mapping: &dyn ArtifactPathMapper,
    ) -> buck2_error::Result<()> {
        let content = {
            let mut builder = MacroOutput {
                result: String::new(),
            };
            let mut ctx = MacroContext::new(self.fs, &self.relative_to_path);
            resolved_macro.add_to_arg(&mut builder, &mut ctx, artifact_path_mapping)?;
            builder.result
        };

        self.outputs.push(content);
        Ok(())
    }

    fn set_current_relative_to_path(
        &mut self,
        generate: &dyn Fn(&dyn CommandLineContext) -> buck2_error::Result<Option<RelativePathBuf>>,
    ) -> buck2_error::Result<()> {
        self.relative_to_path = generate(&DefaultCommandLineContext::new(self.fs))?;
        Ok(())
    }
}

struct MacroContext<'a> {
    fs: &'a ExecutorFs<'a>,
    maybe_relative_to_path: &'a Option<RelativePathBuf>,
}

impl<'a> MacroContext<'a> {
    fn new(fs: &'a ExecutorFs, maybe_relative_to_path: &'a Option<RelativePathBuf>) -> Self {
        Self {
            fs,
            maybe_relative_to_path,
        }
    }

    fn relativize_path(&self, path: ProjectRelativePathBuf) -> RelativePathBuf {
        if let Some(relative_to_path) = self.maybe_relative_to_path {
            relative_to_path.relative(path)
        } else {
            path.into()
        }
    }
}

impl CommandLineContext for MacroContext<'_> {
    fn resolve_project_path(
        &self,
        path: ProjectRelativePathBuf,
    ) -> buck2_error::Result<CommandLineLocation<'_>> {
        Ok(CommandLineLocation::from_relative_path(
            self.relativize_path(path),
            self.fs.path_separator(),
        ))
    }

    fn fs(&self) -> &ExecutorFs<'_> {
        self.fs
    }

    fn next_macro_file_path(&mut self) -> buck2_error::Result<RelativePathBuf> {
        unreachable!("write-to-file macros could not be nested")
    }
}

// TODO(torozco): Just remove this, and ArgBuilder
struct MacroOutput {
    result: String,
}

impl ArgBuilder for MacroOutput {
    fn push_str(&mut self, s: &str) {
        self.result.push_str(s)
    }
}
