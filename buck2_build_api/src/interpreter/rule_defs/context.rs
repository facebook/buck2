/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cell::RefCell;
use std::cell::RefMut;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::sync::Arc;

use buck2_core::category::Category;
use buck2_core::fs::paths::ForwardRelativePathBuf;
use buck2_core::fs::paths::RelativePathBuf;
use buck2_execute::materialize::http::Checksum;
use buck2_interpreter::types::label::Label;
use derive_more::Display;
use gazebo::any::ProvidesStaticType;
use gazebo::prelude::*;
use indexmap::indexset;
use indexmap::IndexSet;
use sha1::Digest;
use sha1::Sha1;
use starlark::collections::SmallMap;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::eval::Evaluator;
use starlark::starlark_type;
use starlark::values::dict::Dict;
use starlark::values::docs::DocItem;
use starlark::values::function::FUNCTION_TYPE;
use starlark::values::none::NoneOr;
use starlark::values::none::NoneType;
use starlark::values::structs::Struct;
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
use starlark::values::ValueOf;
use starlark::values::ValueTyped;
use thiserror::Error;

use crate::actions::artifact::OutputArtifact;
use crate::actions::copy::CopyMode;
use crate::actions::copy::UnregisteredCopyAction;
use crate::actions::download_file::UnregisteredDownloadFileAction;
use crate::actions::run::dep_files::RunActionDepFiles;
use crate::actions::run::new_executor_preference;
use crate::actions::run::MetadataParameter;
use crate::actions::run::UnregisteredRunAction;
use crate::actions::symlinked_dir::UnregisteredSymlinkedDirAction;
use crate::actions::write::UnregisteredWriteAction;
use crate::actions::write_json::UnregisteredWriteJsonAction;
use crate::actions::write_macros::UnregisteredWriteMacrosToFileAction;
use crate::analysis::registry::AnalysisRegistry;
use crate::artifact_groups::ArtifactGroup;
use crate::attrs::resolve::attr_type::arg::value::ResolvedMacro;
use crate::interpreter::rule_defs::artifact::StarlarkArtifact;
use crate::interpreter::rule_defs::artifact::StarlarkDeclaredArtifact;
use crate::interpreter::rule_defs::artifact::StarlarkOutputArtifact;
use crate::interpreter::rule_defs::artifact::ValueAsArtifactLike;
use crate::interpreter::rule_defs::artifact_tagging::ArtifactTag;
use crate::interpreter::rule_defs::cmd_args::CommandLineArgLike;
use crate::interpreter::rule_defs::cmd_args::CommandLineArtifactVisitor;
use crate::interpreter::rule_defs::cmd_args::CommandLineBuilderContext;
use crate::interpreter::rule_defs::cmd_args::SimpleCommandLineArtifactVisitor;
use crate::interpreter::rule_defs::cmd_args::StarlarkCommandLine;
use crate::interpreter::rule_defs::cmd_args::ValueAsCommandLineLike;
use crate::interpreter::rule_defs::cmd_args::WriteToFileMacroVisitor;

#[derive(Error, Debug)]
enum DownloadFileError {
    #[error("Must pass in at least one checksum (e.g. `sha1 = ...`)")]
    MissingChecksum,
}

#[derive(Error, Debug)]
enum DynamicOutputError {
    #[error("Output list may not be empty")]
    EmptyOutput,
    #[error("List of dynamic inputs may not be empty")]
    EmptyDynamic,
    #[error("Final argument must be a function, got `{0}`")]
    NotAFunction(String),
}

/// Functions to allow users to interact with the Actions registry.
///
/// Accessed via `ctx.actions.<function>`
#[derive(ProvidesStaticType, Debug, Display, Trace, NoSerialize)]
#[display(fmt = "<ctx.actions>")]
pub struct AnalysisActions<'v> {
    // Use a RefCell/Option so when we are done with it, without obtaining exclusive access,
    // we can take the internal state without having to clone it.
    pub state: RefCell<Option<AnalysisRegistry<'v>>>,
    // Copies from the ctx, so we can capture them for `dynamic`.
    pub attributes: Value<'v>,
}

impl<'v> StarlarkTypeRepr for &'v AnalysisActions<'v> {
    fn starlark_type_repr() -> String {
        AnalysisActions::get_type_starlark_repr()
    }
}

impl<'v> UnpackValue<'v> for &'v AnalysisActions<'v> {
    fn unpack_value(x: Value<'v>) -> Option<&'v AnalysisActions<'v>> {
        x.downcast_ref()
    }
}

impl<'v> AnalysisActions<'v> {
    pub fn state(&self) -> RefMut<AnalysisRegistry<'v>> {
        RefMut::map(self.state.borrow_mut(), |x| {
            x.as_mut().expect("state to be present during execution")
        })
    }
}

impl<'v> StarlarkValue<'v> for AnalysisActions<'v> {
    starlark_type!("actions");

    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(register_context_actions)
    }
}

impl<'v> AllocValue<'v> for AnalysisActions<'v> {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_complex_no_freeze(self)
    }
}

struct RefAnalysisAction<'v>(&'v AnalysisActions<'v>);

impl<'v> StarlarkTypeRepr for RefAnalysisAction<'v> {
    fn starlark_type_repr() -> String {
        AnalysisActions::starlark_type_repr()
    }
}

impl<'v> UnpackValue<'v> for RefAnalysisAction<'v> {
    fn unpack_value(value: Value<'v>) -> Option<Self> {
        Some(RefAnalysisAction(
            value.downcast_ref::<AnalysisActions>().unwrap(),
        ))
    }
}

#[derive(ProvidesStaticType, Debug, Display, Trace, NoSerialize)]
#[display(fmt = "<ctx>")]
pub struct AnalysisContext<'v> {
    attributes: Value<'v>, // A struct
    actions: Value<'v>,    // AnalysisActions
    label: Option<ValueTyped<'v, Label<'v>>>,
    artifacts: Value<'v>, // SmallMap of artifact to artifact value
    outputs: Value<'v>,   // SmallMap of outputs to unbound output
}

/// Simple holder for documetnation from AnalysisContext
pub struct ContextDocs {
    /// Docs for ctx
    pub context: Option<DocItem>,
    /// Docs for ctx.actions
    pub actions: Option<DocItem>,
}

impl<'v> AnalysisContext<'v> {
    /// The context that is provided to users' UDR implementation functions. Comprised of things like attribute values, actions, etc
    pub(crate) fn new(
        heap: &'v Heap,
        attributes: Value<'v>,
        label: Option<ValueTyped<'v, Label<'v>>>,
        registry: AnalysisRegistry<'v>,
    ) -> Self {
        Self::new_dynamic(
            heap,
            attributes,
            label,
            registry,
            Dict::default(),
            Dict::default(),
        )
    }

    pub(crate) fn new_dynamic(
        heap: &'v Heap,
        attributes: Value<'v>,
        label: Option<ValueTyped<'v, Label<'v>>>,
        registry: AnalysisRegistry<'v>,
        artifacts: Dict<'v>,
        outputs: Dict<'v>,
    ) -> Self {
        // Check the types match what the user expects. None is allowed for bxl
        assert!(Struct::from_value(attributes).is_some() || attributes.is_none());

        Self {
            attributes,
            actions: heap.alloc(AnalysisActions {
                state: RefCell::new(Some(registry)),
                attributes,
            }),
            label,
            artifacts: heap.alloc(artifacts),
            outputs: heap.alloc(outputs),
        }
    }

    /// Must take an `AnalysisContext` which has never had `take_state` called on it before.
    pub(crate) fn take_state(value: Value<'v>) -> AnalysisRegistry<'v> {
        value
            .downcast_ref::<AnalysisContext>()
            .expect("ctx was an AnalysisContext")
            .actions
            .downcast_ref::<AnalysisActions>()
            .expect("ctx.actions to be AnalysisActions")
            .state
            .borrow_mut()
            .take()
            .expect("nothing to have stolen state yet")
    }

    /// Returns the documentation for AnalysisContext and AnalysisActions based on their get_methods() calls.
    ///
    /// That is the only reason that this function should be called.
    pub fn ctx_documentation() -> ContextDocs {
        static CTX_METHODS: MethodsStatic = MethodsStatic::new();
        static ACTIONS_METHODS: MethodsStatic = MethodsStatic::new();

        let context = CTX_METHODS
            .methods(register_context)
            .map(|methods| methods.documentation());
        let actions = ACTIONS_METHODS
            .methods(register_context_actions)
            .map(|methods| methods.documentation());

        ContextDocs { context, actions }
    }
}

impl<'v> StarlarkValue<'v> for AnalysisContext<'v> {
    starlark_type!("context");

    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(register_context)
    }
}

impl<'v> AllocValue<'v> for AnalysisContext<'v> {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_complex_no_freeze(self)
    }
}

struct RefAnalysisContext<'v>(&'v AnalysisContext<'v>);

impl<'v> StarlarkTypeRepr for RefAnalysisContext<'v> {
    fn starlark_type_repr() -> String {
        AnalysisContext::starlark_type_repr()
    }
}

impl<'v> UnpackValue<'v> for RefAnalysisContext<'v> {
    fn unpack_value(value: Value<'v>) -> Option<Self> {
        Some(RefAnalysisContext(
            value.downcast_ref::<AnalysisContext>().unwrap(),
        ))
    }
}

#[starlark_module]
fn register_context(builder: &mut MethodsBuilder) {
    #[starlark(attribute)]
    fn attrs<'v>(this: RefAnalysisContext) -> anyhow::Result<Value<'v>> {
        Ok(this.0.attributes)
    }

    #[starlark(attribute)]
    fn actions<'v>(this: RefAnalysisContext) -> anyhow::Result<Value<'v>> {
        Ok(this.0.actions)
    }

    #[starlark(attribute)]
    fn label<'v>(this: RefAnalysisContext) -> anyhow::Result<Value<'v>> {
        Ok(this.0.label.map_or(Value::new_none(), |v| v.to_value()))
    }

    #[starlark(attribute)]
    fn outputs<'v>(this: RefAnalysisContext) -> anyhow::Result<Value<'v>> {
        Ok(this.0.outputs)
    }

    #[starlark(attribute)]
    fn artifacts<'v>(this: RefAnalysisContext) -> anyhow::Result<Value<'v>> {
        Ok(this.0.artifacts)
    }
}

#[derive(Debug, Error)]
enum RunActionError {
    #[error("expected at least one output artifact, did not get any")]
    NoOutputsSpecified,
    #[error("`weight` must be a positive integer, got `{0}`")]
    InvalidWeight(i32),
    #[error("`dep_files` values must be artifact tags, got `{}` for key `{}`", .value, .key)]
    InvalidDepFileTag { key: String, value: String },
    #[error("`dep_files` value with key `{}` has an invalid count of associated oututs. Expected 1, got {}.", .key, .count)]
    InvalidDepFileOutputs { key: String, count: usize },
    #[error("`dep_files` with keys `{}` and {} are using the same tag", .first, .second)]
    ConflictingDepFiles { first: String, second: String },
    #[error(
        "missing `metadata_path` parameter which is required when `metadata_env_var` parameter is present"
    )]
    MetadataPathMissing,
    #[error(
        "missing `metadata_env_var` parameter which is required when `metadata_path` parameter is present"
    )]
    MetadataEnvVarMissing,
}

#[derive(Debug, Error)]
enum WriteActionError {
    #[error(
        "Argument type attributes detected in a content to be written into a file, but support for arguments was not turned on. Use `allow_args` parameter to turn on the support for arguments."
    )]
    ArgAttrsDetectedButNotAllowed,
}

fn create_dir_tree<'v>(
    eval: &mut Evaluator<'v, '_>,
    this: &AnalysisActions<'v>,
    output: Value<'v>,
    srcs: Value<'v>,
    copy: bool,
) -> anyhow::Result<Value<'v>> {
    // validate that the moves are valid, and move them into inputs
    let action = UnregisteredSymlinkedDirAction::new(copy, srcs)?;
    let inputs = action.inputs();

    let mut this = this.state();
    let (output_value, output_artifact) = this.get_or_declare_output(eval, output, "output")?;
    this.register_action(inputs, indexset![output_artifact], action, None)?;

    Ok(output_value)
}

fn copy_file<'v>(
    eval: &mut Evaluator<'v, '_>,
    this: &AnalysisActions<'v>,
    dest: Value<'v>,
    src: Value<'v>,
    copy: CopyMode,
) -> anyhow::Result<Value<'v>> {
    let src = src
        .as_artifact()
        .ok_or_else(|| ValueError::IncorrectParameterTypeNamed("src".to_owned()))?;

    let mut this = this.state();
    let (output_value, output_artifact) = this.get_or_declare_output(eval, dest, "dest")?;

    this.register_action(
        indexset![ArtifactGroup::Artifact(src.get_bound_deprecated()?)],
        indexset![output_artifact],
        UnregisteredCopyAction::new(copy),
        None,
    )?;
    Ok(output_value)
}

#[starlark_module]
fn register_context_actions(builder: &mut MethodsBuilder) {
    fn declare_output<'v>(
        this: &AnalysisActions<'v>,
        #[starlark(require = pos)] prefix: &str,
        #[starlark(require = pos)] filename: Option<&str>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<StarlarkDeclaredArtifact> {
        let artifact = match filename {
            None => this.state().declare_output(None, prefix)?,
            Some(filename) => this.state().declare_output(Some(prefix), filename)?,
        };

        Ok(StarlarkDeclaredArtifact::new(
            eval.call_stack_top_location(),
            artifact,
        ))
    }

    fn write_json<'v>(
        this: &AnalysisActions<'v>,
        #[starlark(require = pos)] output: Value<'v>,
        #[starlark(require = pos)] content: Value<'v>,
        #[starlark(require = named, default = false)] with_inputs: bool,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        let mut this = this.state();
        let (output_value, output_artifact) = this.get_or_declare_output(eval, output, "output")?;

        UnregisteredWriteJsonAction::validate(content)?;
        this.register_action(
            IndexSet::new(),
            indexset![output_artifact],
            UnregisteredWriteJsonAction::new(),
            Some(content),
        )?;
        // TODO(cjhopman): The with_inputs thing can go away once we have artifact dependencies (we'll still
        // need the UnregisteredWriteJsonAction::cli() to represent the dependency though).
        if with_inputs {
            let cli = UnregisteredWriteJsonAction::cli(output_value, content)?;
            Ok(eval.heap().alloc(cli))
        } else {
            Ok(output_value)
        }
    }

    fn write<'v>(
        this: &AnalysisActions<'v>,
        #[starlark(require = pos)] output: Value<'v>,
        #[starlark(require = pos)] content: Value<'v>,
        #[starlark(require = named, default = false)] is_executable: bool,
        #[starlark(require = named, default = false)] allow_args: bool,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        fn count_write_to_file_macros(
            args_allowed: bool,
            cli: &dyn CommandLineArgLike,
        ) -> anyhow::Result<u32> {
            if !args_allowed && cli.contains_arg_attr() {
                return Err(anyhow::anyhow!(
                    WriteActionError::ArgAttrsDetectedButNotAllowed
                ));
            }

            struct WriteToFileMacrosCounter {
                count: u32,
            }

            impl WriteToFileMacroVisitor for WriteToFileMacrosCounter {
                fn visit_write_to_file_macro(&mut self, _m: &ResolvedMacro) -> anyhow::Result<()> {
                    self.count += 1;
                    Ok(())
                }

                fn set_current_relative_to_path(
                    &mut self,
                    _gen: &dyn Fn(
                        &dyn CommandLineBuilderContext,
                    ) -> anyhow::Result<Option<RelativePathBuf>>,
                ) -> anyhow::Result<()> {
                    Ok(())
                }
            }

            let mut counter = WriteToFileMacrosCounter { count: 0 };
            cli.visit_write_to_file_macros(&mut counter)?;
            Ok(counter.count)
        }

        let (content_cli, written_macro_count) =
            if let Some(content_arg) = content.as_command_line() {
                let count = count_write_to_file_macros(allow_args, content_arg)?;
                (content, count)
            } else {
                let cli = StarlarkCommandLine::try_from_value(content)?;
                let count = count_write_to_file_macros(allow_args, &cli)?;
                (eval.heap().alloc(cli), count)
            };

        let mut this = this.state();
        let (output_value, output_artifact) = this.get_or_declare_output(eval, output, "output")?;

        let written_macro_files = if written_macro_count > 0 {
            let macro_directory_path = {
                // There might be several write actions at once, use write action output hash to deterministically avoid collisions for .macro files.
                let digest = output_artifact
                    .get_path()
                    .with_full_path(|path| Sha1::digest(path.as_str().as_bytes()));
                let sha = hex::encode(digest);
                format!("__macros/{}", sha)
            };

            let mut written_macro_files = indexset![];
            for i in 0..written_macro_count {
                let macro_file =
                    this.declare_output(None, &format!("{}/{}.macro", &macro_directory_path, i))?;
                written_macro_files.insert(macro_file);
            }

            let state = &mut *this;
            let action = UnregisteredWriteMacrosToFileAction::new();
            state.register_action(
                indexset![],
                written_macro_files.iter().map(|a| a.as_output()).collect(),
                action,
                Some(eval.heap().alloc(content_cli)),
            )?;

            written_macro_files
        } else {
            indexset![]
        };

        let action = {
            let maybe_macro_files = if allow_args {
                let mut macro_files = indexset![];
                for a in &written_macro_files {
                    macro_files.insert(a.dupe().ensure_bound()?.into_artifact());
                }
                Some(macro_files)
            } else {
                None
            };
            UnregisteredWriteAction::new(is_executable, maybe_macro_files)
        };
        this.register_action(
            indexset![],
            indexset![output_artifact],
            action,
            Some(content_cli),
        )?;

        if allow_args {
            let macro_files: Vec<StarlarkDeclaredArtifact> = written_macro_files
                .into_iter()
                .map(|a| StarlarkDeclaredArtifact::new(None, a))
                .collect();
            Ok(eval.heap().alloc((output_value, macro_files)))
        } else {
            // Prefer simpler API when there is no possibility for write-to-file macros to be present in a content
            Ok(output_value)
        }
    }

    fn copy_file<'v>(
        this: &AnalysisActions<'v>,
        #[starlark(require = pos)] dest: Value<'v>,
        #[starlark(require = pos)] src: Value<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        copy_file(eval, this, dest, src, CopyMode::Copy)
    }

    fn symlink_file<'v>(
        this: &AnalysisActions<'v>,
        #[starlark(require = pos)] dest: Value<'v>,
        #[starlark(require = pos)] src: Value<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        copy_file(eval, this, dest, src, CopyMode::Symlink)
    }

    fn symlinked_dir<'v>(
        this: &AnalysisActions<'v>,
        #[starlark(require = pos)] output: Value<'v>,
        #[starlark(require = pos)] srcs: Value<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        create_dir_tree(eval, this, output, srcs, false)
    }

    fn copied_dir<'v>(
        this: &AnalysisActions<'v>,
        #[starlark(require = pos)] output: Value<'v>,
        #[starlark(require = pos)] srcs: Value<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        create_dir_tree(eval, this, output, srcs, true)
    }

    fn run<'v>(
        this: &AnalysisActions<'v>,
        #[starlark(require = pos)] arguments: Value<'v>,
        #[starlark(require = named)] category: String,
        #[starlark(require = named, default = NoneOr::None)] identifier: NoneOr<String>,
        #[starlark(require = named)] env: Option<ValueOf<'v, SmallMap<&'v str, Value<'v>>>>,
        #[starlark(require = named, default = false)] local_only: bool,
        #[starlark(require = named, default = false)] prefer_local: bool,
        #[starlark(require = named, default = false)] always_print_stderr: bool,
        #[starlark(require = named, default = 1)] weight: i32,
        #[starlark(require = named)] dep_files: Option<ValueOf<'v, SmallMap<&'v str, Value<'v>>>>,
        #[starlark(require = named)] metadata_env_var: Option<String>,
        #[starlark(require = named)] metadata_path: Option<String>,
        // TODO(scottcao): Refactor `no_outputs_cleanup` to `outputs_cleanup`
        #[starlark(require = named, default = false)] no_outputs_cleanup: bool,
        #[starlark(require = named, default = false)] allow_cache_upload: bool,
        heap: &'v Heap,
    ) -> anyhow::Result<NoneType> {
        struct RunCommandArtifactVisitor {
            inner: SimpleCommandLineArtifactVisitor,
            tagged_outputs: HashMap<ArtifactTag, Vec<OutputArtifact>>,
        }

        impl RunCommandArtifactVisitor {
            fn new() -> Self {
                Self {
                    inner: SimpleCommandLineArtifactVisitor::new(),
                    tagged_outputs: HashMap::new(),
                }
            }
        }

        impl CommandLineArtifactVisitor for RunCommandArtifactVisitor {
            fn visit_input(&mut self, input: ArtifactGroup, tag: Option<&ArtifactTag>) {
                self.inner.visit_input(input, tag);
            }

            fn visit_output(&mut self, artifact: OutputArtifact, tag: Option<&ArtifactTag>) {
                match tag {
                    None => {}
                    Some(tag) => {
                        self.tagged_outputs
                            .entry(tag.dupe())
                            .or_default()
                            .push(artifact.dupe());
                    }
                }

                self.inner.visit_output(artifact, tag);
            }
        }

        let executor_preference = new_executor_preference(local_only, prefer_local)?;

        let mut artifact_visitor = RunCommandArtifactVisitor::new();

        let starlark_cli = StarlarkCommandLine::try_from_value(arguments)?;
        starlark_cli.visit_artifacts(&mut artifact_visitor)?;

        if weight < 1 {
            return Err(RunActionError::InvalidWeight(weight).into());
        }
        let weight = weight as usize;

        let starlark_env = match env {
            None => Value::new_none(),
            Some(env) => {
                for v in env.typed.values() {
                    v.as_command_line_err()?
                        .visit_artifacts(&mut artifact_visitor)?;
                }
                env.value
            }
        };

        let RunCommandArtifactVisitor {
            inner: artifacts,
            tagged_outputs,
        } = artifact_visitor;

        let mut dep_files_configuration = RunActionDepFiles::new();

        if let Some(dep_files) = dep_files {
            for (key, value) in dep_files.typed.iter() {
                let tag = value.downcast_ref::<ArtifactTag>().ok_or_else(|| {
                    RunActionError::InvalidDepFileTag {
                        key: (*key).to_owned(),
                        value: value.to_string(),
                    }
                })?;

                let tagged = tagged_outputs.get(tag);
                let count = tagged.map_or(0, |t| t.len());

                if count != 1 {
                    return Err(RunActionError::InvalidDepFileOutputs {
                        key: (*key).to_owned(),
                        count,
                    }
                    .into());
                }

                match dep_files_configuration.labels.entry(tag.dupe()) {
                    Entry::Vacant(v) => {
                        v.insert(Arc::from(*key));
                    }
                    Entry::Occupied(o) => {
                        return Err(RunActionError::ConflictingDepFiles {
                            first: (**o.get()).to_owned(),
                            second: (*key).to_owned(),
                        }
                        .into());
                    }
                }
            }
        }

        let category = Category::try_from(category)?;
        let identifier = identifier.into_option();

        let metadata_param = match (metadata_env_var, metadata_path) {
            (Some(env_var), Some(path)) => {
                let path: ForwardRelativePathBuf = path.try_into()?;
                this.state().claim_output_path(&path)?;
                Ok(Some(MetadataParameter { env_var, path }))
            }
            (Some(_), None) => Err(anyhow::anyhow!(RunActionError::MetadataPathMissing)),
            (None, Some(_)) => Err(anyhow::anyhow!(RunActionError::MetadataEnvVarMissing)),
            (None, None) => Ok(None),
        }?;

        if artifacts.outputs.is_empty() {
            return Err(RunActionError::NoOutputsSpecified.into());
        }
        let starlark = heap.alloc((starlark_cli, starlark_env));

        let action = UnregisteredRunAction {
            category,
            identifier,
            executor_preference,
            always_print_stderr,
            weight,
            dep_files: dep_files_configuration,
            metadata_param,
            no_outputs_cleanup,
            allow_cache_upload,
        };
        this.state().register_action(
            artifacts.inputs,
            artifacts.outputs,
            action,
            Some(starlark),
        )?;
        Ok(NoneType)
    }

    fn download_file<'v>(
        this: &AnalysisActions<'v>,
        #[starlark(require = pos)] output: Value<'v>,
        #[starlark(require = pos)] url: &str,
        #[starlark(require = named, default = NoneOr::None)] sha1: NoneOr<&str>,
        #[starlark(require = named, default = NoneOr::None)] sha256: NoneOr<&str>,
        #[starlark(require = named, default = false)] is_executable: bool,
        #[starlark(require = named, default = false)] is_deferrable: bool,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        let mut this = this.state();
        let (output_value, output_artifact) = this.get_or_declare_output(eval, output, "output")?;

        let checksum = match (
            sha1.into_option().map(Arc::from),
            sha256.into_option().map(Arc::from),
        ) {
            (Some(sha1), None) => Checksum::Sha1(sha1),
            (None, Some(sha256)) => Checksum::Sha256(sha256),
            (Some(sha1), Some(sha256)) => Checksum::Both { sha1, sha256 },
            (None, None) => return Err(DownloadFileError::MissingChecksum.into()),
        };

        this.register_action(
            IndexSet::new(),
            indexset![output_artifact],
            UnregisteredDownloadFileAction::new(
                checksum,
                Arc::from(url),
                is_executable,
                is_deferrable,
            ),
            None,
        )?;
        Ok(output_value)
    }

    fn tset<'v>(
        this: &AnalysisActions<'v>,
        #[starlark(require = pos)] definition: Value<'v>,
        value: Option<Value<'v>>,
        children: Option<Value<'v>>, // An iterable.
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        let mut this = this.state();
        this.create_transitive_set(definition, value, children, eval)
    }

    fn dynamic_output<'v>(
        this: &'v AnalysisActions<'v>,
        dynamic: Vec<StarlarkArtifact>,
        inputs: Vec<StarlarkArtifact>,
        outputs: Vec<StarlarkOutputArtifact>,
        f: Value<'v>,
        heap: &'v Heap,
    ) -> anyhow::Result<NoneType> {
        // Parameter validation
        let lambda_type = f.get_type();
        if lambda_type != FUNCTION_TYPE {
            return Err(DynamicOutputError::NotAFunction(lambda_type.to_owned()).into());
        }
        if dynamic.is_empty() {
            return Err(DynamicOutputError::EmptyDynamic.into());
        }
        if outputs.is_empty() {
            return Err(DynamicOutputError::EmptyOutput.into());
        }

        // Conversion
        let dynamic = dynamic.iter().map(|x| x.artifact()).collect();
        let inputs = inputs.iter().map(|x| x.artifact()).collect();
        let outputs = outputs.iter().map(|x| x.artifact()).collect();

        // Registration
        let attributes_lambda = heap.alloc((this.attributes, f));
        let mut this = this.state();
        this.register_dynamic_output(dynamic, inputs, outputs, attributes_lambda)?;
        Ok(NoneType)
    }

    /// Allocate a new input tag
    fn artifact_tag<'v>(this: &AnalysisActions<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        let _ = this;
        Ok(heap.alloc(ArtifactTag::new()))
    }
}

#[cfg(test)]
mod tests {
    use buck2_core::configuration::Configuration;
    use buck2_core::provider::label::ConfiguredProvidersLabel;
    use buck2_core::provider::label::ProvidersName;
    use buck2_core::target::testing::TargetLabelExt;
    use buck2_core::target::TargetLabel;
    use buck2_execute::base_deferred_key::BaseDeferredKey;
    use buck2_interpreter::types::label::LabelGen;
    use buck2_node::configuration::execution::ExecutionPlatformResolution;
    use gazebo::prelude::*;
    use indoc::indoc;
    use starlark::collections::SmallMap;
    use starlark::environment::GlobalsBuilder;
    use starlark::environment::Module;
    use starlark::eval::Evaluator;
    use starlark::eval::ReturnFileLoader;
    use starlark::syntax::AstModule;
    use starlark::syntax::Dialect;
    use starlark::values::structs::Struct;
    use starlark::values::Value;
    use starlark::values::ValueTyped;

    use crate::analysis::registry::AnalysisRegistry;
    use crate::interpreter::rule_defs::context::AnalysisContext;
    use crate::starlark::values::UnpackValue;

    fn run_ctx_test(
        content: &str,
        result_handler: impl FnOnce(anyhow::Result<Value>) -> anyhow::Result<()>,
    ) -> anyhow::Result<()> {
        let func_mod = Module::new();
        let globals = GlobalsBuilder::extended()
            .with(crate::interpreter::rule_defs::register_rule_defs)
            .build();
        let prelude = indoc!(
            r#"
            def assert_eq(a, b):
                if a != b:
                    fail("Expected {}, got {}".format(a, b))
            "#
        );
        let full_content = format!("{}\n{}", prelude, content);

        let mut eval = Evaluator::new(&func_mod);
        let ast = AstModule::parse("foo.bzl", full_content, &Dialect::Extended).unwrap();
        eval.eval_module(ast, &globals).unwrap();
        let frozen_func_mod = func_mod.freeze()?;
        let test_function = frozen_func_mod.get("test").unwrap();

        let modules = hashmap!["func_mod" => &frozen_func_mod];

        let env = Module::new();
        let file_loader = ReturnFileLoader { modules: &modules };
        let test_function = test_function.owned_value(env.frozen_heap());
        let mut eval = Evaluator::new(&env);
        eval.set_loader(&file_loader);
        let label = TargetLabel::testing_parse("root//foo/bar:some_name")
            .configure(Configuration::testing_new());
        let registry = AnalysisRegistry::new_from_owner(
            BaseDeferredKey::TargetLabel(label.dupe()),
            ExecutionPlatformResolution::unspecified(),
        );
        let mut values = SmallMap::with_capacity(1);
        values.insert(
            eval.heap().alloc_str("name"),
            eval.heap().alloc("some_name"),
        );
        let attributes = eval.heap().alloc(Struct::new(values));

        let ctx = eval.heap().alloc(AnalysisContext::new(
            eval.heap(),
            attributes,
            Some(
                ValueTyped::new(eval.heap().alloc(LabelGen::new(
                    env.heap(),
                    ConfiguredProvidersLabel::new(label, ProvidersName::Default),
                )))
                .unwrap(),
            ),
            registry,
        ));

        let returned = eval.eval_function(test_function, &[ctx], &[]);
        result_handler(returned)
    }

    #[test]
    fn ctx_instantiates() -> anyhow::Result<()> {
        let content = indoc!(
            r#"
            def test(ctx):
                assert_eq("foo/bar", ctx.label.package)
                assert_eq("some_name", ctx.label.name)
                assert_eq(None, ctx.label.sub_target)
                return ctx.attrs.name
            "#
        );
        run_ctx_test(content, |ret| {
            assert_eq!("some_name", ret.unwrap().unpack_str().unwrap());
            Ok(())
        })
    }

    #[test]
    fn declare_output_declares_outputs() -> anyhow::Result<()> {
        let content = indoc!(
            r#"
            def test(c):
                out = c.actions.declare_output("foo/bar.cpp")
                return (out.basename, out.short_path)
            "#
        );

        run_ctx_test(content, |ret| {
            let a = <(&str, &str)>::unpack_value(ret.unwrap()).unwrap();
            assert_eq!("bar.cpp", a.0);
            assert_eq!("foo/bar.cpp", a.1);
            Ok(())
        })
    }

    #[test]
    fn declare_output_with_prefix() -> anyhow::Result<()> {
        let content = indoc!(
            r#"
            def test(c):
                out = c.actions.declare_output("out/test", "foo/bar.cpp")
                return (out.basename, out.short_path)
            "#
        );

        run_ctx_test(content, |ret| {
            let a = <(&str, &str)>::unpack_value(ret.unwrap()).unwrap();
            assert_eq!("bar.cpp", a.0);
            assert_eq!("foo/bar.cpp", a.1);
            Ok(())
        })
    }

    #[test]
    fn declare_output_dot() -> anyhow::Result<()> {
        let content = indoc!(
            r#"
            def test(c):
                return c.actions.declare_output("magic", ".")
            "#
        );

        let expect = "artifact with an empty filename component";
        run_ctx_test(content, |ret| match ret {
            Err(e) if e.to_string().contains(expect) => Ok(()),
            _ => panic!(
                "Expected a specific failure containing `{}`, got {:?}",
                expect, ret
            ),
        })
    }

    #[test]
    fn declare_output_dot_bad() -> anyhow::Result<()> {
        let content = indoc!(
            r#"
            def test(c):
                return c.actions.declare_output("..")
            "#
        );

        let expect = "expected a normalized path";
        run_ctx_test(content, |ret| match ret {
            Err(e) if e.to_string().contains(expect) => Ok(()),
            _ => panic!(
                "Expected a specific failure containing `{}`, got {:?}",
                expect, ret
            ),
        })
    }
    #[test]
    fn declare_output_dotdot() -> anyhow::Result<()> {
        let content = indoc!(
            r#"
            def test(c):
                return c.actions.declare_output("foo/..")
            "#
        );

        let expect = "expected a normalized path";
        run_ctx_test(content, |ret| match ret {
            Err(e) if e.to_string().contains(expect) => Ok(()),
            _ => panic!(
                "Expected a specific failure containing `{}`, got {:?}",
                expect, ret
            ),
        })
    }

    #[test]
    fn declare_output_require_bound() -> anyhow::Result<()> {
        let content = indoc!(
            r#"
            def test(c):
                a = c.actions.declare_output("a")
                b = c.actions.declare_output("b")
                c.actions.run([a, b.as_output()], category = "test_category")
            "#
        );

        let expect = "should be bound by now";
        run_ctx_test(content, |ret| match ret {
            Err(e) if e.to_string().contains(expect) => Ok(()),
            _ => panic!(
                "Expected a specific failure containing `{}`, got {:?}",
                expect, ret
            ),
        })
    }
}
