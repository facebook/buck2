/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! This module contains support for running actions and asynchronous providers
//!
//! An 'Action' is a unit of work with a set of input files known as 'Artifact's that are required
//! for its execution, and a set of output files called 'BuildArtifact's that are created by its
//! execution. Each 'Action' registered by a rule will only be executed when it's 'BuildArtifact's
//! are requested to be available. It will be guaranteed by the action system that all input
//! 'Artifact's are available before the execution of an 'Action'.
//!
//! 'Actions' struct will act as a general registry where users can create new 'Artifact's that
//! represent the outputs of the execution of their 'Action'. These are 'DeclaredArtifact's that
//! are yet bound to any 'Action's. When 'Action's are registered, they will be bound to their
//! appropriate 'DeclaredArtifact' to create a 'BuildArtifact'
//!
//! An 'Action' can be bound to multiple 'BuildArtifact's, but each 'BuildArtifact' can only be
//! bound to a particular 'Action'.

use std::borrow::Cow;
use std::fmt::Debug;
use std::ops::ControlFlow;
use std::sync::Arc;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_artifact::actions::key::ActionKey;
use buck2_artifact::artifact::artifact_type::Artifact;
use buck2_artifact::artifact::build_artifact::BuildArtifact;
use buck2_common::io::IoProvider;
use buck2_core::category::Category;
use buck2_core::category::CategoryRef;
use buck2_core::content_hash::ContentBasedPathHash;
use buck2_core::deferred::base_deferred_key::BaseDeferredKey;
use buck2_core::execution_types::executor_config::CommandExecutorConfig;
use buck2_core::fs::artifact_path_resolver::ArtifactFs;
use buck2_events::dispatch::EventDispatcher;
use buck2_execute::artifact::fs::ExecutorFs;
use buck2_execute::digest_config::DigestConfig;
use buck2_execute::execute::action_digest_and_blobs::ActionDigestAndBlobs;
use buck2_execute::execute::blocking::BlockingExecutor;
use buck2_execute::execute::cache_uploader::CacheUploadResult;
use buck2_execute::execute::cache_uploader::IntoRemoteDepFile;
use buck2_execute::execute::manager::CommandExecutionManager;
use buck2_execute::execute::prepared::PreparedAction;
use buck2_execute::execute::request::CommandExecutionRequest;
use buck2_execute::execute::request::ExecutorPreference;
use buck2_execute::execute::result::CommandExecutionResult;
use buck2_execute::materialize::materializer::Materializer;
use buck2_execute::re::manager::UnconfiguredRemoteExecutionClient;
use buck2_execute::re::output_trees_download_config::OutputTreesDownloadConfig;
use buck2_file_watcher::mergebase::Mergebase;
use buck2_fs::paths::forward_rel_path::ForwardRelativePathBuf;
use buck2_http::HttpClient;
use derivative::Derivative;
use derive_more::Display;
use dice_futures::cancellation::CancellationContext;
use fxhash::FxHashMap;
use indexmap::IndexMap;
use indexmap::IndexSet;
use indexmap::indexmap;
use remote_execution::TActionResult2;
use starlark::values::Heap;
use starlark::values::OwnedFrozenValue;
use starlark::values::ValueOfUnchecked;
use starlark::values::dict::DictType;
use static_assertions::_core::ops::Deref;

use crate::actions::execute::action_execution_target::ActionExecutionTarget;
use crate::actions::execute::action_executor::ActionExecutionMetadata;
use crate::actions::execute::action_executor::ActionOutputs;
use crate::actions::execute::error::ExecuteError;
use crate::actions::impls::run_action_knobs::RunActionKnobs;
use crate::artifact_groups::ArtifactGroup;
use crate::artifact_groups::ArtifactGroupValues;
use crate::interpreter::rule_defs::artifact::starlark_artifact::StarlarkArtifact;
use crate::interpreter::rule_defs::artifact::starlark_artifact_value::StarlarkArtifactValue;
use crate::interpreter::rule_defs::cmd_args::ArtifactPathMapper;

pub mod artifact;
pub mod box_slice_set;
pub mod calculation;
mod error;
pub mod error_handler;
pub mod execute;
pub mod impls;
pub mod query;
pub mod registry;

/// Represents an unregistered 'Action' that will be registered into the 'Actions' module.
/// The 'UnregisteredAction' is not executable until it is registered, upon which it becomes an
/// 'Action' that is executable.
pub trait UnregisteredAction: Allocative + Send {
    /// consumes the self and becomes a registered 'Action'. The 'Action' will be executable
    /// and no longer bindable to any other 'Artifact's.
    fn register(
        self: Box<Self>,
        outputs: IndexSet<BuildArtifact>,
        starlark_data: Option<OwnedFrozenValue>,
        error_handler: Option<OwnedFrozenValue>,
    ) -> buck2_error::Result<Box<dyn Action>>;
}

/// A registered, immutable 'Action' that is fully bound. All it's 'Artifact's, both inputs and
/// outputs are verified to exist.
///
/// The 'Action' can be executed to produce the set of 'BuildArtifact's it declares. Before
/// execution, all input 'Artifact's will be made available to access.
#[async_trait]
pub trait Action: Allocative + Debug + Send + Sync + 'static {
    /// A machine readable kind identifying this type of action.
    fn kind(&self) -> buck2_data::ActionKind;

    /// All the input 'Artifact's, both sources and built artifacts, that are required for
    /// executing this artifact. While nothing enforces it, this should be a pure function.
    fn inputs(&self) -> buck2_error::Result<Cow<'_, [ArtifactGroup]>>;

    /// All the outputs this 'Artifact' will generate. Just like inputs, this should be a pure
    /// function. Note that outputs in action result might be ordered differently.
    fn outputs(&self) -> Cow<'_, [BuildArtifact]>;

    /// Returns a reference to an output of the action. All actions are required to have at least one output.
    fn first_output(&self) -> &BuildArtifact;

    /// Runs the 'Action', where all inputs are available but the output directory may not have
    /// been cleaned up. Upon success, it is expected that all outputs will be available
    async fn execute(
        &self,
        ctx: &mut dyn ActionExecutionCtx,
    ) -> Result<(ActionOutputs, ActionExecutionMetadata), ExecuteError>;

    /// A machine-readable category for this action, intended to be used when analyzing actions outside of buck2 itself.
    ///
    /// A category provides a namespace for identifiers within the rule that produced this action. Examples of
    /// categories would be things such as `cxx_compile`, `cxx_link`, and so on. Categories are user-specified in the
    /// rule implementation; however, buck2 enforces some restrictions on category names.
    fn category(&self) -> CategoryRef<'_>;

    /// A machine-readable identifier for this action. Required (but as of now, not yet enforced) to be unique within
    /// a category within a single invocation of a rule. Like categories, identifiers are also user-specified and buck2
    /// ascribes no semantics to them. Examples of category-identifier pairs would be `cxx_compile` + `MyCppFile.cpp`,
    /// reflecting a C++ compiler invocation for a file `MyCppFile.cpp`.
    ///
    /// Not required; if None, only one action will be given in the given category. The user should
    /// be given either control over the identifier or the category.
    fn identifier(&self) -> Option<&str>;

    /// Whether to always print stderr, or only print when a user asks for it.
    fn always_print_stderr(&self) -> bool {
        false
    }

    /// Provides a string name for this action, obtained by combining the provided category and identifier.
    fn name(&self) -> String {
        if let Some(identifier) = self.identifier() {
            format!("{} {}", self.category(), identifier)
        } else {
            self.category().to_string()
        }
    }

    fn aquery_attributes(
        &self,
        _fs: &ExecutorFs,
        _artifact_path_mapping: &dyn ArtifactPathMapper,
    ) -> IndexMap<String, String> {
        indexmap! {}
    }

    fn error_handler(&self) -> Option<OwnedFrozenValue> {
        None
    }

    fn failed_action_output_artifacts<'v>(
        &self,
        _artifact_fs: &ArtifactFs,
        _heap: Heap<'v>,
        _outputs: Option<&ActionOutputs>,
    ) -> buck2_error::Result<ValueOfUnchecked<'v, DictType<StarlarkArtifact, StarlarkArtifactValue>>>
    {
        Ok(ValueOfUnchecked::new(starlark::values::Value::new_none()))
    }

    fn all_outputs_are_content_based(&self) -> bool {
        for output in self.outputs().iter() {
            if !output.get_path().is_content_based_path() {
                return false;
            }
        }
        true
    }

    fn all_inputs_are_eligible_for_dedupe(&self) -> bool {
        self.all_ineligible_for_dedup_inputs().is_empty()
    }

    fn all_ineligible_for_dedup_inputs(&self) -> Vec<String> {
        let target_platform = if let BaseDeferredKey::TargetLabel(configured_label) =
            self.first_output().key().owner()
        {
            Some(configured_label.cfg())
        } else {
            None
        };

        let mut ineligible_inputs = Vec::new();
        for ag in self.inputs().unwrap_or_default().iter() {
            if !ag.is_eligible_for_dedupe(target_platform) {
                ineligible_inputs.push(ag.to_string());
            }
        }
        ineligible_inputs
    }

    // TODO this probably wants more data for execution, like printing a short_name and the target
}

/// The context for actions to use when executing
#[async_trait]
pub trait ActionExecutionCtx: Send + Sync {
    fn target(&self) -> ActionExecutionTarget<'_>;

    /// An 'ArtifactFs' to be used for managing 'Artifact's
    fn fs(&self) -> &ArtifactFs;

    fn executor_fs(&self) -> ExecutorFs<'_>;

    /// A `Materializer` used for expensive materializations
    fn materializer(&self) -> &dyn Materializer;

    fn events(&self) -> &EventDispatcher;

    fn command_execution_manager(&self) -> CommandExecutionManager;

    fn mergebase(&self) -> &Mergebase;

    fn prepare_action(
        &mut self,
        request: &CommandExecutionRequest,
        re_outputs_required: bool,
    ) -> buck2_error::Result<PreparedAction>;

    async fn action_cache(
        &mut self,
        manager: CommandExecutionManager,
        request: &CommandExecutionRequest,
        prepared_action: &PreparedAction,
    ) -> ControlFlow<CommandExecutionResult, CommandExecutionManager>;

    async fn remote_dep_file_cache(
        &mut self,
        manager: CommandExecutionManager,
        request: &CommandExecutionRequest,
        prepared_action: &PreparedAction,
    ) -> ControlFlow<CommandExecutionResult, CommandExecutionManager>;

    async fn cache_upload(
        &mut self,
        action: &ActionDigestAndBlobs,
        execution_result: &CommandExecutionResult,
        re_result: Option<TActionResult2>,
        dep_file_entry: Option<&mut dyn IntoRemoteDepFile>,
    ) -> buck2_error::Result<CacheUploadResult>;

    /// Executes a command
    /// TODO(bobyf) this seems like it deserves critical sections?
    async fn exec_cmd(
        &mut self,
        manager: CommandExecutionManager,
        request: &CommandExecutionRequest,
        prepared_action: &PreparedAction,
    ) -> CommandExecutionResult;

    fn unpack_command_execution_result(
        &mut self,
        executor_preference: ExecutorPreference,
        result: CommandExecutionResult,
        allows_cache_upload: bool,
        allows_dep_file_cache_upload: bool,
        input_files_bytes: Option<u64>,
        incremental_kind: buck2_data::IncrementalKind,
    ) -> Result<(ActionOutputs, ActionExecutionMetadata), ExecuteError>;

    /// Clean up all the output directories for this action. This requires a mutable reference
    /// because you shouldn't be doing anything else with the ActionExecutionCtx while cleaning the
    /// outputs.
    async fn cleanup_outputs(&mut self) -> buck2_error::Result<()>;

    /// Get the value of an Artifact. This Artifact _must_ have been declared
    /// as an input to the associated action or a panic will be raised.
    fn artifact_values(&self, input: &ArtifactGroup) -> &ArtifactGroupValues;

    fn artifact_path_mapping(
        &self,
        filter: Option<IndexSet<ArtifactGroup>>,
    ) -> FxHashMap<&Artifact, ContentBasedPathHash>;

    fn blocking_executor(&self) -> &dyn BlockingExecutor;

    fn re_client(&self) -> UnconfiguredRemoteExecutionClient;

    fn re_platform(&self) -> &remote_execution::Platform;

    fn digest_config(&self) -> DigestConfig;

    /// Obtain per-command knobs for RunAction.
    fn run_action_knobs(&self) -> &RunActionKnobs;

    fn cancellation_context(&self) -> &CancellationContext;

    /// I/O layer access to add non-source files (e.g. downloaded files) to
    /// offline archive trace. If None, tracing is not enabled.
    fn io_provider(&self) -> Arc<dyn IoProvider>;

    /// Http client used for fetching and downloading remote artifacts.
    fn http_client(&self) -> HttpClient;

    fn output_trees_download_config(&self) -> &OutputTreesDownloadConfig;
}

#[derive(buck2_error::Error, Debug)]
#[buck2(input)]
pub enum ActionErrors {
    #[error("Output path for artifact or metadata file cannot be empty.")]
    EmptyOutputPath,
    #[error(
        "Multiple artifacts and/or metadata files are declared at the same output location `{0}` declared at `{1}`."
    )]
    ConflictingOutputPath(ForwardRelativePathBuf, String),
    #[error(
        "Multiple artifacts and/or metadata files are declared at conflicting output locations. Output path `{0}` conflicts with the following output paths: {1:?}."
    )]
    ConflictingOutputPaths(ForwardRelativePathBuf, Vec<String>),
    #[error(
        "Action category `{0}` contains duplicate identifier `{1}`; category-identifier pairs must be unique within a rule"
    )]
    ActionCategoryIdentifierNotUnique(Category, String),
    #[error(
        "Analysis produced multiple actions with category `{0}` and at least one of them had no identifier. Add an identifier to these actions to disambiguate them"
    )]
    ActionCategoryDuplicateSingleton(Category),
}

#[derive(Derivative, Debug, Display, Allocative)]
#[derivative(Eq, Hash, PartialEq)]
#[display("Action(key={}, name={})", key, action.name())]
pub struct RegisteredAction {
    /// The key uniquely identifies a registered action.
    /// The key to the action is a one to one mapping.
    key: ActionKey,
    #[derivative(Hash = "ignore", PartialEq = "ignore")]
    action: Box<dyn Action>,
    #[derivative(Hash = "ignore", PartialEq = "ignore")]
    executor_config: Arc<CommandExecutorConfig>,
}

impl RegisteredAction {
    pub fn new(
        key: ActionKey,
        action: Box<dyn Action>,
        executor_config: Arc<CommandExecutorConfig>,
    ) -> Self {
        Self {
            key,
            action,
            executor_config,
        }
    }

    pub fn action(&self) -> &dyn Action {
        self.action.as_ref()
    }

    /// Gets the target label to the rule that created this action.
    pub fn owner(&self) -> &BaseDeferredKey {
        self.key.owner()
    }

    /// Gets the action key, uniquely identifying this action in a target.
    pub(crate) fn action_key(&self) -> ForwardRelativePathBuf {
        // We want the action key to not cause instability in the RE action.
        // As an artifact can only be bound as an output to one action, we know it uniquely identifies the action and we can
        // derive the scratch path from that and that will be no unstable than the artifact already is.
        let output_path = self.action.first_output().get_path();
        match output_path.dynamic_actions_action_key() {
            Some(k) => k
                .as_file_name()
                .as_forward_rel_path()
                .join(output_path.path()),
            None => output_path.path().to_buf(),
        }
    }

    pub fn key(&self) -> &ActionKey {
        &self.key
    }

    pub(crate) fn execution_config(&self) -> &CommandExecutorConfig {
        &self.executor_config
    }

    pub fn category(&self) -> CategoryRef<'_> {
        self.action.category()
    }

    pub fn identifier(&self) -> Option<&str> {
        self.action.identifier()
    }
}

impl Deref for RegisteredAction {
    type Target = dyn Action;

    fn deref(&self) -> &Self::Target {
        self.action.as_ref()
    }
}

/// An 'UnregisteredAction' that is stored by the 'ActionsRegistry' to be registered.
/// The stored inputs have not yet been validated as bound, but will be validated upon registering.
#[derive(Allocative)]
struct ActionToBeRegistered {
    key: ActionKey,
    outputs: IndexSet<BuildArtifact>,
    action: Box<dyn UnregisteredAction>,
}

impl ActionToBeRegistered {
    fn new<A: UnregisteredAction + 'static>(
        key: ActionKey,
        outputs: IndexSet<BuildArtifact>,
        a: A,
    ) -> Self {
        Self {
            key,
            outputs,
            action: Box::new(a),
        }
    }

    pub(crate) fn key(&self) -> &ActionKey {
        &self.key
    }

    fn register(
        self,
        starlark_data: Option<OwnedFrozenValue>,
        error_handler: Option<OwnedFrozenValue>,
    ) -> buck2_error::Result<Box<dyn Action>> {
        self.action
            .register(self.outputs, starlark_data, error_handler)
    }
}
