/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
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

pub mod artifact;
pub mod box_slice_set;
pub mod build_listener;
pub mod calculation;
pub mod execute;
pub mod impls;
pub mod key;
pub mod query;
pub mod registry;

use std::any::Demand;
use std::borrow::Cow;
use std::fmt::Debug;
use std::sync::Arc;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_common::executor_config::CommandExecutorConfig;
use buck2_common::io::IoProvider;
use buck2_core::category::Category;
use buck2_core::fs::artifact_path_resolver::ArtifactFs;
use buck2_core::fs::buck_out_path::BuckOutPath;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePathBuf;
use buck2_events::dispatch::EventDispatcher;
use buck2_execute::artifact::fs::ExecutorFs;
use buck2_execute::artifact_value::ArtifactValue;
use buck2_execute::digest_config::DigestConfig;
use buck2_execute::execute::blocking::BlockingExecutor;
use buck2_execute::execute::request::CommandExecutionRequest;
use buck2_execute::materialize::materializer::Materializer;
use buck2_execute::re::manager::ManagedRemoteExecutionClient;
use derivative::Derivative;
use derive_more::Display;
use indexmap::indexmap;
use indexmap::IndexMap;
use indexmap::IndexSet;
use key::ActionKey;
use more_futures::cancellation::CancellationContext;
use starlark::values::OwnedFrozenValue;
use static_assertions::_core::ops::Deref;
use thiserror::Error;

use crate::actions::artifact::build_artifact::BuildArtifact;
use crate::actions::artifact::provide_outputs::ProvideOutputs;
use crate::actions::execute::action_execution_target::ActionExecutionTarget;
use crate::actions::execute::action_executor::ActionExecutionMetadata;
use crate::actions::execute::action_executor::ActionOutputs;
use crate::actions::impls::run_action_knobs::RunActionKnobs;
use crate::artifact_groups::ArtifactGroup;
use crate::artifact_groups::ArtifactGroupValues;
use crate::deferred::base_deferred_key::BaseDeferredKey;
use crate::deferred::types::AnyValue;
use crate::deferred::types::TrivialDeferred;

/// Represents an unregistered 'Action' that will be registered into the 'Actions' module.
/// The 'UnregisteredAction' is not executable until it is registered, upon which it becomes an
/// 'Action' that is executable.
pub trait UnregisteredAction: Allocative {
    /// consumes the self and becomes a registered 'Action'. The 'Action' will be executable
    /// and no longer bindable to any other 'Artifact's.
    fn register(
        self: Box<Self>,
        inputs: IndexSet<ArtifactGroup>,
        outputs: IndexSet<BuildArtifact>,
        starlark_data: Option<OwnedFrozenValue>,
    ) -> anyhow::Result<Box<dyn Action>>;
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
    fn inputs(&self) -> anyhow::Result<Cow<'_, [ArtifactGroup]>>;

    /// All the outputs this 'Artifact' will generate. Just like inputs, this should be a pure
    /// function.
    fn outputs(&self) -> anyhow::Result<Cow<'_, [BuildArtifact]>>;

    /// Obtains an executable for this action.
    fn as_executable(&self) -> ActionExecutable<'_>;

    /// A machine-readable category for this action, intended to be used when analyzing actions outside of buck2 itself.
    ///
    /// A category provides a namespace for identifiers within the rule that produced this action. Examples of
    /// categories would be things such as `cxx_compile`, `cxx_link`, and so on. Categories are user-specified in the
    /// rule implementation; however, buck2 enforces some restrictions on category names.
    fn category(&self) -> &Category;

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

    fn aquery_attributes(&self, _fs: &ExecutorFs) -> IndexMap<String, String> {
        indexmap! {}
    }

    // TODO this probably wants more data for execution, like printing a short_name and the target
}

pub enum ActionExecutable<'a> {
    Pristine(&'a dyn PristineActionExecutable),
    Incremental(&'a dyn IncrementalActionExecutable),
}

#[async_trait]
pub trait PristineActionExecutable: Send + Sync + 'static {
    /// Runs the 'Action', where all inputs are available and the output directory has been cleaned
    /// up. Upon success, it is expected that all outputs will be available
    async fn execute(
        &self,
        ctx: &mut dyn ActionExecutionCtx,
    ) -> anyhow::Result<(ActionOutputs, ActionExecutionMetadata)>;
}

#[async_trait]
pub trait IncrementalActionExecutable: Send + Sync + 'static {
    /// Runs the 'Action', where all inputs are available but the output directory may not have
    /// been cleaned up. Upon success, it is expected that all outputs will be available
    async fn execute(
        &self,
        ctx: &mut dyn ActionExecutionCtx,
    ) -> anyhow::Result<(ActionOutputs, ActionExecutionMetadata)>;
}

/// The context for actions to use when executing
#[async_trait]
pub trait ActionExecutionCtx: Send + Sync {
    fn target(&self) -> ActionExecutionTarget<'_>;

    /// An 'ArtifactFs' to be used for managing 'Artifact's
    fn fs(&self) -> &ArtifactFs;

    fn executor_fs(&self) -> ExecutorFs;

    /// A `Materializer` used for expensive materializations
    fn materializer(&self) -> &dyn Materializer;

    fn events(&self) -> &EventDispatcher;

    /// Executes a command
    /// TODO(bobyf) this seems like it deserves critical sections?
    async fn exec_cmd(
        &mut self,
        request: &CommandExecutionRequest,
    ) -> anyhow::Result<(
        IndexMap<BuckOutPath, ArtifactValue>,
        ActionExecutionMetadata,
    )>;

    /// Clean up all the output directories for this action. This requires a mutable reference
    /// because you shouldn't be doing anything else with the ActionExecutionCtx while cleaning the
    /// outputs.
    async fn cleanup_outputs(&mut self) -> anyhow::Result<()>;

    /// Get the value of an Artifact. This Artifact _must_ have been declared
    /// as an input to the associated action or a panic will be raised.
    fn artifact_values(&self, input: &ArtifactGroup) -> &ArtifactGroupValues;

    fn blocking_executor(&self) -> &dyn BlockingExecutor;

    fn re_client(&self) -> ManagedRemoteExecutionClient;

    fn digest_config(&self) -> DigestConfig;

    /// Obtain per-command knobs for RunAction.
    fn run_action_knobs(&self) -> RunActionKnobs;

    fn cancellation_context(&self) -> &CancellationContext;

    /// I/O layer access to add non-source files (e.g. downloaded files) to
    /// offline archive trace. If None, tracing is not enabled.
    fn io_provider(&self) -> Arc<dyn IoProvider>;
}

#[derive(Error, Debug)]
pub enum ActionErrors {
    #[error("Output path for artifact or metadata file cannot be empty.")]
    EmptyOutputPath,
    #[error(
        "Multiple artifacts and/or metadata files are declared at the same output location `{0}`."
    )]
    ConflictingOutputPath(ForwardRelativePathBuf),
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
#[display(fmt = "Action(key={}, name={})", key, "action.name()")]
pub struct RegisteredAction {
    /// The key uniquely identifies a registered action.
    /// The key to the action is a one to one mapping.
    key: ActionKey,
    #[derivative(Hash = "ignore", PartialEq = "ignore")]
    action: Box<dyn Action>,
    #[derivative(Hash = "ignore", PartialEq = "ignore")]
    executor_config: Arc<CommandExecutorConfig>,
}

impl TrivialDeferred for Arc<RegisteredAction> {
    fn as_any_value(&self) -> &dyn AnyValue {
        self
    }

    fn provide<'a>(&'a self, demand: &mut Demand<'a>) {
        demand.provide_value_with(|| {
            ProvideOutputs(
                self.action
                    .outputs()
                    .map(|outputs| outputs.iter().cloned().collect()),
            )
        });
    }
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
        self.key.deferred_key().owner()
    }

    pub fn key(&self) -> &ActionKey {
        &self.key
    }

    pub fn execution_config(&self) -> &CommandExecutorConfig {
        &self.executor_config
    }

    pub fn category(&self) -> &Category {
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

/// An 'UnregisteredAction' that is stored by the 'ActionRegistry' to be registered.
/// The stored inputs have not yet been validated as bound, but will be validated upon registering.
#[derive(Allocative)]
struct ActionToBeRegistered {
    inputs: IndexSet<ArtifactGroup>,
    outputs: IndexSet<BuildArtifact>,
    action: Box<dyn UnregisteredAction>,
}

impl ActionToBeRegistered {
    fn new<A: UnregisteredAction + 'static>(
        inputs: IndexSet<ArtifactGroup>,
        outputs: IndexSet<BuildArtifact>,
        a: A,
    ) -> Self {
        Self {
            inputs,
            outputs,
            action: Box::new(a),
        }
    }

    fn register(self, starlark_data: Option<OwnedFrozenValue>) -> anyhow::Result<Box<dyn Action>> {
        self.action
            .register(self.inputs, self.outputs, starlark_data)
    }
}
