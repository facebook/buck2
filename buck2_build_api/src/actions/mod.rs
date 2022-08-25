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
pub mod artifact_utils;
pub mod build_listener;
pub mod calculation;
pub mod copy;
pub mod digest;
pub mod directory;
pub mod download_file;
pub mod run;
pub mod symlinked_dir;
pub mod write;
pub mod write_json;
pub mod write_macros;

use std::borrow::Cow;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt::Debug;
use std::path::PathBuf;
use std::sync::Arc;

use async_trait::async_trait;
use buck2_core::category::Category;
use buck2_core::directory::Directory;
use buck2_core::directory::DirectoryBuilder;
use buck2_core::directory::DirectoryEntry;
use buck2_core::directory::DirectoryInsertError;
use buck2_core::directory::DirectoryIterator;
use buck2_core::directory::NoHasher;
use buck2_core::fs::paths::ForwardRelativePath;
use buck2_core::fs::paths::ForwardRelativePathBuf;
use buck2_core::soft_error;
use buck2_events::dispatch::EventDispatcher;
use buck2_node::configuration::execution::ExecutionPlatformResolution;
use buck2_node::execute::config::CommandExecutorConfig;
use derivative::Derivative;
use derive_more::Display;
use gazebo::prelude::*;
use indexmap::indexmap;
use indexmap::IndexMap;
use indexmap::IndexSet;
use once_cell::sync::Lazy;
use starlark::values::OwnedFrozenValue;
use static_assertions::_core::ops::Deref;
use thiserror::Error;

use crate::actions::artifact::fs::ArtifactFs;
use crate::actions::artifact::fs::ExecutorFs;
use crate::actions::artifact::ArtifactValue;
use crate::actions::artifact::BuildArtifact;
use crate::actions::artifact::DeclaredArtifact;
use crate::actions::artifact::OutputArtifact;
use crate::actions::run::knobs::RunActionKnobs;
use crate::analysis::registry::AnalysisValueFetcher;
use crate::artifact_groups::ArtifactGroup;
use crate::artifact_groups::ArtifactGroupValues;
use crate::deferred::BaseDeferredKey;
use crate::deferred::Deferred;
use crate::deferred::DeferredCtx;
use crate::deferred::DeferredData;
use crate::deferred::DeferredId;
use crate::deferred::DeferredInput;
use crate::deferred::DeferredRegistry;
use crate::deferred::DeferredValue;
use crate::deferred::ReservedDeferredData;
use crate::events::proto::ToProtoMessage;
use crate::execute::blocking::BlockingExecutor;
use crate::execute::commands::CommandExecutionOutput;
use crate::execute::commands::CommandExecutionRequest;
use crate::execute::commands::CommandExecutionTarget;
use crate::execute::materializer::Materializer;
use crate::execute::ActionExecutionMetadata;
use crate::execute::ActionOutputs;
use crate::path::BuckOutPath;

/// Represents an unregistered 'Action' that will be registered into the 'Actions' module.
/// The 'UnregisteredAction' is not executable until it is registered, upon which it becomes an
/// 'Action' that is executable.
pub trait UnregisteredAction {
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
pub trait Action: Debug + Send + Sync + 'static {
    /// A machine readable kind identifying this type of action.
    fn kind(&self) -> buck2_data::ActionKind;

    /// All the input 'Artifact's, both sources and built artifacts, that are required for
    /// executing this artifact. While nothing enforces it, this should be a pure function.
    fn inputs(&self) -> anyhow::Result<Cow<'_, IndexSet<ArtifactGroup>>>;

    /// All the outputs this 'Artifact' will generate. Just like inputs, this should be a pure
    /// function.
    fn outputs(&self) -> anyhow::Result<Cow<'_, IndexSet<BuildArtifact>>>;

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
    /// Not required; if None, only one action will be given in the given category.
    fn identifier(&self) -> Option<&str> {
        // TODO(swgillespie) provide this for action implementations
        None
    }

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
    fn target(&self) -> CommandExecutionTarget<'_>;

    /// An 'ArtifactFs' to be used for managing 'Artifact's
    fn fs(&self) -> &ArtifactFs;

    fn executor_fs(&self) -> ExecutorFs;

    /// A `Materializer` used for expensive materializations
    fn materializer(&self) -> &dyn Materializer;

    fn events(&self) -> &EventDispatcher;

    /// Executes a command
    async fn exec_cmd(
        &mut self,
        request: &CommandExecutionRequest,
    ) -> anyhow::Result<(
        IndexMap<CommandExecutionOutput, ArtifactValue>,
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

    /// Obtian per-command knobs for RunAction.
    fn run_action_knobs(&self) -> RunActionKnobs;
}

/// The actions registry for a particular analysis of a rule implementation
pub struct ActionsRegistry {
    owner: BaseDeferredKey,
    action_key: Option<Arc<str>>,
    artifacts: IndexSet<DeclaredArtifact>,
    pending: Vec<(
        ReservedDeferredData<Arc<RegisteredAction>>,
        ActionToBeRegistered,
    )>,
    execution_platform: ExecutionPlatformResolution,
    claimed_output_paths: DirectoryBuilder<(), NoHasher>,
}

#[derive(Error, Debug)]
pub(crate) enum ActionErrors {
    #[error("Output path for artifact or metadata file cannot be empty.")]
    EmptyOutputPath,
    #[error(
        "Multiple artifacts and/or metadata files are declared at the same output location `{0}`."
    )]
    ConflictingOutputPath(ForwardRelativePathBuf),
    #[error(
        "Multiple artifacts and/or metadata files are declared at conflicting output locations. Output path `{0}` conflicts with the following output paths: {1:?}."
    )]
    ConflictingOutputPaths(ForwardRelativePathBuf, Vec<PathBuf>),
    #[error(
        "Action category `{0}` contains duplicate identifier `{1}`; category-identifier pairs must be unique within a rule"
    )]
    ActionCategoryIdentifierNotUnique(Category, String),
    #[error(
        "Analysis produced multiple actions with category `{0}` and no identifier. Add an identifier to these actions to disambiguate them"
    )]
    ActionCategoryDuplicateSingleton(Category),
}

impl ActionsRegistry {
    pub fn new(owner: BaseDeferredKey, execution_platform: ExecutionPlatformResolution) -> Self {
        Self {
            owner,
            action_key: None,
            artifacts: Default::default(),
            pending: Default::default(),
            execution_platform,
            claimed_output_paths: DirectoryBuilder::empty(),
        }
    }

    pub fn set_action_key(&mut self, action_key: Arc<str>) {
        self.action_key = Some(action_key);
    }

    pub fn declare_dynamic_output(&mut self, path: BuckOutPath) -> DeclaredArtifact {
        // We don't want to claim path, because the output belongs to different (outer) context
        DeclaredArtifact::new(path)
    }

    pub fn claim_output_path(&mut self, path: &ForwardRelativePath) -> anyhow::Result<()> {
        match self
            .claimed_output_paths
            .insert(path, DirectoryEntry::Leaf(()))
        {
            Ok(None) => Ok(()),
            Ok(Some(conflict)) => match conflict {
                DirectoryEntry::Leaf(_payload) => Err(anyhow::anyhow!(
                    ActionErrors::ConflictingOutputPath(path.to_owned())
                )),
                DirectoryEntry::Dir(conflict_dir) => {
                    let conflicting_paths = conflict_dir
                        .ordered_walk()
                        .with_paths()
                        .filter_map(|(p, i)| match i {
                            DirectoryEntry::Leaf(_) => Some(path.join(p).to_string().into()),
                            _ => None,
                        })
                        .collect::<Vec<_>>();
                    Err(anyhow::anyhow!(ActionErrors::ConflictingOutputPaths(
                        path.to_owned(),
                        conflicting_paths,
                    )))
                }
            },
            Err(DirectoryInsertError::EmptyPath) => {
                Err(anyhow::anyhow!(ActionErrors::EmptyOutputPath))
            }
            Err(DirectoryInsertError::CannotTraverseLeaf { path: conflict }) => {
                Err(anyhow::anyhow!(ActionErrors::ConflictingOutputPaths(
                    path.to_owned(),
                    vec![conflict.to_string().into()],
                )))
            }
        }
    }

    /// Temporary wrapper for gradual rollout of conflicts detection
    fn soft_claim_output_path(&mut self, path: &ForwardRelativePath) -> anyhow::Result<()> {
        let result = self.claim_output_path(path);
        match result {
            Ok(x) => Ok(x),
            Err(e) => {
                if let Some(action_error) = e.downcast_ref::<ActionErrors>() {
                    // Only fail on exact conflicts where two artifacts have exactly same paths.
                    if let ActionErrors::ConflictingOutputPath(_) = action_error {
                        Err(e)
                    } else {
                        soft_error!(e)?;
                        Ok(())
                    }
                } else {
                    panic!("expected one of `ActionErrors`");
                }
            }
        }
    }

    /// Declares a new output file that will be generated by some action.
    pub fn declare_artifact(
        &mut self,
        prefix: Option<ForwardRelativePathBuf>,
        path: ForwardRelativePathBuf,
    ) -> anyhow::Result<DeclaredArtifact> {
        let (path, hidden) = match prefix {
            None => (path, 0),
            Some(prefix) => (prefix.join(path), prefix.iter().count()),
        };
        self.soft_claim_output_path(&path)?;
        let mut out_path = BuckOutPath::with_hidden(self.owner.dupe(), path, hidden);
        if let Some(action_key) = &self.action_key {
            out_path.set_action_key(action_key.dupe());
        }
        let declared = DeclaredArtifact::new(out_path);
        if !self.artifacts.insert(declared.dupe()) {
            panic!("not expected duplicate artifact after output path was successfully claimed");
        }
        Ok(declared)
    }

    /// Registers the supplied action
    pub fn register<A: UnregisteredAction + 'static>(
        &mut self,
        registry: &mut DeferredRegistry,
        inputs: IndexSet<ArtifactGroup>,
        outputs: IndexSet<OutputArtifact>,
        action: A,
    ) -> anyhow::Result<DeferredId> {
        let reserved = registry.reserve::<Arc<RegisteredAction>>();

        let mut bound_outputs = IndexSet::new();
        for output in outputs {
            let bound = output
                .bind(reserved.data().dupe())?
                .as_base_artifact()
                .dupe();
            bound_outputs.insert(bound);
        }
        let id = reserved.data().deferred_key().id();
        self.pending.push((
            reserved,
            ActionToBeRegistered::new(inputs, bound_outputs, action),
        ));

        Ok(id)
    }

    /// Consumes the registry so no more 'Action's can be registered. This returns
    /// an 'ActionAnalysisResult' that holds all the registered 'Action's
    pub(crate) fn ensure_bound(
        self,
        registry: &mut DeferredRegistry,
        analysis_value_fetcher: &AnalysisValueFetcher,
    ) -> anyhow::Result<()> {
        for artifact in self.artifacts {
            artifact.ensure_bound()?;
        }

        // Buck2 has an invariant that pairs of categories and identifiers are unique throughout a build. That
        // invariant is enforced here, using observed_names to keep track of the categories and identifiers that we've seen.
        let mut observed_names: HashMap<Category, HashSet<String>> = HashMap::new();
        for (key, a) in self.pending.into_iter() {
            let starlark_data = analysis_value_fetcher.get(key.data().deferred_key().id());
            let action_key = key.data().dupe();
            let action = a.register(starlark_data)?;
            match (action.category(), action.identifier()) {
                (category, Some(identifier)) => {
                    let existing_identifiers = observed_names
                        .entry(category.clone())
                        .or_insert_with(HashSet::<String>::new);
                    // false -> identifier was already present in the set
                    if !existing_identifiers.insert(identifier.to_owned()) {
                        return Err(ActionErrors::ActionCategoryIdentifierNotUnique(
                            category.clone(),
                            identifier.to_owned(),
                        )
                        .into());
                    }
                }
                (category, None) => {
                    if observed_names
                        .insert(category.clone(), HashSet::new())
                        .is_some()
                    {
                        return Err(ActionErrors::ActionCategoryDuplicateSingleton(
                            category.clone(),
                        )
                        .into());
                    };
                }
            }

            registry.bind(
                key,
                RegisteredActionData(Arc::new(RegisteredAction::new(
                    action_key,
                    action,
                    self.execution_platform.executor_config()?.clone(),
                ))),
            );
        }

        Ok(())
    }
}

#[derive(Derivative, Debug, Display)]
#[derivative(Eq, Hash, PartialEq)]
#[display(fmt = "Action(key={}, name={})", key, "action.name()")]
pub struct RegisteredAction {
    /// The key uniquely identifies a registered action.
    /// The key to the action is a one to one mapping.
    key: ActionKey,
    #[derivative(Hash = "ignore", PartialEq = "ignore", PartialOrd = "ignore")]
    action: Box<dyn Action>,
    #[derivative(Hash = "ignore", PartialEq = "ignore", PartialOrd = "ignore")]
    executor_config: CommandExecutorConfig,
}

impl RegisteredAction {
    pub fn new(
        key: ActionKey,
        action: Box<dyn Action>,
        executor_config: CommandExecutorConfig,
    ) -> Self {
        Self {
            key,
            action,
            executor_config,
        }
    }

    pub fn action(&self) -> &dyn Action {
        &*self.action
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
        &*self.action
    }
}

#[derive(Clone, Dupe)]
struct RegisteredActionData(Arc<RegisteredAction>);

impl Deferred for RegisteredActionData {
    type Output = Arc<RegisteredAction>;

    fn inputs(&self) -> &IndexSet<DeferredInput> {
        static INPUTS: Lazy<IndexSet<DeferredInput>> = Lazy::new(IndexSet::new);
        &*INPUTS
    }

    fn execute(&self, _ctx: &mut dyn DeferredCtx) -> anyhow::Result<DeferredValue<Self::Output>> {
        Ok(DeferredValue::Ready(self.0.dupe()))
    }
}

/// An 'UnregisteredAction' that is stored by the 'ActionRegistry' to be registered.
/// The stored inputs have not yet been validated as bound, but will be validated upon registering.
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
            action: box a,
        }
    }

    fn register(self, starlark_data: Option<OwnedFrozenValue>) -> anyhow::Result<Box<dyn Action>> {
        self.action
            .register(self.inputs, self.outputs, starlark_data)
    }
}

/// A key to look up an 'Action' from the 'ActionAnalysisResult'.
/// Since 'Action's are registered as 'Deferred's
pub type ActionKey = DeferredData<Arc<RegisteredAction>>;

impl ToProtoMessage for ActionKey {
    type Message = buck2_data::ActionKey;

    fn as_proto(&self) -> Self::Message {
        buck2_data::ActionKey {
            id: self.deferred_key().id().as_usize().to_ne_bytes().to_vec(),
            owner: Some(match self.deferred_key().owner() {
                BaseDeferredKey::TargetLabel(t) => {
                    buck2_data::action_key::Owner::TargetLabel(t.as_proto())
                }
                BaseDeferredKey::BxlLabel(l) => buck2_data::action_key::Owner::BxlKey(l.as_proto()),
            }),
            key: self.deferred_key().action_key(),
        }
    }
}

pub mod testing {
    use std::borrow::Cow;

    use async_trait::async_trait;
    use buck2_core::category::Category;
    use derivative::Derivative;
    use indexmap::IndexSet;
    use starlark::values::OwnedFrozenValue;

    use crate::actions::artifact::BuildArtifact;
    use crate::actions::Action;
    use crate::actions::ActionExecutable;
    use crate::actions::ActionExecutionCtx;
    use crate::actions::ArtifactGroup;
    use crate::actions::PristineActionExecutable;
    use crate::actions::UnregisteredAction;
    use crate::execute::commands::CommandExecutionInput;
    use crate::execute::commands::CommandExecutionRequest;
    use crate::execute::ActionExecutionMetadata;
    use crate::execute::ActionOutputs;

    /// A simple unregistered action that will eventually be resolved into an action that runs the
    /// given cmd as the action execution command. Used for testing
    ///
    /// This action is for testing, and bypasses the need to create starlark values and frozen
    /// modules
    pub struct SimpleUnregisteredAction {
        cmd: Vec<String>,
        inputs: Vec<CommandExecutionInput>,
        category: Category,
        identifier: Option<String>,
    }

    impl SimpleUnregisteredAction {
        pub fn new(
            cmd: Vec<String>,
            inputs: Vec<CommandExecutionInput>,
            category: Category,
            identifier: Option<String>,
        ) -> Self {
            Self {
                cmd,
                inputs,
                category,
                identifier,
            }
        }
    }

    /// The action created by SimpleUnregisteredAction, or directly.
    #[derive(Derivative)]
    #[derivative(Debug)]
    pub struct SimpleAction {
        inputs: IndexSet<ArtifactGroup>,
        outputs: IndexSet<BuildArtifact>,
        cmd: Vec<String>,
        #[derivative(Debug = "ignore")]
        inputmap: Vec<CommandExecutionInput>,
        category: Category,
        identifier: Option<String>,
    }

    impl SimpleAction {
        pub fn new(
            inputs: IndexSet<ArtifactGroup>,
            outputs: IndexSet<BuildArtifact>,
            cmd: Vec<String>,
            inputmap: Vec<CommandExecutionInput>,
            category: Category,
            identifier: Option<String>,
        ) -> Self {
            Self {
                inputs,
                outputs,
                cmd,
                inputmap,
                category,
                identifier,
            }
        }
    }

    impl UnregisteredAction for SimpleUnregisteredAction {
        fn register(
            self: Box<Self>,
            inputs: IndexSet<ArtifactGroup>,
            outputs: IndexSet<BuildArtifact>,
            _starlark_data: Option<OwnedFrozenValue>,
        ) -> anyhow::Result<Box<dyn Action>> {
            Ok(box SimpleAction {
                inputs,
                outputs,
                cmd: self.cmd,
                inputmap: self.inputs,
                category: self.category,
                identifier: self.identifier,
            })
        }
    }

    #[async_trait]
    impl Action for SimpleAction {
        fn kind(&self) -> buck2_data::ActionKind {
            buck2_data::ActionKind::NotSet
        }

        fn inputs(&self) -> anyhow::Result<Cow<'_, IndexSet<ArtifactGroup>>> {
            Ok(Cow::Borrowed(&self.inputs))
        }

        fn outputs(&self) -> anyhow::Result<Cow<'_, IndexSet<BuildArtifact>>> {
            Ok(Cow::Borrowed(&self.outputs))
        }

        fn as_executable(&self) -> ActionExecutable<'_> {
            ActionExecutable::Pristine(self)
        }

        fn category(&self) -> &Category {
            &self.category
        }

        fn identifier(&self) -> Option<&str> {
            self.identifier.as_deref()
        }
    }

    #[async_trait]
    impl PristineActionExecutable for SimpleAction {
        async fn execute(
            &self,
            ctx: &mut dyn ActionExecutionCtx,
        ) -> anyhow::Result<(ActionOutputs, ActionExecutionMetadata)> {
            let req = CommandExecutionRequest::new(
                self.cmd.clone(),
                self.inputmap.clone(),
                self.outputs.clone(),
                hashmap![],
            );

            let (outputs, meta) = ctx.exec_cmd(&req).await?;

            let outputs = outputs
                .into_iter()
                .filter_map(|(o, v)| Some((o.into_build_artifact()?, v)))
                .collect();
            let outputs = ActionOutputs::new(outputs);

            Ok((outputs, meta))
        }
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use assert_matches::assert_matches;
    use buck2_core::category::Category;
    use buck2_core::configuration::Configuration;
    use buck2_core::fs::paths::ForwardRelativePathBuf;
    use buck2_core::package::testing::PackageExt;
    use buck2_core::package::Package;
    use buck2_core::target::testing::ConfiguredTargetLabelExt;
    use buck2_core::target::ConfiguredTargetLabel;
    use buck2_core::target::TargetName;
    use buck2_node::configuration::execution::ExecutionPlatform;
    use buck2_node::configuration::execution::ExecutionPlatformResolution;
    use buck2_node::execute::config::CommandExecutorConfig;
    use gazebo::prelude::*;
    use indexmap::indexset;

    use crate::actions::artifact::testing::ArtifactTestingExt;
    use crate::actions::artifact::testing::BuildArtifactTestingExt;
    use crate::actions::artifact::BuildArtifact;
    use crate::actions::testing::SimpleUnregisteredAction;
    use crate::actions::ActionErrors;
    use crate::actions::ActionsRegistry;
    use crate::actions::ArtifactGroup;
    use crate::analysis::registry::AnalysisValueFetcher;
    use crate::deferred::testing::DeferredIdExt;
    use crate::deferred::BaseDeferredKey;
    use crate::deferred::BaseKey;
    use crate::deferred::DeferredId;
    use crate::deferred::DeferredRegistry;
    use crate::path::BuckOutPath;

    #[test]
    fn declaring_artifacts() -> anyhow::Result<()> {
        let base = BaseDeferredKey::TargetLabel(ConfiguredTargetLabel::testing_new(
            Package::testing_new("cell", "pkg"),
            TargetName::unchecked_new("foo"),
            Configuration::testing_new(),
        ));
        let mut actions =
            ActionsRegistry::new(base.dupe(), ExecutionPlatformResolution::unspecified());
        let out1 = ForwardRelativePathBuf::unchecked_new("bar.out".into());
        let buckout1 = BuckOutPath::new(base.dupe(), out1.clone());
        let declared1 = actions.declare_artifact(None, out1.clone())?;
        declared1
            .get_path()
            .with_full_path(|p| assert_eq!(p, buckout1.path()));

        let out2 = ForwardRelativePathBuf::unchecked_new("bar2.out".into());
        let buckout2 = BuckOutPath::new(base, out2.clone());
        let declared2 = actions.declare_artifact(None, out2)?;
        declared2
            .get_path()
            .with_full_path(|p| assert_eq!(p, buckout2.path()));

        if actions.declare_artifact(None, out1).is_ok() {
            panic!("should error due to duplicate artifact")
        }

        assert_eq!(actions.artifacts.contains(&declared1), true);
        assert_eq!(actions.artifacts.contains(&declared2), true);

        Ok(())
    }

    #[test]
    fn claiming_conflicting_path() -> anyhow::Result<()> {
        let target = ConfiguredTargetLabel::testing_new(
            Package::testing_new("cell", "pkg"),
            TargetName::unchecked_new("my_target"),
            Configuration::testing_new(),
        );
        let mut actions = ActionsRegistry::new(
            BaseDeferredKey::TargetLabel(target.dupe()),
            ExecutionPlatformResolution::unspecified(),
        );

        let out1 = ForwardRelativePathBuf::unchecked_new("foo/a/1".into());
        actions.claim_output_path(&out1)?;

        let out2 = ForwardRelativePathBuf::unchecked_new("foo/a/2".into());
        actions.claim_output_path(&out2)?;

        {
            let expected_conflicts: Vec<PathBuf> = vec!["foo/a/1".into()];
            let prefix_claimed = ForwardRelativePathBuf::unchecked_new("foo/a/1/some/path".into());
            assert_matches!(
                actions.claim_output_path(&prefix_claimed),
                Err(e) => {
                    assert_matches!(
                        e.downcast_ref::<ActionErrors>(),
                        Some(ActionErrors::ConflictingOutputPaths(_inserted, existing)) => {
                            assert_eq!(existing, &expected_conflicts);
                        }
                    );
                }
            );
        }

        assert_matches!(
            actions.claim_output_path(&out1),
            Err(e) => {
                assert_matches!(
                    e.downcast_ref::<ActionErrors>(),
                    Some(ActionErrors::ConflictingOutputPath(_))
                );
            }
        );

        {
            let overwrite_dir = ForwardRelativePathBuf::unchecked_new("foo".into());
            let expected_conflicts: Vec<PathBuf> = vec!["foo/a/1".into(), "foo/a/2".into()];
            assert_matches!(
                actions.claim_output_path(&overwrite_dir),
                Err(e) => {
                    assert_matches!(
                        e.downcast_ref::<ActionErrors>(),
                        Some(ActionErrors::ConflictingOutputPaths(_inserted, existing)) => {
                            assert_eq!(existing, &expected_conflicts);
                        }
                    );
                }
            );
        }

        Ok(())
    }

    #[test]
    fn register_actions() -> anyhow::Result<()> {
        let base = BaseDeferredKey::TargetLabel(ConfiguredTargetLabel::testing_new(
            Package::testing_new("cell", "pkg"),
            TargetName::unchecked_new("foo"),
            Configuration::testing_new(),
        ));
        let mut deferreds = DeferredRegistry::new(BaseKey::Base(base.dupe()));
        let mut actions =
            ActionsRegistry::new(base.dupe(), ExecutionPlatformResolution::unspecified());
        let out = ForwardRelativePathBuf::unchecked_new("bar.out".into());
        let declared = actions.declare_artifact(None, out)?;

        let inputs = indexset![ArtifactGroup::Artifact(
            BuildArtifact::testing_new(
                base.unpack_target_label().unwrap().dupe(),
                ForwardRelativePathBuf::unchecked_new("input".into()),
                DeferredId::testing_new(1),
            )
            .into()
        )];
        let outputs = indexset![declared.as_output()];

        let unregistered_action = SimpleUnregisteredAction::new(
            vec![],
            vec![],
            Category::try_from("fake_action").unwrap(),
            None,
        );
        assert_eq!(
            actions
                .register(&mut deferreds, inputs, outputs, unregistered_action)
                .is_ok(),
            true
        );

        assert_eq!(actions.pending.len(), 1);
        assert_eq!(declared.testing_is_bound(), true);
        assert_eq!(
            actions
                .pending
                .iter()
                .any(|(reserved, _)| reserved.data() == &declared.testing_action_key().unwrap()),
            true
        );

        Ok(())
    }

    #[test]
    fn finalizing_actions() -> anyhow::Result<()> {
        let base = BaseDeferredKey::TargetLabel(ConfiguredTargetLabel::testing_new(
            Package::testing_new("cell", "pkg"),
            TargetName::unchecked_new("foo"),
            Configuration::testing_new(),
        ));
        let mut deferreds = DeferredRegistry::new(BaseKey::Base(base.dupe()));
        let mut actions = ActionsRegistry::new(
            base.dupe(),
            ExecutionPlatformResolution::new(
                Some(ExecutionPlatform::legacy_execution_platform(
                    CommandExecutorConfig::testing_local(),
                    Configuration::testing_new(),
                )),
                Vec::new(),
            ),
        );
        let out = ForwardRelativePathBuf::unchecked_new("bar.out".into());
        let declared = actions.declare_artifact(None, out)?;

        let inputs = indexset![ArtifactGroup::Artifact(
            BuildArtifact::testing_new(
                base.unpack_target_label().unwrap().dupe(),
                ForwardRelativePathBuf::unchecked_new("input".into()),
                DeferredId::testing_new(1),
            )
            .into()
        )];
        let outputs = indexset![declared.as_output()];

        let unregistered_action = SimpleUnregisteredAction::new(
            vec![],
            vec![],
            Category::try_from("fake_action").unwrap(),
            None,
        );
        actions.register(&mut deferreds, inputs, outputs, unregistered_action)?;

        let result = actions.ensure_bound(&mut deferreds, &AnalysisValueFetcher::default());
        assert_eq!(result.is_ok(), true, "Expected Ok(_), got `{:?}`", result);

        let registered_deferreds = deferreds.take_result()?;

        assert_eq!(registered_deferreds.len(), 1);

        assert_eq!(
            registered_deferreds
                .get(&declared.testing_action_key().unwrap().deferred_key().id())
                .is_some(),
            true
        );

        Ok(())
    }

    #[test]
    fn duplicate_category_singleton_actions() {
        let result =
            category_identifier_test(&[("singleton_category", None), ("singleton_category", None)])
                .unwrap_err()
                .downcast::<ActionErrors>()
                .unwrap();

        assert!(matches!(
            result,
            ActionErrors::ActionCategoryDuplicateSingleton(_)
        ));
    }

    #[test]
    fn duplicate_category_identifier() {
        let result = category_identifier_test(&[
            ("cxx_compile", Some("foo.cpp")),
            ("cxx_compile", Some("foo.cpp")),
        ])
        .unwrap_err()
        .downcast::<ActionErrors>()
        .unwrap();

        assert!(matches!(
            result,
            ActionErrors::ActionCategoryIdentifierNotUnique(_, _)
        ),);
    }

    fn category_identifier_test(
        action_names: &[(&'static str, Option<&'static str>)],
    ) -> anyhow::Result<()> {
        let base = BaseDeferredKey::TargetLabel(ConfiguredTargetLabel::testing_new(
            Package::testing_new("cell", "pkg"),
            TargetName::unchecked_new("foo"),
            Configuration::testing_new(),
        ));
        let mut deferreds = DeferredRegistry::new(BaseKey::Base(base.dupe()));
        let mut actions = ActionsRegistry::new(
            base.dupe(),
            ExecutionPlatformResolution::new(
                Some(ExecutionPlatform::legacy_execution_platform(
                    CommandExecutorConfig::testing_local(),
                    Configuration::testing_new(),
                )),
                Vec::new(),
            ),
        );
        for (category, identifier) in action_names {
            let unregistered_action = SimpleUnregisteredAction::new(
                vec![],
                vec![],
                Category::try_from(category.to_owned()).unwrap(),
                identifier.map(|i| i.to_owned()),
            );

            actions.register(
                &mut deferreds,
                indexset![],
                indexset![],
                unregistered_action,
            )?;
        }

        actions.ensure_bound(&mut deferreds, &AnalysisValueFetcher::default())
    }
}
