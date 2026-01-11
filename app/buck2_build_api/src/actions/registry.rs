/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::collections::HashMap;
use std::collections::HashSet;
use std::sync::Arc;

use allocative::Allocative;
use buck2_artifact::actions::key::ActionIndex;
use buck2_artifact::actions::key::ActionKey;
use buck2_artifact::artifact::artifact_type::DeclaredArtifact;
use buck2_artifact::artifact::artifact_type::OutputArtifact;
use buck2_artifact::artifact::build_artifact::BuildArtifact;
use buck2_core::category::Category;
use buck2_core::deferred::key::DeferredHolderKey;
use buck2_core::execution_types::execution::ExecutionPlatformResolution;
use buck2_core::fs::buck_out_path::BuckOutPathKind;
use buck2_core::fs::buck_out_path::BuildArtifactPath;
use buck2_directory::directory;
use buck2_directory::directory::builder::DirectoryBuilder;
use buck2_directory::directory::builder::DirectoryInsertError;
use buck2_directory::directory::directory::Directory;
use buck2_directory::directory::directory_hasher::NoDigest;
use buck2_directory::directory::directory_iterator::DirectoryIterator;
use buck2_directory::directory::entry::DirectoryEntry;
use buck2_error::BuckErrorContext;
use buck2_error::internal_error;
use buck2_execute::execute::request::OutputType;
use buck2_fs::paths::forward_rel_path::ForwardRelativePath;
use buck2_fs::paths::forward_rel_path::ForwardRelativePathBuf;
use dupe::Dupe;
use dupe::OptionDupedExt;
use gazebo::prelude::SliceExt;
use indexmap::IndexSet;
use starlark::codemap::FileSpan;
use starlark::collections::SmallMap;
use starlark::collections::SmallSet;
use starlark::values::Heap;
use starlark::values::Trace;

use crate::actions::ActionErrors;
use crate::actions::ActionToBeRegistered;
use crate::actions::RegisteredAction;
use crate::actions::UnregisteredAction;
use crate::analysis::registry::AnalysisValueFetcher;
use crate::deferred::calculation::ActionLookup;

/// The actions registry for a particular analysis of a rule, dynamic actions, anon target, BXL.
#[derive(Allocative, Trace)]
pub struct ActionsRegistry<'v> {
    owner: DeferredHolderKey,
    artifacts: SmallSet<DeclaredArtifact<'v>>,

    // For a dynamic_output, maps the ActionKeys for the outputs that have been bound
    // to this dynamic_output to the DeclaredArtifact created in the dynamic_output.
    declared_dynamic_outputs: SmallMap<ActionKey, DeclaredArtifact<'v>>,
    pending: Vec<ActionToBeRegistered>,
    pub execution_platform: ExecutionPlatformResolution,
    claimed_output_paths: DirectoryBuilder<Option<FileSpan>, NoDigest>,
}

impl<'v> ActionsRegistry<'v> {
    pub fn new(owner: DeferredHolderKey, execution_platform: ExecutionPlatformResolution) -> Self {
        Self {
            owner,
            artifacts: Default::default(),
            declared_dynamic_outputs: SmallMap::new(),
            pending: Default::default(),
            execution_platform,
            claimed_output_paths: DirectoryBuilder::empty(),
        }
    }

    pub fn declare_dynamic_output(
        &mut self,
        artifact: &BuildArtifact,
        heap: Heap<'v>,
    ) -> buck2_error::Result<DeclaredArtifact<'v>> {
        if !self.pending.is_empty() {
            return Err(internal_error!(
                "output for dynamic_output/actions declared after actions: {}, {:?}",
                artifact,
                self.pending.map(|v| v.key())
            ));
        }

        // We don't want to claim path, because the output belongs to different (outer) context.

        // We also don't care to keep track of the hidden components count since this output will
        // never escape the dynamic lambda.
        // TODO(cjhopman): dynamic values mean this can escape. does this need to be updated for that?
        let new_artifact =
            DeclaredArtifact::new(artifact.get_path().dupe(), artifact.output_type(), 0, heap);

        assert!(
            self.declared_dynamic_outputs
                .insert(artifact.key().dupe(), new_artifact.dupe())
                .is_none()
        );

        Ok(new_artifact)
    }

    pub fn claim_output_path(
        &mut self,
        path: &ForwardRelativePath,
        declaration_location: Option<FileSpan>,
    ) -> buck2_error::Result<()> {
        fn display_location_opt(location: Option<&FileSpan>) -> &dyn std::fmt::Display {
            location.map_or(&"<unknown>" as _, |l| l as _)
        }

        match self
            .claimed_output_paths
            .insert(path, DirectoryEntry::Leaf(declaration_location))
        {
            Ok(None) => Ok(()),
            Ok(Some(conflict)) => match conflict {
                DirectoryEntry::Leaf(location) => Err(ActionErrors::ConflictingOutputPath(
                    path.to_owned(),
                    display_location_opt(location.as_ref()).to_string(),
                )
                .into()),
                DirectoryEntry::Dir(conflict_dir) => {
                    let conflicting_paths = conflict_dir
                        .ordered_walk_leaves()
                        .with_paths()
                        .map(|(p, location)| {
                            format!(
                                "{} declared at {}",
                                path.join(p),
                                display_location_opt(location.as_ref()),
                            )
                        })
                        .collect::<Vec<_>>();
                    Err(
                        ActionErrors::ConflictingOutputPaths(path.to_owned(), conflicting_paths)
                            .into(),
                    )
                }
            },
            Err(DirectoryInsertError::EmptyPath) => Err(ActionErrors::EmptyOutputPath.into()),
            Err(DirectoryInsertError::CannotTraverseLeaf { path: conflict }) => {
                let location =
                    match directory::find::find(self.claimed_output_paths.as_ref(), &conflict) {
                        Ok(Some(DirectoryEntry::Leaf(l))) => l.as_ref(),
                        _ => None,
                    };

                let conflict = format!(
                    "{} declared at {}",
                    conflict,
                    display_location_opt(location),
                );

                Err(ActionErrors::ConflictingOutputPaths(path.to_owned(), vec![conflict]).into())
            }
        }
    }

    /// Declares a new output file that will be generated by some action.
    pub fn declare_artifact(
        &mut self,
        prefix: Option<ForwardRelativePathBuf>,
        path: ForwardRelativePathBuf,
        output_type: OutputType,
        declaration_location: Option<FileSpan>,
        path_resolution_method: BuckOutPathKind,
        heap: Heap<'v>,
    ) -> buck2_error::Result<DeclaredArtifact<'v>> {
        let (path, hidden) = match prefix {
            None => (path, 0),
            Some(prefix) => (prefix.join(path), prefix.iter().count()),
        };
        self.claim_output_path(&path, declaration_location)?;
        let out_path = BuildArtifactPath::with_dynamic_actions_action_key(
            self.owner.dupe(),
            path,
            path_resolution_method,
        );
        let declared = DeclaredArtifact::new(out_path, output_type, hidden, heap);
        if !self.artifacts.insert(declared.dupe()) {
            panic!("not expected duplicate artifact after output path was successfully claimed");
        }
        Ok(declared)
    }

    /// Registers the supplied action
    pub fn register<A: UnregisteredAction + 'static>(
        &mut self,
        self_key: &DeferredHolderKey,
        outputs: IndexSet<OutputArtifact>,
        action: A,
    ) -> buck2_error::Result<ActionKey> {
        let key = ActionKey::new(
            self_key.dupe(),
            // If there are declared_dynamic_outputs, then the analysis that created this one has
            // already created ActionKeys for each of those declared outputs and so we offset the
            // index by that number.
            ActionIndex::new(
                (self.declared_dynamic_outputs.len() + self.pending.len()).try_into()?,
            ),
        );
        let mut bound_outputs = IndexSet::with_capacity(outputs.len());
        for output in outputs {
            let bound = output.bind(key.dupe())?.as_base_artifact().dupe();
            bound_outputs.insert(bound);
        }
        self.pending
            .push(ActionToBeRegistered::new(key.dupe(), bound_outputs, action));

        Ok(key)
    }

    /// Consumes the registry so no more 'Action's can be registered. This returns
    /// an 'ActionAnalysisResult' that holds all the registered 'Action's
    pub fn finalize(
        self,
    ) -> buck2_error::Result<
        impl FnOnce(&AnalysisValueFetcher) -> buck2_error::Result<RecordedActions> + use<>,
    > {
        for artifact in self.artifacts {
            artifact.ensure_bound()?;
        }

        let num_action_keys = self.declared_dynamic_outputs.len() + self.pending.len();
        let mut actions = RecordedActions::new(num_action_keys);

        for (key, artifact) in self.declared_dynamic_outputs.into_iter() {
            actions.insert_dynamic_output(key, artifact.ensure_bound()?.action_key().dupe());
        }

        Ok(move |analysis_value_fetcher: &AnalysisValueFetcher| {
            // Buck2 has an invariant that pairs of categories and identifiers are unique throughout a build. That
            // invariant is enforced here, using observed_names to keep track of the categories and identifiers that we've seen.
            let mut observed_names: HashMap<Category, HashSet<String>> = HashMap::new();
            for a in self.pending.into_iter() {
                let key = a.key().dupe();
                let (starlark_data, error_handler) =
                    analysis_value_fetcher.get_action_data(&key)?;
                let action = a.register(starlark_data, error_handler)?;
                match (action.category(), action.identifier()) {
                    (category, Some(identifier)) => {
                        let existing_identifiers = observed_names
                            .entry(category.to_owned())
                            .or_insert_with(HashSet::<String>::new);
                        // false -> identifier was already present in the set
                        if !existing_identifiers.insert(identifier.to_owned()) {
                            return Err(ActionErrors::ActionCategoryIdentifierNotUnique(
                                category.to_owned(),
                                identifier.to_owned(),
                            )
                            .into());
                        }
                    }
                    (category, None) => {
                        if observed_names
                            .insert(category.to_owned(), HashSet::new())
                            .is_some()
                        {
                            return Err(ActionErrors::ActionCategoryDuplicateSingleton(
                                category.to_owned(),
                            )
                            .into());
                        };
                    }
                }

                actions.insert(
                    key.dupe(),
                    Arc::new(RegisteredAction::new(
                        key,
                        action,
                        (*self.execution_platform.executor_config()?).dupe(),
                    )),
                );
            }

            Ok(actions)
        })
    }

    pub fn testing_artifacts(&self) -> &SmallSet<DeclaredArtifact<'v>> {
        &self.artifacts
    }

    pub fn testing_pending_action_keys(&self) -> Vec<ActionKey> {
        self.pending.map(|a| a.key().dupe())
    }

    pub(crate) fn execution_platform(&self) -> &ExecutionPlatformResolution {
        &self.execution_platform
    }

    pub(crate) fn actions_len(&self) -> usize {
        self.pending.len()
    }

    pub(crate) fn artifacts_len(&self) -> usize {
        self.artifacts.len()
    }
}

#[derive(Debug, Allocative)]
pub struct RecordedActions {
    /// Vec of actions indexed by ActionKey::id.
    ///
    /// ActionLookup::Action indicates that this analysis created the action.
    ///
    /// It's possible for an Action to appear in this map multiple times. That can
    /// happen for a dynamic_outputs' "outputs" argument when the output is bound to
    /// an action created in that dynamic_output.
    ///
    /// ActionLookup::Deferred is only used for a dynamic_outputs "outputs" argument
    /// that has been re-bound to another dynamic_output.
    actions: Vec<ActionLookup>,
}

impl RecordedActions {
    pub fn new(capacity: usize) -> Self {
        Self {
            actions: Vec::with_capacity(capacity),
        }
    }

    pub fn insert(&mut self, key: ActionKey, action: Arc<RegisteredAction>) {
        self.insert_action_lookup(key, ActionLookup::Action(action));
    }

    fn insert_action_lookup(&mut self, key: ActionKey, action: ActionLookup) {
        assert!(self.actions.len() == key.action_index().0 as usize);
        self.actions.push(action);
    }

    /// Inserts a binding for a dynamic_outputs' "outputs" arg.
    pub fn insert_dynamic_output(&mut self, key: ActionKey, bound_to_key: ActionKey) {
        // TODO(cjhopman): This doesn't seem to work the way it's intended. We won't ever hit the Some case because we insert all
        // the dynamic_output "outputs" first before inserting the actual registered actions.
        match self.actions.get(bound_to_key.action_index().0 as usize) {
            Some(ActionLookup::Action(v)) => {
                // indicates that a dynamic_output "outputs" has been bound to an action it created
                self.insert_action_lookup(key, ActionLookup::Action(v.dupe()));
            }
            _ => {
                self.insert_action_lookup(key, ActionLookup::Deferred(bound_to_key));
            }
        }
    }

    pub fn lookup(&self, key: &ActionKey) -> buck2_error::Result<ActionLookup> {
        self.actions
            .get(key.action_index().0 as usize)
            .duped()
            .with_internal_error(|| format!("action key missing in recorded actions {key}"))
    }

    /// Iterates over the actions created in this analysis.
    pub fn iter_actions(&self) -> impl Iterator<Item = &Arc<RegisteredAction>> + '_ {
        self.actions.iter().filter_map(|v| match v {
            ActionLookup::Action(a) => Some(a),
            ActionLookup::Deferred(_) => None,
        })
    }
}
