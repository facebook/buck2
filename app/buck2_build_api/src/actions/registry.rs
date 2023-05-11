/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::collections::HashSet;
use std::sync::Arc;

use allocative::Allocative;
use buck2_core::category::Category;
use buck2_core::directory;
use buck2_core::directory::Directory;
use buck2_core::directory::DirectoryBuilder;
use buck2_core::directory::DirectoryEntry;
use buck2_core::directory::DirectoryInsertError;
use buck2_core::directory::DirectoryIterator;
use buck2_core::directory::NoDigest;
use buck2_core::fs::buck_out_path::BuckOutPath;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePath;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePathBuf;
use buck2_execute::execute::request::OutputType;
use buck2_node::configuration::execution::ExecutionPlatformResolution;
use dupe::Dupe;
use indexmap::IndexSet;
use starlark::codemap::FileSpan;

use crate::actions::artifact::artifact_type::DeclaredArtifact;
use crate::actions::artifact::artifact_type::OutputArtifact;
use crate::actions::key::ActionKey;
use crate::actions::ActionErrors;
use crate::actions::ActionToBeRegistered;
use crate::actions::RegisteredAction;
use crate::actions::UnregisteredAction;
use crate::analysis::registry::AnalysisValueFetcher;
use crate::artifact_groups::ArtifactGroup;
use crate::deferred::base_deferred_key::BaseDeferredKey;
use crate::deferred::types::DeferredId;
use crate::deferred::types::DeferredRegistry;
use crate::deferred::types::ReservedTrivialDeferredData;

/// The actions registry for a particular analysis of a rule implementation
#[derive(Allocative)]
pub struct ActionsRegistry {
    owner: BaseDeferredKey,
    action_key: Option<Arc<str>>,
    artifacts: IndexSet<DeclaredArtifact>,
    pending: Vec<(
        ReservedTrivialDeferredData<Arc<RegisteredAction>>,
        ActionToBeRegistered,
    )>,
    execution_platform: ExecutionPlatformResolution,
    claimed_output_paths: DirectoryBuilder<Option<FileSpan>, NoDigest>,
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

    pub fn declare_dynamic_output(
        &mut self,
        path: BuckOutPath,
        output_type: OutputType,
    ) -> DeclaredArtifact {
        // We don't want to claim path, because the output belongs to different (outer) context. We
        // also don't care to keep track of the hidden components count since this output will
        // never escape the dynamic lambda.
        DeclaredArtifact::new(path, output_type, 0)
    }

    pub fn claim_output_path(
        &mut self,
        path: &ForwardRelativePath,
        declaration_location: Option<FileSpan>,
    ) -> anyhow::Result<()> {
        match self
            .claimed_output_paths
            .insert(path, DirectoryEntry::Leaf(declaration_location))
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
                        .filter_map(|(p, entry)| match entry {
                            DirectoryEntry::Leaf(location) => Some(format!(
                                "{} declared at {}",
                                path.join(p),
                                location
                                    .as_ref()
                                    .map_or(&"<unknown>" as _, |l| l as &dyn std::fmt::Display)
                            )),
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
                let location = match directory::find(&self.claimed_output_paths, &conflict) {
                    Ok(Some(DirectoryEntry::Leaf(l))) => l.as_ref(),
                    _ => None,
                };

                let conflict = format!(
                    "{} declared at {}",
                    conflict,
                    location
                        .as_ref()
                        .map_or(&"<unknown>" as _, |l| l as &dyn std::fmt::Display)
                );

                Err(anyhow::anyhow!(ActionErrors::ConflictingOutputPaths(
                    path.to_owned(),
                    vec![conflict],
                )))
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
    ) -> anyhow::Result<DeclaredArtifact> {
        let (path, hidden) = match prefix {
            None => (path, 0),
            Some(prefix) => (prefix.join(path), prefix.iter().count()),
        };
        self.claim_output_path(&path, declaration_location)?;
        let out_path = BuckOutPath::with_action_key(
            self.owner.dupe().into_dyn(),
            path,
            self.action_key.dupe(),
        );
        let declared = DeclaredArtifact::new(out_path, output_type, hidden);
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
        let reserved = registry.reserve_trivial::<Arc<RegisteredAction>>();

        let mut bound_outputs = IndexSet::with_capacity(outputs.len());
        for output in outputs {
            let bound = output
                .bind(ActionKey::new(reserved.data().dupe()))?
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
    pub fn ensure_bound(
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
            let starlark_data = analysis_value_fetcher.get(key.data().deferred_key().id())?;
            let action_key = ActionKey::new(key.data().dupe());
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

            registry.bind_trivial(
                key,
                Arc::new(RegisteredAction::new(
                    action_key,
                    action,
                    (*self.execution_platform.executor_config()?).dupe(),
                )),
            );
        }

        Ok(())
    }

    pub fn testing_artifacts(&self) -> &IndexSet<DeclaredArtifact> {
        &self.artifacts
    }

    pub fn testing_pending(
        &self,
    ) -> impl Iterator<Item = &ReservedTrivialDeferredData<Arc<RegisteredAction>>> {
        self.pending.iter().map(|(reserved, _)| reserved)
    }
}
