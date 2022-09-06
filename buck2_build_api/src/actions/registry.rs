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
use buck2_execute::base_deferred_key::BaseDeferredKey;
use buck2_execute::path::buck_out_path::BuckOutPath;
use buck2_node::configuration::execution::ExecutionPlatformResolution;
use gazebo::dupe::Dupe;
use indexmap::IndexSet;

use crate::actions::artifact::DeclaredArtifact;
use crate::actions::artifact::OutputArtifact;
use crate::actions::key::ActionKey;
use crate::actions::ActionErrors;
use crate::actions::ActionToBeRegistered;
use crate::actions::RegisteredAction;
use crate::actions::RegisteredActionData;
use crate::actions::UnregisteredAction;
use crate::analysis::registry::AnalysisValueFetcher;
use crate::artifact_groups::ArtifactGroup;
use crate::deferred::types::DeferredId;
use crate::deferred::types::DeferredRegistry;
use crate::deferred::types::ReservedDeferredData;

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
                        soft_error!("conflicting_output_path", e)?;
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
        let out_path = BuckOutPath::with_hidden_and_action_key(
            self.owner.dupe(),
            path,
            hidden,
            self.action_key.dupe(),
        );
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

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use assert_matches::assert_matches;
    use buck2_common::executor_config::CommandExecutorConfig;
    use buck2_core::category::Category;
    use buck2_core::configuration::Configuration;
    use buck2_core::fs::paths::ForwardRelativePathBuf;
    use buck2_core::package::testing::PackageExt;
    use buck2_core::package::Package;
    use buck2_core::target::testing::ConfiguredTargetLabelExt;
    use buck2_core::target::ConfiguredTargetLabel;
    use buck2_core::target::TargetName;
    use buck2_execute::base_deferred_key::BaseDeferredKey;
    use buck2_execute::path::buck_out_path::BuckOutPath;
    use buck2_node::configuration::execution::ExecutionPlatform;
    use buck2_node::configuration::execution::ExecutionPlatformResolution;
    use gazebo::prelude::*;
    use indexmap::indexset;

    use crate::actions::artifact::build_artifact::BuildArtifact;
    use crate::actions::artifact::testing::ArtifactTestingExt;
    use crate::actions::artifact::testing::BuildArtifactTestingExt;
    use crate::actions::registry::ActionsRegistry;
    use crate::actions::testings::SimpleUnregisteredAction;
    use crate::actions::ActionErrors;
    use crate::actions::ArtifactGroup;
    use crate::analysis::registry::AnalysisValueFetcher;
    use crate::deferred::types::testing::DeferredIdExt;
    use crate::deferred::types::BaseKey;
    use crate::deferred::types::DeferredId;
    use crate::deferred::types::DeferredRegistry;

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

        let unregistered_action =
            SimpleUnregisteredAction::new(vec![], Category::try_from("fake_action").unwrap(), None);
        assert_eq!(
            actions
                .register(&mut deferreds, inputs, outputs, unregistered_action)
                .is_ok(),
            true
        );

        assert_eq!(actions.pending.len(), 1);
        assert_eq!(declared.testing_is_bound(), true);
        assert_eq!(
            actions.pending.iter().any(|(reserved, _)| reserved.data()
                == declared.testing_action_key().unwrap().deferred_data()),
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

        let unregistered_action =
            SimpleUnregisteredAction::new(vec![], Category::try_from("fake_action").unwrap(), None);
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
