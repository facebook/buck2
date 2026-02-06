/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::collections::HashSet;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_artifact::actions::key::ActionKey;
use buck2_core::configuration::compatibility::MaybeCompatible;
use buck2_core::deferred::key::DeferredHolderKey;
use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
use buck2_interpreter::dice::starlark_provider::StarlarkEvalKind;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use dice::CancellationContext;
use dice::DiceComputations;
use dice::Key;
use dupe::Dupe;
use fxhash::FxHashSet;
use starlark::values::FrozenHeapRef;

use crate::analysis::calculation::RuleAnalysisCalculation;
use crate::artifact_groups::ArtifactGroup;
use crate::build::graph_properties::ConfiguredGraphPropertiesValues;
use crate::build::sketch_impl::DEFAULT_SKETCH_VERSION;
use crate::build::sketch_impl::MergeableGraphSketch;
use crate::build::sketch_impl::Sketcher;
use crate::deferred::calculation::DeferredHolder;

/// Computes an unweighted sketch of the action graph by traversing from root artifacts.
///
/// This function traverses the action graph starting from the provided root artifacts,
/// sketching each unique action key encountered. The sketch can be used for similarity
/// comparisons between action graphs.
///
/// Returns a tuple of (is_complete, sketch) where is_complete indicates whether the
/// traversal was able to visit all nodes (false if some nodes were missing due to
/// failed analysis or dynamic nodes).
pub fn compute_action_graph_sketch<'a>(
    root_artifacts: impl IntoIterator<Item = &'a ArtifactGroup>,
    state: &fxhash::FxHashMap<DeferredHolderKey, DeferredHolder>,
) -> buck2_error::Result<(bool, MergeableGraphSketch<ActionKey>)> {
    let mut sketcher = DEFAULT_SKETCH_VERSION.create_sketcher();
    let complete = compute_action_graph_sketch_impl(root_artifacts, state, &mut sketcher)?;
    Ok((complete, sketcher.into_mergeable_graph_sketch()))
}

/// Private implementation that accepts any Sketcher for testing.
fn compute_action_graph_sketch_impl<'a>(
    root_artifacts: impl IntoIterator<Item = &'a ArtifactGroup>,
    state: &fxhash::FxHashMap<DeferredHolderKey, DeferredHolder>,
    sketcher: &mut impl Sketcher<ActionKey>,
) -> buck2_error::Result<bool> {
    let (complete, actions) =
        super::implementation::traverse::traverse_partial_action_graph(root_artifacts, state)?;

    for action_key in actions.iter() {
        sketcher.sketch(action_key);
    }

    Ok(complete)
}

/// Returns the total graph size for all dependencies of a target without caching the result on the DICE graph.
/// The cost of storing this on DICE is extremely low, so there's almost no reason to use this function (we currently
/// expose it just for performance testing).
pub fn compute_configured_graph_sketch(
    node: ConfiguredTargetNode,
    compute_sketch: bool,
) -> ConfiguredGraphPropertiesValues {
    let mut sketcher = if compute_sketch {
        Some(DEFAULT_SKETCH_VERSION.create_sketcher())
    } else {
        None
    };
    let size = compute_configured_graph_sketch_impl(&node, &mut sketcher);
    ConfiguredGraphPropertiesValues {
        configured_graph_size: size as _,
        configured_graph_sketch: sketcher.map(|s| s.into_mergeable_graph_sketch()),
    }
}

/// Private implementation that accepts any Sketcher for testing.
fn compute_configured_graph_sketch_impl<'a>(
    node: &'a ConfiguredTargetNode,
    sketcher: &mut impl Sketcher<ConfiguredTargetLabel>,
) -> usize {
    let mut queue = vec![node];
    let mut visited: HashSet<_, fxhash::FxBuildHasher> = HashSet::default();
    visited.insert(node);

    while let Some(item) = queue.pop() {
        for d in item.deps() {
            if visited.insert(d) {
                queue.push(d);
            }
        }

        sketcher.sketch(item.label());
    }

    visited.len()
}

#[derive(
    Clone,
    Dupe,
    derive_more::Display,
    Debug,
    Eq,
    Hash,
    PartialEq,
    Allocative
)]
#[display("AnalysisGraphPropertiesKey({})", label)]
pub(crate) struct AnalysisGraphPropertiesKey {
    pub label: ConfiguredTargetLabel,
}

#[async_trait]
impl Key for AnalysisGraphPropertiesKey {
    type Value = buck2_error::Result<MaybeCompatible<MergeableGraphSketch<StarlarkEvalKind>>>;

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellation: &CancellationContext,
    ) -> Self::Value {
        let analysis_result = ctx.get_analysis_result(&self.label).await?;
        analysis_result.try_map(|analysis_result| {
            Ok(gather_heap_graph_sketch(
                analysis_result
                    .analysis_values()
                    .analysis_storage()?
                    .owner(),
            ))
        })
    }

    fn equality(a: &Self::Value, b: &Self::Value) -> bool {
        match (a, b) {
            (Ok(a), Ok(b)) => a == b,
            _ => false,
        }
    }
}

/// Private implementation that accepts any Sketcher for testing.
fn gather_heap_graph_sketch_impl(
    root: &FrozenHeapRef,
    sketcher: &mut impl Sketcher<StarlarkEvalKind>,
) {
    let mut visited = FxHashSet::default();
    visited.insert(root);
    let mut queue = vec![root];

    while let Some(item) = queue.pop() {
        let Some(name) = item.name() else {
            continue;
        };
        let name = name.downcast_ref::<StarlarkEvalKind>().unwrap();
        sketcher.sketch_weighted(name, item.allocated_bytes() as u64);
        queue.extend(item.refs().filter(|f| visited.insert(*f)));
    }
}

/// Computes a sketch of the memory transitively referenced by the given starlark heap.
///
/// Using the heap graph instead of the configured graph directly is advantageous primarily because
/// analysis results don't otherwise directly encode the structure of the analysis graph. "Guessing"
/// at what the graph is, while probably possible in practice, is a bit brittle. It would also mean
/// that we wouldn't know about anon targets, which would be a bit of a shame.
fn gather_heap_graph_sketch(root: &FrozenHeapRef) -> MergeableGraphSketch<StarlarkEvalKind> {
    let mut sketcher = DEFAULT_SKETCH_VERSION.create_sketcher();
    gather_heap_graph_sketch_impl(root, &mut sketcher);
    sketcher.into_mergeable_graph_sketch()
}

#[cfg(test)]
mod tests {
    use std::borrow::Cow;
    use std::collections::HashMap;
    use std::sync::Arc;

    use allocative::Allocative;
    use async_trait::async_trait;
    use buck2_artifact::actions::key::ActionIndex;
    use buck2_artifact::actions::key::ActionKey;
    use buck2_artifact::artifact::build_artifact::BuildArtifact;
    use buck2_core::category::Category;
    use buck2_core::category::CategoryRef;
    use buck2_core::configuration::data::ConfigurationData;
    use buck2_core::deferred::key::DeferredHolderKey;
    use buck2_core::execution_types::executor_config::CommandExecutorConfig;
    use buck2_core::fs::buck_out_path::BuckOutPathKind;
    use buck2_core::fs::buck_out_path::BuildArtifactPath;
    use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
    use buck2_fs::paths::forward_rel_path::ForwardRelativePathBuf;
    use dupe::Dupe;
    use fxhash::FxHashSet;
    use indexmap::IndexSet;

    use crate::actions::Action;
    use crate::actions::ActionExecutionCtx;
    use crate::actions::RegisteredAction;
    use crate::actions::box_slice_set::BoxSliceSet;
    use crate::actions::execute::action_executor::ActionExecutionMetadata;
    use crate::actions::execute::action_executor::ActionOutputs;
    use crate::actions::execute::error::ExecuteError;
    use crate::actions::registry::RecordedActions;
    use crate::analysis::AnalysisResult;
    use crate::analysis::registry::RecordedAnalysisValues;
    use crate::artifact_groups::ArtifactGroup;
    use crate::build::sketch_impl::Sketcher;
    use crate::deferred::calculation::DeferredHolder;

    /// A mock sketcher that records all sketch calls for verification in tests.
    struct MockSketcher<T: Clone> {
        items: Vec<T>,
    }

    impl<T: Clone> MockSketcher<T> {
        fn new() -> Self {
            Self { items: Vec::new() }
        }
    }

    impl<T: Clone> Sketcher<T> for MockSketcher<T> {
        fn sketch(&mut self, t: &T) {
            self.items.push(t.clone());
        }

        fn sketch_weighted(&mut self, _t: &T, _weight: u64) {
            unimplemented!("sketch_weighted not used in these tests")
        }
    }

    /// A minimal mock action for testing action graph traversal.
    #[derive(Debug, Allocative)]
    struct MockAction {
        inputs: BoxSliceSet<ArtifactGroup>,
        outputs: BoxSliceSet<BuildArtifact>,
        category: Category,
        identifier: Option<String>,
    }

    impl MockAction {
        fn new(
            inputs: IndexSet<ArtifactGroup>,
            outputs: IndexSet<BuildArtifact>,
            identifier: Option<String>,
        ) -> Self {
            Self {
                inputs: BoxSliceSet::from(inputs),
                outputs: BoxSliceSet::from(outputs),
                category: Category::new("test".to_owned()).unwrap(),
                identifier,
            }
        }
    }

    #[async_trait]
    impl Action for MockAction {
        fn kind(&self) -> buck2_data::ActionKind {
            buck2_data::ActionKind::NotSet
        }

        fn inputs(&self) -> buck2_error::Result<Cow<'_, [ArtifactGroup]>> {
            Ok(Cow::Borrowed(self.inputs.as_slice()))
        }

        fn outputs(&self) -> Cow<'_, [BuildArtifact]> {
            Cow::Borrowed(self.outputs.as_slice())
        }

        fn first_output(&self) -> &BuildArtifact {
            &self.outputs.as_slice()[0]
        }

        fn category(&self) -> CategoryRef<'_> {
            self.category.as_ref()
        }

        fn identifier(&self) -> Option<&str> {
            self.identifier.as_deref()
        }

        async fn execute(
            &self,
            _ctx: &mut dyn ActionExecutionCtx,
            _waiting_data: buck2_build_signals::env::WaitingData,
        ) -> Result<(ActionOutputs, ActionExecutionMetadata), ExecuteError> {
            unimplemented!("MockAction::execute should not be called in tests")
        }
    }

    fn create_target(name: &str) -> ConfiguredTargetLabel {
        let cfg = ConfigurationData::testing_new();
        ConfiguredTargetLabel::testing_parse(name, cfg)
    }

    fn create_build_artifact(holder_key: &DeferredHolderKey, index: u32) -> BuildArtifact {
        let action_key = ActionKey::new(holder_key.dupe(), ActionIndex::new(index));
        BuildArtifact::new(
            BuildArtifactPath::new(
                holder_key.owner().dupe(),
                ForwardRelativePathBuf::unchecked_new(format!("output-{index}")),
                BuckOutPathKind::default(),
            ),
            action_key,
            buck2_execute::execute::request::OutputType::File,
        )
        .unwrap()
    }

    fn create_action(
        holder_key: &DeferredHolderKey,
        index: u32,
        inputs: Vec<&ArtifactGroup>,
    ) -> (ActionKey, BuildArtifact, Arc<RegisteredAction>) {
        let action_key = ActionKey::new(holder_key.dupe(), ActionIndex::new(index));
        let output = create_build_artifact(holder_key, index);
        let action = Arc::new(RegisteredAction::new(
            action_key.dupe(),
            Box::new(MockAction::new(
                inputs.into_iter().cloned().collect(),
                IndexSet::from([output.dupe()]),
                Some(format!("action-{index}")),
            )),
            CommandExecutorConfig::testing_local(),
        ));
        (action_key, output, action)
    }

    #[test]
    fn test_action_graph_sketch_impl_empty() {
        let state = fxhash::FxHashMap::default();
        let mut mock_sketcher: MockSketcher<ActionKey> = MockSketcher::new();

        let complete = super::compute_action_graph_sketch_impl(
            Vec::<&ArtifactGroup>::new(),
            &state,
            &mut mock_sketcher,
        )
        .unwrap();

        assert!(complete);
        assert!(mock_sketcher.items.is_empty());
    }

    #[test]
    fn test_action_graph_sketch_impl_missing_state_returns_incomplete() {
        // Test that missing state returns incomplete=false
        let target = create_target("root//:lib");
        let holder_key = DeferredHolderKey::for_analysis(target.dupe());

        let output = create_build_artifact(&holder_key, 0);
        let artifact_group = ArtifactGroup::Artifact(output.into());

        // Empty state - the action is not registered
        let state = fxhash::FxHashMap::default();

        let mut mock_sketcher: MockSketcher<ActionKey> = MockSketcher::new();
        let complete =
            super::compute_action_graph_sketch_impl([&artifact_group], &state, &mut mock_sketcher)
                .unwrap();

        // Should be incomplete because we couldn't find the action
        assert!(!complete);
    }

    #[test]
    fn test_action_graph_sketch_impl_diamond() {
        // Test diamond dependency: action0 -> action1, action2 -> action3
        //                                  \                    /
        //                                   -----> action3 <----
        let target = create_target("root//:lib");
        let holder_key = DeferredHolderKey::for_analysis(target.dupe());

        // Base action
        let (action_key0, output0, action0) = create_action(&holder_key, 0, vec![]);
        let artifact0 = ArtifactGroup::Artifact(output0.into());

        // Two actions that both depend on action0
        let (action_key1, output1, action1) = create_action(&holder_key, 1, vec![&artifact0]);
        let artifact1 = ArtifactGroup::Artifact(output1.into());

        let (action_key2, output2, action2) = create_action(&holder_key, 2, vec![&artifact0]);
        let artifact2 = ArtifactGroup::Artifact(output2.into());

        // Final action that depends on both action1 and action2
        let (action_key3, output3, action3) =
            create_action(&holder_key, 3, vec![&artifact1, &artifact2]);
        let artifact3 = ArtifactGroup::Artifact(output3.into());

        // Build state with all actions
        let mut recorded_actions = RecordedActions::new(4);
        recorded_actions.insert(action_key0.dupe(), action0);
        recorded_actions.insert(action_key1.dupe(), action1);
        recorded_actions.insert(action_key2.dupe(), action2);
        recorded_actions.insert(action_key3.dupe(), action3);

        let analysis_values =
            RecordedAnalysisValues::testing_new_actions_only(holder_key.dupe(), recorded_actions);
        let holder = DeferredHolder::Analysis(AnalysisResult::new(
            analysis_values,
            None,
            HashMap::new(),
            0,
            0,
            None,
        ));

        let mut state = fxhash::FxHashMap::default();
        state.insert(holder_key, holder);

        let mut mock_sketcher: MockSketcher<ActionKey> = MockSketcher::new();
        let complete =
            super::compute_action_graph_sketch_impl([&artifact3], &state, &mut mock_sketcher)
                .unwrap();

        assert!(complete);
        // Diamond has 4 unique actions - action0 should only be sketched once
        assert_eq!(mock_sketcher.items.len(), 4);
        let sketched: FxHashSet<_> = mock_sketcher.items.into_iter().collect();
        assert!(sketched.contains(&action_key0));
        assert!(sketched.contains(&action_key1));
        assert!(sketched.contains(&action_key2));
        assert!(sketched.contains(&action_key3));
    }
}
