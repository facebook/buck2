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
use buck2_core::fs::artifact_path_resolver::ArtifactFs;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_core::package::PackageLabel;
use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
use buck2_directory::directory::directory::Directory;
use buck2_directory::directory::directory_iterator::DirectoryIterator;
use buck2_directory::directory::directory_iterator::DirectoryIteratorPathStack;
use buck2_directory::directory::entry::DirectoryEntry;
use buck2_directory::directory::walk::unordered_entry_walk;
use buck2_execute::artifact::artifact_dyn::ArtifactDyn;
use buck2_execute::artifact_value::ArtifactValue;
use buck2_execute::directory::ActionDirectoryMember;
use buck2_hash::BuckHashSet;
use buck2_interpreter::dice::starlark_provider::StarlarkEvalKind;
use buck2_interpreter::load_module::InterpreterCalculation;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_node::nodes::configured_frontend::ConfiguredTargetNodeCalculation;
use buck2_node::nodes::frontend::TargetGraphCalculation;
use buck2_sketches::ActionGraphSketch;
use buck2_sketches::ArtifactCountSketch;
use buck2_sketches::ArtifactSizeSketch;
use buck2_sketches::DependencyGraphSketch;
use buck2_sketches::MemoryUsageSketch;
use dice::CancellationContext;
use dice::DiceComputations;
use dice::DiceKeyDyn;
use dice::Key;
use dice::OkPagableValueSerialize;
use dice::ValueSerialize;
use dupe::Dupe;
use futures::FutureExt;
use pagable::Pagable;
use pagable::pagable_typetag;
use starlark::values::FrozenHeapName;
use starlark::values::FrozenHeapRef;

use crate::analysis::calculation::RuleAnalysisCalculation;
use crate::artifact_groups::ArtifactGroup;
use crate::artifact_groups::calculation::ArtifactGroupCalculation;
use crate::build::BuildProviderType;
use crate::build::graph_properties::ConfiguredGraphPropertiesValues;
use crate::build::sketch_impl::DEFAULT_SKETCH_VERSION;
use crate::build::sketch_impl::MergeableGraphSketch;
use crate::build::sketch_impl::Sketcher;
use crate::build::sketch_impl::VersionedSketcher;
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
    state: &buck2_hash::BuckHashMap<DeferredHolderKey, DeferredHolder>,
) -> buck2_error::Result<(bool, MergeableGraphSketch<ActionKey, ActionGraphSketch>)> {
    let mut sketcher = DEFAULT_SKETCH_VERSION.create_sketcher();
    let complete = compute_action_graph_sketch_impl(root_artifacts, state, &mut sketcher)?;
    Ok((complete, sketcher.into_mergeable_graph_sketch()))
}

/// Private implementation that accepts any Sketcher for testing.
fn compute_action_graph_sketch_impl<'a>(
    root_artifacts: impl IntoIterator<Item = &'a ArtifactGroup>,
    state: &buck2_hash::BuckHashMap<DeferredHolderKey, DeferredHolder>,
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
    let mut sketcher: Option<VersionedSketcher<ConfiguredTargetLabel, DependencyGraphSketch>> =
        if compute_sketch {
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
    let mut visited: BuckHashSet<_> = BuckHashSet::default();
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

type AnalysisHeapSketches = (
    Option<MergeableGraphSketch<StarlarkEvalKind, MemoryUsageSketch>>,
    Option<MergeableGraphSketch<StarlarkEvalKind, MemoryUsageSketch>>,
);

/// Holds optional size and count sketches for artifact paths.
pub(crate) struct ArtifactPathSketches {
    pub(crate) size: Option<MergeableGraphSketch<ProjectRelativePathBuf, ArtifactSizeSketch>>,
    pub(crate) count: Option<MergeableGraphSketch<ProjectRelativePathBuf, ArtifactCountSketch>>,
}

impl ArtifactPathSketches {
    pub(crate) fn empty() -> Self {
        Self {
            size: None,
            count: None,
        }
    }
}

/// Computes artifact path sketches from an iterator of (path, size) pairs.
///
/// For each file, sketches the path string weighted by its size (for size sketch)
/// or weight 1 (for count sketch). Callers should walk artifact directory entries
/// at file granularity (e.g. via `unordered_entry_walk`) so that projected
/// artifacts and directory contents are sketched uniformly.
pub(crate) fn compute_artifact_path_sketches(
    artifacts: impl Iterator<Item = (ProjectRelativePathBuf, u64)>,
    sketch_size: bool,
    sketch_count: bool,
) -> ArtifactPathSketches {
    let mut size_sketcher = if sketch_size {
        Some(DEFAULT_SKETCH_VERSION.create_sketcher::<ProjectRelativePathBuf, ArtifactSizeSketch>())
    } else {
        None
    };
    let mut count_sketcher = if sketch_count {
        Some(
            DEFAULT_SKETCH_VERSION.create_sketcher::<ProjectRelativePathBuf, ArtifactCountSketch>(),
        )
    } else {
        None
    };

    compute_artifact_path_sketches_impl(artifacts, size_sketcher.as_mut(), count_sketcher.as_mut());

    ArtifactPathSketches {
        size: size_sketcher.map(|s| s.into_mergeable_graph_sketch()),
        count: count_sketcher.map(|s| s.into_mergeable_graph_sketch()),
    }
}

/// Private implementation that accepts any Sketchers for testing.
fn compute_artifact_path_sketches_impl<S, C>(
    artifacts: impl Iterator<Item = (ProjectRelativePathBuf, u64)>,
    mut size_sketcher: Option<&mut S>,
    mut count_sketcher: Option<&mut C>,
) where
    S: Sketcher<ProjectRelativePathBuf>,
    C: Sketcher<ProjectRelativePathBuf>,
{
    for (path, size) in artifacts {
        if let Some(s) = size_sketcher.as_deref_mut() {
            s.sketch_weighted(&path, size);
        }
        if let Some(c) = count_sketcher.as_deref_mut() {
            c.sketch(&path);
        }
    }
}

/// Computes artifact path sketches for one top-level target's outputs.
pub(crate) async fn compute_artifact_path_sketches_for_target(
    ctx: &mut DiceComputations<'_>,
    outputs: &[(ArtifactGroup, BuildProviderType)],
    artifact_fs: &ArtifactFs,
    providers_to_skip: &HashSet<BuildProviderType>,
    sketch_size: bool,
    sketch_count: bool,
) -> buck2_error::Result<ArtifactPathSketches> {
    let values =
        collect_immediate_artifact_values(ctx, outputs, artifact_fs, providers_to_skip).await?;

    let mut weighted_paths: Vec<(ProjectRelativePathBuf, u64)> = Vec::new();
    for (artifact_path, value) in &values {
        let mut walk = unordered_entry_walk(value.entry().as_ref().map_dir(Directory::as_ref));
        while let Some((rel_path, entry)) = walk.next() {
            if let DirectoryEntry::Leaf(leaf) = entry {
                weighted_paths.push((artifact_path.join(rel_path.get()), leaf_size(leaf)));
            }
        }

        if let Some(deps) = value.deps() {
            let deps_entry = DirectoryEntry::Dir(deps.dupe());
            let mut walk = unordered_entry_walk(deps_entry.as_ref().map_dir(Directory::as_ref));
            while let Some((rel_path, entry)) = walk.next() {
                if let DirectoryEntry::Leaf(leaf) = entry {
                    weighted_paths.push((
                        ProjectRelativePathBuf::from(rel_path.get()),
                        leaf_size(leaf),
                    ));
                }
            }
        }
    }

    Ok(compute_artifact_path_sketches(
        weighted_paths.into_iter(),
        sketch_size,
        sketch_count,
    ))
}

fn leaf_size(leaf: &ActionDirectoryMember) -> u64 {
    match leaf {
        ActionDirectoryMember::File(f) => f.digest.size(),
        ActionDirectoryMember::Symlink(s) => s.target().as_str().len() as u64,
        ActionDirectoryMember::ExternalSymlink(s) => s.target().as_os_str().len() as u64,
    }
}

/// Resolves the immediate output artifacts of final providers via DICE.
///
/// Filters `outputs` by `providers_to_skip`, then resolves each remaining
/// `ArtifactGroup` via `ensure_artifact_group()`. Returns `(path, value)`
/// pairs where `path` is the project-relative on-disk path of the resolved
/// artifact (content-hashed for content-based artifacts, configuration-hashed
/// otherwise) and `value` is its `ArtifactValue`. Callers walk the value's
/// `entry()` and `deps()` to obtain per-file paths and sizes; symlink chains
/// are pre-merged into `deps` at action-construction time, so transitive
/// symlink targets are surfaced for free.
pub(crate) async fn collect_immediate_artifact_values(
    ctx: &mut DiceComputations<'_>,
    outputs: &[(ArtifactGroup, BuildProviderType)],
    artifact_fs: &ArtifactFs,
    providers_to_skip: &HashSet<BuildProviderType>,
) -> buck2_error::Result<Vec<(ProjectRelativePathBuf, ArtifactValue)>> {
    let filtered: Vec<&ArtifactGroup> = outputs
        .iter()
        .filter(|(_, pt)| !providers_to_skip.contains(pt))
        .map(|(artifact, _)| artifact)
        .collect();

    let all_values = ctx
        .try_compute_join(filtered.iter(), |ctx, artifact_group| {
            async move { ctx.ensure_artifact_group(artifact_group).await }.boxed()
        })
        .await?;

    all_values
        .iter()
        .flat_map(|values| values.iter())
        .map(|(artifact, value)| {
            artifact
                .resolve_path(
                    artifact_fs,
                    if artifact.path_resolution_requires_artifact_value() {
                        Some(value.content_based_path_hash())
                    } else {
                        None
                    }
                    .as_ref(),
                )
                .map(|path| (path, value.dupe()))
        })
        .collect()
}

#[derive(
    Clone,
    Dupe,
    derive_more::Display,
    Debug,
    Eq,
    Hash,
    PartialEq,
    Allocative,
    Pagable
)]
#[display(
    "AnalysisGraphPropertiesKey({}, retained={}, peak={})",
    label,
    compute_retained,
    compute_peak
)]
#[pagable_typetag(DiceKeyDyn)]
pub(crate) struct AnalysisGraphPropertiesKey {
    pub label: ConfiguredTargetLabel,
    pub compute_retained: bool,
    pub compute_peak: bool,
}

#[async_trait]
impl Key for AnalysisGraphPropertiesKey {
    type Value = buck2_error::Result<MaybeCompatible<AnalysisHeapSketches>>;

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
                self.compute_retained,
                self.compute_peak,
            ))
        })
    }

    fn equality(a: &Self::Value, b: &Self::Value) -> bool {
        match (a, b) {
            (Ok(a), Ok(b)) => a == b,
            _ => false,
        }
    }

    fn value_serialize() -> impl ValueSerialize<Value = Self::Value> {
        OkPagableValueSerialize::<Self::Value>::new()
    }
}

#[derive(
    Clone,
    Dupe,
    derive_more::Display,
    Debug,
    Eq,
    Hash,
    PartialEq,
    Allocative,
    Pagable
)]
#[display("LoadGraphPropertiesKey({})", label)]
#[pagable_typetag(DiceKeyDyn)]
pub(crate) struct LoadGraphPropertiesKey {
    pub label: ConfiguredTargetLabel,
}

fn collect_transitive_packages(root: &ConfiguredTargetNode) -> BuckHashSet<PackageLabel> {
    let mut packages = BuckHashSet::default();
    let mut visited = BuckHashSet::default();
    visited.insert(root);
    let mut queue = vec![root];
    while let Some(node) = queue.pop() {
        packages.insert(node.label().pkg());
        for dep in node.deps() {
            if visited.insert(dep) {
                queue.push(dep);
            }
        }
    }
    packages
}

#[async_trait]
impl Key for LoadGraphPropertiesKey {
    type Value = buck2_error::Result<
        MaybeCompatible<MergeableGraphSketch<StarlarkEvalKind, MemoryUsageSketch>>,
    >;

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellation: &CancellationContext,
    ) -> Self::Value {
        let configured_node = ctx
            .get_configured_target_node(&self.label)
            .await
            .require_compatible()?;

        let packages = collect_transitive_packages(&configured_node);
        let mut sketcher = DEFAULT_SKETCH_VERSION.create_sketcher();

        let pkg_results = ctx
            .try_compute_join(packages.iter(), |ctx, pkg| {
                async move {
                    ctx.get_interpreter_results(pkg.dupe())
                        .await
                        .map(|r| (pkg.dupe(), r))
                }
                .boxed()
            })
            .await?;

        let mut imports = BuckHashSet::default();
        for (pkg, eval_result) in &pkg_results {
            let peak_bytes = eval_result.starlark_peak_allocated_bytes;
            if peak_bytes > 0 {
                sketcher.sketch_weighted(&StarlarkEvalKind::LoadBuildFile(pkg.dupe()), peak_bytes);
            }

            imports.extend(eval_result.imports());
        }

        let loaded_modules = ctx
            .try_compute_join(imports.iter(), |ctx, import| {
                async move {
                    ctx.get_loaded_module_from_import_path(import)
                        .await
                        .map(|m| m.env().frozen_heap().dupe())
                }
                .boxed()
            })
            .await?;

        let mut visited: BuckHashSet<&FrozenHeapRef> = BuckHashSet::default();
        let mut queue: Vec<&FrozenHeapRef> = Vec::new();
        for heap in &loaded_modules {
            if visited.insert(heap) {
                queue.push(heap);
            }
        }
        while let Some(item) = queue.pop() {
            if let Some(FrozenHeapName::User(user_name)) = item.name() {
                if let Some(eval_kind) = user_name.as_any().downcast_ref::<StarlarkEvalKind>() {
                    if let Some(peak_bytes) = item.peak_allocated_bytes() {
                        sketcher.sketch_weighted(eval_kind, peak_bytes as u64);
                    }
                }
            }
            queue.extend(item.refs().filter(|f| visited.insert(f)));
        }

        Ok(MaybeCompatible::Compatible(
            sketcher.into_mergeable_graph_sketch(),
        ))
    }

    fn equality(a: &Self::Value, b: &Self::Value) -> bool {
        match (a, b) {
            (Ok(a), Ok(b)) => a == b,
            _ => false,
        }
    }

    fn value_serialize() -> impl ValueSerialize<Value = Self::Value> {
        OkPagableValueSerialize::<Self::Value>::new()
    }
}

/// Computes a sketch of the memory transitively referenced by the given starlark heap.
///
/// Using the heap graph instead of the configured graph directly is advantageous primarily because
/// analysis results don't otherwise directly encode the structure of the analysis graph. "Guessing"
/// at what the graph is, while probably possible in practice, is a bit brittle. It would also mean
/// that we wouldn't know about anon targets, which would be a bit of a shame.
fn gather_heap_graph_sketch(
    root: &FrozenHeapRef,
    compute_retained: bool,
    compute_peak: bool,
) -> AnalysisHeapSketches {
    let mut retained_sketcher = compute_retained.then(|| DEFAULT_SKETCH_VERSION.create_sketcher());
    let mut peak_sketcher = compute_peak.then(|| DEFAULT_SKETCH_VERSION.create_sketcher());

    let mut visited = BuckHashSet::default();
    visited.insert(root);
    let mut queue = vec![root];

    while let Some(item) = queue.pop() {
        let Some(name) = item.name() else {
            continue;
        };
        if let FrozenHeapName::User(user_name) = name {
            if let Some(eval_kind) = user_name.as_any().downcast_ref::<StarlarkEvalKind>() {
                if let Some(s) = &mut retained_sketcher {
                    s.sketch_weighted(eval_kind, item.allocated_bytes() as u64);
                }
                if let Some(s) = &mut peak_sketcher {
                    if let Some(peak) = item.peak_allocated_bytes() {
                        s.sketch_weighted(eval_kind, peak as u64);
                    }
                }
            }
        }
        queue.extend(item.refs().filter(|f| visited.insert(*f)));
    }

    (
        retained_sketcher.map(|s| s.into_mergeable_graph_sketch()),
        peak_sketcher.map(|s| s.into_mergeable_graph_sketch()),
    )
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
    use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
    use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
    use buck2_fs::paths::forward_rel_path::ForwardRelativePathBuf;
    use buck2_hash::BuckHashMap;
    use buck2_hash::BuckHashSet;
    use buck2_hash::BuckIndexSet;
    use dupe::Dupe;
    use pagable::Pagable;
    use pagable::pagable_typetag;

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
    use crate::build::detailed_aggregated_metrics::buck2_sketches::compute_artifact_path_sketches_impl;
    use crate::build::sketch_impl::Sketcher;
    use crate::deferred::calculation::DeferredHolder;

    /// A mock sketcher that records all sketch calls for verification in tests.
    struct MockSketcher<T: Clone> {
        items: Vec<T>,
        weighted_items: Vec<(T, u64)>,
    }

    impl<T: Clone> MockSketcher<T> {
        fn new() -> Self {
            Self {
                items: Vec::new(),
                weighted_items: Vec::new(),
            }
        }
    }

    impl<T: Clone> Sketcher<T> for MockSketcher<T> {
        fn sketch(&mut self, t: &T) {
            self.items.push(t.clone());
        }

        fn sketch_weighted(&mut self, t: &T, weight: u64) {
            self.weighted_items.push((t.clone(), weight));
        }
    }

    /// A minimal mock action for testing action graph traversal.
    #[derive(Debug, Allocative, Pagable)]
    struct MockAction {
        inputs: BoxSliceSet<ArtifactGroup>,
        outputs: BoxSliceSet<BuildArtifact>,
        category: Category,
        identifier: Option<String>,
    }

    impl MockAction {
        fn new(
            inputs: BuckIndexSet<ArtifactGroup>,
            outputs: BuckIndexSet<BuildArtifact>,
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

    #[pagable_typetag]
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
                BuckIndexSet::from([output.dupe()]),
                Some(format!("action-{index}")),
            )),
            CommandExecutorConfig::testing_local(),
        ));
        (action_key, output, action)
    }

    #[test]
    fn test_action_graph_sketch_impl_empty() {
        let state = BuckHashMap::default();
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
        let state = BuckHashMap::default();

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

        let mut state = BuckHashMap::default();
        state.insert(holder_key, holder);

        let mut mock_sketcher: MockSketcher<ActionKey> = MockSketcher::new();
        let complete =
            super::compute_action_graph_sketch_impl([&artifact3], &state, &mut mock_sketcher)
                .unwrap();

        assert!(complete);
        // Diamond has 4 unique actions - action0 should only be sketched once
        assert_eq!(mock_sketcher.items.len(), 4);
        let sketched: BuckHashSet<_> = mock_sketcher.items.into_iter().collect();
        assert!(sketched.contains(&action_key0));
        assert!(sketched.contains(&action_key1));
        assert!(sketched.contains(&action_key2));
        assert!(sketched.contains(&action_key3));
    }

    #[test]
    fn test_artifact_path_sketches_does_not_dedup_paths() {
        // Pins current behavior: the sketcher receives the same path multiple
        // times when the input contains duplicates. This corresponds to the
        // per-target / cross-target duplicate-traversal perf bug in
        // `compute_artifact_path_sketches_for_target`.
        let p_shared = ProjectRelativePathBuf::unchecked_new("buck-out/shared.txt".to_owned());
        let p_other = ProjectRelativePathBuf::unchecked_new("buck-out/other.txt".to_owned());

        let input = vec![
            (p_shared.clone(), 10),
            (p_other.clone(), 20),
            (p_shared.clone(), 30),
        ];

        let mut size_mock: MockSketcher<ProjectRelativePathBuf> = MockSketcher::new();
        let mut count_mock: MockSketcher<ProjectRelativePathBuf> = MockSketcher::new();

        compute_artifact_path_sketches_impl(
            input.into_iter(),
            Some(&mut size_mock),
            Some(&mut count_mock),
        );

        assert_eq!(
            count_mock.items,
            vec![p_shared.clone(), p_other.clone(), p_shared.clone()],
        );
        assert_eq!(
            size_mock.weighted_items,
            vec![
                (p_shared.clone(), 10),
                (p_other.clone(), 20),
                (p_shared.clone(), 30),
            ],
        );
    }
}
