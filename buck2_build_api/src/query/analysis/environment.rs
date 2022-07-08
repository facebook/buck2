/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Cow;
use std::collections::HashSet;
use std::collections::VecDeque;
use std::marker::PhantomData;

use anyhow::Context;
use async_trait::async_trait;
use buck2_common::result::SharedResult;
use buck2_core::build_file_path::BuildFilePath;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::target::ConfiguredTargetLabel;
use buck2_node::attrs::attr_type::attr_config::AttrConfig;
use buck2_node::attrs::configured_attr::ConfiguredAttr;
use buck2_node::attrs::inspect_options::AttrInspectOptions;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_query::query::environment::LabeledNode;
use buck2_query::query::environment::NodeLabel;
use buck2_query::query::environment::QueryEnvironment;
use buck2_query::query::environment::QueryTarget;
use buck2_query::query::syntax::simple::eval::error::QueryError;
use buck2_query::query::syntax::simple::eval::file_set::FileSet;
use buck2_query::query::syntax::simple::eval::set::TargetSet;
use buck2_query::query::syntax::simple::eval::values::QueryValue;
use buck2_query::query::syntax::simple::functions::helpers::QueryBinaryOp;
use buck2_query::query::syntax::simple::functions::helpers::QueryFunction;
use buck2_query::query::syntax::simple::functions::DefaultQueryFunctionsModule;
use buck2_query::query::syntax::simple::functions::QueryFunctions;
use buck2_query::query::traversal::async_depth_limited_traversal;
use buck2_query::query::traversal::async_fast_depth_first_postorder_traversal;
use buck2_query::query::traversal::async_unordered_traversal;
use buck2_query::query::traversal::AsyncNodeLookup;
use buck2_query::query::traversal::AsyncTraversalDelegate;
use buck2_query::query::traversal::ChildVisitor;
use buck2_query::query::traversal::NodeLookup;
use buck2_query::query_module;
use buck2_query_parser::BinaryOp;
use futures::Future;
use gazebo::prelude::*;
use indexmap::IndexSet;
use owning_ref::ArcRef;
use ref_cast::RefCast;
use thiserror::Error;

use crate::actions::artifact::Artifact;
use crate::actions::artifact::OutputArtifact;
use crate::artifact_groups::deferred::DeferredTransitiveSetData;
use crate::artifact_groups::deferred::TransitiveSetKey;
use crate::artifact_groups::ArtifactGroup;
use crate::deferred::AnyValue;
use crate::interpreter::rule_defs::artifact_tagging::ArtifactTag;
use crate::interpreter::rule_defs::cmd_args::CommandLineArtifactVisitor;
use crate::interpreter::rule_defs::cmd_args::ValueAsCommandLineLike;
use crate::interpreter::rule_defs::transitive_set::TransitiveSet;

#[derive(Debug, Error)]
enum AnalysisQueryError {
    #[error("file literals aren't supported in query attributes (got `{0}`)")]
    FileLiteralsNotAllowed(String),
    #[error(
        "template_placeholder_info `{0}` of target `{1}` used in query attributes had artifact (`{2}`) not produced by a target, only target-produced artifacts supported here"
    )]
    NonTargetBoundArtifact(String, ConfiguredTargetLabel, Artifact),
}

#[async_trait]
pub trait ConfiguredGraphQueryEnvironmentDelegate: Send + Sync {
    fn eval_literal(&self, literal: &str) -> anyhow::Result<ConfiguredTargetNode>;

    async fn get_template_info_provider_artifacts(
        &self,
        configured_label: &ConfiguredTargetLabel,
        template_name: &str,
    ) -> anyhow::Result<Vec<ArtifactGroup>>;

    /// Looks up a transitive set.
    ///
    /// WE CANNOT USE THIS TO LOOKUP ALL NODES IN A TSET GRAPH!!! Doing so can lead to
    /// massive memory regressions.
    ///
    /// Using this will add dice edges for any fetched keys. It's very important
    /// that we do not flatten a full tset into the dice deps of our analysis nodes.
    ///
    /// Really, we shouldn't need this at all. We have references to the starlark
    /// values and we should be able to traverse them directly, but the cmdlinearg
    /// apis don't allow us to do that (they convert values into ArtifactGroup,
    /// which holds only the symbolic reference to the tset, for example).
    async fn dice_lookup_transitive_set(
        &self,
        key: TransitiveSetKey,
    ) -> SharedResult<ArcRef<dyn AnyValue, DeferredTransitiveSetData>>;
}

pub struct ConfiguredGraphQueryEnvironment<'a> {
    delegate: &'a dyn ConfiguredGraphQueryEnvironmentDelegate,
}

struct ConfiguredGraphFunctions<'a>(PhantomData<&'a ()>);
#[query_module(ConfiguredGraphQueryEnvironment<'a>)]
impl<'a> ConfiguredGraphFunctions<'a> {
    async fn classpath(
        &self,
        env: &ConfiguredGraphQueryEnvironment<'a>,
        targets: TargetSet<ConfiguredGraphNodeRef>,
        depth: Option<u64>,
    ) -> Result<QueryValue<ConfiguredGraphNodeRef>, QueryError> {
        // if depth param is provided and it is not equal to 1, then it's not supported
        let mut run_first_order_classpath = false;
        if let Some(depth_int) = depth.map(|v| v as i32) {
            run_first_order_classpath = depth_int == 1;
            if !run_first_order_classpath {
                return Err(QueryError::InvalidDepth(depth_int));
            }
        }

        let template_name = if run_first_order_classpath {
            "first_order_classpath"
        } else {
            "classpath_including_targets_with_no_output"
        };

        Ok(env
            .get_from_template_placeholder_info(template_name, &targets)
            .await?
            .into())
    }
}

impl<'a> ConfiguredGraphQueryEnvironment<'a> {
    pub fn new(delegate: &'a dyn ConfiguredGraphQueryEnvironmentDelegate) -> Self {
        Self { delegate }
    }

    pub fn functions() -> impl QueryFunctions<ConfiguredGraphQueryEnvironment<'a>> {
        struct Functions<'a> {
            defaults: DefaultQueryFunctionsModule<ConfiguredGraphQueryEnvironment<'a>>,
            extra_functions: ConfiguredGraphFunctions<'a>,
        }

        impl<'a> QueryFunctions<ConfiguredGraphQueryEnvironment<'a>> for Functions<'a> {
            fn get(
                &self,
                name: &str,
            ) -> Option<&dyn QueryFunction<ConfiguredGraphQueryEnvironment<'a>>> {
                if let Some(v) = self.extra_functions.get(name) {
                    Some(v)
                } else {
                    self.defaults.get(name)
                }
            }

            fn get_op(
                &self,
                op: BinaryOp,
            ) -> Option<&dyn QueryBinaryOp<ConfiguredGraphQueryEnvironment<'a>>> {
                if let Some(v) = self.extra_functions.get_op(op) {
                    Some(v)
                } else {
                    self.defaults.get_op(op)
                }
            }
        }

        Functions {
            defaults: DefaultQueryFunctionsModule::new(),
            extra_functions: ConfiguredGraphFunctions(PhantomData),
        }
    }

    /// For each input target goes into its exposed list of providers, finds TemplatePlaceholderInfo among them
    /// and accesses its internal `keyed_variables` map with the passed `template_name` key.
    /// Then converts retrieved value form cmd args into targets and returns them back as result.
    fn get_from_template_placeholder_info<'x>(
        &'x self,
        template_name: &'static str,
        targets: &'x TargetSet<ConfiguredGraphNodeRef>,
    ) -> impl Future<Output = anyhow::Result<TargetSet<ConfiguredGraphNodeRef>>> + 'x {
        async move {
            let mut labels: IndexSet<ConfiguredTargetLabel> = IndexSet::new();

            // Traversing tsets adds complexity here. Ideally, we could just do a normal traversal of these starlark values
            // we get from the template_info provider, but the cmdlinearglike interface only gives us access via ArtifactGroup
            // which only have the key for the tset projection, not the tset or projection itself.
            //
            // Then, next we would want to do a traversal over those ArtifactGroup and use the dice ctx to map the tset projection
            // keys back to their values. We can't do that because that would flatten the tset into the dice deps and rdeps storage.
            //
            // So, instead we need to extract out the ArtifactGroups from the template_info values and lookup the corresponding
            // tsets, manually traverse them and repeat (because the tset node values themselves return ArtifactGroup).
            //
            // This means that we will have unnessary dice nodes pointing to each tset value appearing in the template info and any
            // tset value appearing in another tset's nodes. In the common case, though, that set will be small and we won't have
            // flattened any full tset into the dice deps storage. We'll call those "top-level" tset nodes.

            // This will contain the ArtifactGroups we encounter during our traversal (so only artifacts and top-level tset nodes).
            // Artifacts are put here to keep them in the correct order in the output, tsets are top-level tset nodes that we need
            // to traverse.
            let mut artifacts = VecDeque::new();

            // This will contain the TransitiveSetProjectionKey we encounter as top-level nodes and we will also put in TransitiveSetProjectionKey
            // for all the tset nodes that we encounter during our traversal of those top-level nodes. We don't need to track artifacts because
            // we just extract the targetlabel and put that in the output set and that can dedupe them (and we don't need to further
            // traverse artifacts).
            let mut seen = HashSet::new();

            for target in targets.iter() {
                for artifact in self
                    .delegate
                    .get_template_info_provider_artifacts(target.label(), template_name)
                    .await?
                {
                    artifacts.push_back((target.label().dupe(), artifact))
                }
            }

            while let Some((target, artifact)) = artifacts.pop_front() {
                match artifact {
                    ArtifactGroup::Artifact(artifact) => {
                        if let Some(owner) = artifact.owner() {
                            let target_label = owner.unpack_target_label().ok_or_else(|| {
                                AnalysisQueryError::NonTargetBoundArtifact(
                                    template_name.to_owned(),
                                    target.dupe(),
                                    artifact.dupe(),
                                )
                            })?;
                            labels.insert(target_label.dupe());
                        }
                    }
                    ArtifactGroup::TransitiveSetProjection(tset_key) => {
                        // We've encountered a "top-level" tset node that we haven't yet seen (as either a top-level or intermediate node, doesn't matter).
                        if seen.insert(tset_key.dupe()) {
                            let tset_value = self
                                .delegate
                                .dice_lookup_transitive_set(tset_key.key)
                                .await?;

                            // Now we can traverse this tset from that node. This is a different traversal than our top-level one as we will
                            // be accessing tset internals directly and so we can actually traverse the starlark objects without going back through
                            // dice. We'll be working all with values with lifetimes from `tset_value`.
                            //
                            // We can't use tset's normal traverse because we need to avoid retraversing parts of the tset graph that we've already
                            // traversed (through other top-level tset nodes).
                            let mut queue = VecDeque::new();
                            queue.push_back(tset_value.as_value());
                            while let Some(v) = queue.pop_front() {
                                let as_tset = TransitiveSet::from_value(v)
                                    .context("invalid tset structure")?;

                                // Visit the projection value itself. As this is an opaque cmdargs-like thing, it may contain more top-level tset node
                                // references that need to be pushed into the outer queue.
                                if let Some(v) =
                                    as_tset.get_projection_value(tset_key.projection)?
                                {
                                    struct Visitor<'a>(
                                        &'a mut VecDeque<(ConfiguredTargetLabel, ArtifactGroup)>,
                                        ConfiguredTargetLabel,
                                    );
                                    impl<'a> CommandLineArtifactVisitor for Visitor<'a> {
                                        fn visit_input(
                                            &mut self,
                                            input: ArtifactGroup,
                                            _tag: Option<&ArtifactTag>,
                                        ) {
                                            self.0.push_back((self.1.dupe(), input));
                                        }

                                        fn visit_output(
                                            &mut self,
                                            _artifact: OutputArtifact,
                                            _tag: Option<&ArtifactTag>,
                                        ) {
                                            // ignored
                                        }
                                    }
                                    v.as_command_line_err()?.visit_artifacts(&mut Visitor(
                                        &mut artifacts,
                                        target.dupe(),
                                    ))?;
                                }

                                // Enqueue any children we haven't yet seen (and mark them seen).
                                for child in as_tset.children.iter() {
                                    let child_as_tset = TransitiveSet::from_value(*child)
                                        .context("Invalid deferred")?;
                                    let projection_key =
                                        child_as_tset.get_projection_key(tset_key.projection);
                                    if seen.insert(projection_key) {
                                        queue.push_back(*child);
                                    }
                                }
                            }
                        }
                    }
                }
            }

            self.find_target_nodes(targets, labels).await
        }
    }

    /// Finds the nodes for a list of target labels within the deps of the provided targets.
    ///
    /// It may seem like if we have ConfiguredTargetLabel we should just be able to lookup the
    /// nodes directly, but that would require going through dice and then dice would record
    /// dependencies on all the nodes that we lookup. It's common for these queries to operate
    /// over inputs that are aggregated as data flows up the graph and it's important that we
    /// don't inadvertently cause flattening of those sets.
    async fn find_target_nodes(
        &self,
        targets: &TargetSet<ConfiguredGraphNodeRef>,
        labels_to_find: IndexSet<ConfiguredTargetLabel>,
    ) -> anyhow::Result<TargetSet<ConfiguredGraphNodeRef>> {
        struct TraversalDelegate {
            targets: TargetSet<ConfiguredGraphNodeRef>,
            labels_to_find: IndexSet<ConfiguredTargetLabel>,
        }

        #[async_trait]
        impl AsyncTraversalDelegate<ConfiguredGraphNodeRef> for TraversalDelegate {
            fn visit(&mut self, target: ConfiguredGraphNodeRef) -> anyhow::Result<()> {
                let label = target.label();
                if self.labels_to_find.contains(label) {
                    self.targets.insert(target);
                }

                Ok(())
            }

            async fn for_each_child(
                &mut self,
                target: &ConfiguredGraphNodeRef,
                func: &mut dyn ChildVisitor<ConfiguredGraphNodeRef>,
            ) -> anyhow::Result<()> {
                // if all found then skip traversing further
                if self.targets.len() == self.labels_to_find.len() {
                    return Ok(());
                }

                for dep in target.deps() {
                    func.visit(LabeledNode::node_ref(dep).dupe())?;
                }

                Ok(())
            }
        }

        let mut delegate = TraversalDelegate {
            targets: TargetSet::new(),
            labels_to_find,
        };
        async_unordered_traversal(self, targets.iter(), &mut delegate).await?;

        Ok(delegate.targets)
    }
}

#[async_trait]
impl<'a> QueryEnvironment for ConfiguredGraphQueryEnvironment<'a> {
    type Target = ConfiguredGraphNodeRef;

    async fn get_node(&self, node_ref: &ConfiguredGraphNodeRef) -> anyhow::Result<Self::Target> {
        Ok(node_ref.dupe())
    }

    async fn eval_literals(&self, literal: &[&str]) -> anyhow::Result<TargetSet<Self::Target>> {
        let mut result = TargetSet::new();
        for lit in literal {
            result.insert(ConfiguredGraphNodeRef(self.delegate.eval_literal(lit)?));
        }
        Ok(result)
    }

    async fn eval_file_literal(&self, literal: &str) -> anyhow::Result<FileSet> {
        Err(AnalysisQueryError::FileLiteralsNotAllowed(literal.to_owned()).into())
    }

    async fn dfs_postorder(
        &self,
        root: &TargetSet<Self::Target>,
        delegate: &mut dyn AsyncTraversalDelegate<Self::Target>,
    ) -> anyhow::Result<()> {
        async_fast_depth_first_postorder_traversal(
            self,
            root.iter().map(LabeledNode::node_ref),
            delegate,
        )
        .await
    }

    async fn depth_limited_traversal(
        &self,
        root: &TargetSet<Self::Target>,
        delegate: &mut dyn AsyncTraversalDelegate<Self::Target>,
        depth: u32,
    ) -> anyhow::Result<()> {
        async_depth_limited_traversal(self, root.iter(), delegate, depth).await
    }

    async fn owner(&self, _paths: &FileSet) -> anyhow::Result<TargetSet<Self::Target>> {
        Err(QueryError::FunctionUnimplemented("owner").into())
    }
}

#[async_trait]
impl<'a> NodeLookup<ConfiguredGraphNodeRef> for ConfiguredGraphQueryEnvironment<'a> {
    fn get(&self, label: &ConfiguredGraphNodeRef) -> anyhow::Result<ConfiguredGraphNodeRef> {
        Ok(label.dupe())
    }
}

#[async_trait]
impl<'a> AsyncNodeLookup<ConfiguredGraphNodeRef> for ConfiguredGraphQueryEnvironment<'a> {
    async fn get(&self, label: &ConfiguredGraphNodeRef) -> anyhow::Result<ConfiguredGraphNodeRef> {
        Ok(label.dupe())
    }
}

#[derive(Debug, Dupe, Clone, RefCast)]
#[repr(C)]
pub struct ConfiguredGraphNodeRef(pub ConfiguredTargetNode);

impl ConfiguredGraphNodeRef {
    pub fn label(&self) -> &ConfiguredTargetLabel {
        self.0.name()
    }
}

impl std::fmt::Display for ConfiguredGraphNodeRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.label().fmt(f)
    }
}

impl PartialOrd for ConfiguredGraphNodeRef {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.label().partial_cmp(other.label())
    }
}

impl Ord for ConfiguredGraphNodeRef {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.label().cmp(other.label())
    }
}

impl PartialEq for ConfiguredGraphNodeRef {
    fn eq(&self, other: &Self) -> bool {
        self.label().eq(other.label())
    }
}

impl Eq for ConfiguredGraphNodeRef {}

impl std::hash::Hash for ConfiguredGraphNodeRef {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.label().hash(state)
    }
}

impl NodeLabel for ConfiguredGraphNodeRef {}

impl QueryTarget for ConfiguredGraphNodeRef {
    type NodeRef = ConfiguredGraphNodeRef;
    type Attr = ConfiguredAttr;

    fn node_ref(&self) -> &Self::NodeRef {
        self
    }

    fn rule_type(&self) -> Cow<str> {
        Cow::Borrowed(self.0.rule_type().name())
    }

    fn buildfile_path(&self) -> &BuildFilePath {
        self.0.buildfile_path()
    }

    // TODO(cjhopman): Use existential traits to remove the Box<> once they are stabilized.
    fn deps<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Self::NodeRef> + Send + 'a> {
        box self.0.deps().map(ConfiguredGraphNodeRef::ref_cast)
    }

    fn exec_deps<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Self::NodeRef> + Send + 'a> {
        // TODO(cjhopman): This should return a Result. It should also be implemented.
        unimplemented!("exec_deps() isn't implemented for query attrs")
    }

    fn target_deps<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Self::NodeRef> + Send + 'a> {
        // TODO(cjhopman): This should return a Result. It should also be implemented.
        unimplemented!("target_deps() isn't implemented for query attrs")
    }

    fn attr_any_matches(
        attr: &Self::Attr,
        filter: &dyn Fn(&str) -> anyhow::Result<bool>,
    ) -> anyhow::Result<bool> {
        attr.any_matches(filter)
    }

    fn special_attrs_for_each<E, F: FnMut(&str, &Self::Attr) -> Result<(), E>>(
        &self,
        mut func: F,
    ) -> Result<(), E> {
        for (name, attr) in self.0.special_attrs() {
            func(&name, &attr)?;
        }
        Ok(())
    }

    fn attrs_for_each<E, F: FnMut(&str, &Self::Attr) -> Result<(), E>>(
        &self,
        mut func: F,
    ) -> Result<(), E> {
        for (name, attr) in self.0.attrs(AttrInspectOptions::All) {
            func(name, &attr)?;
        }
        Ok(())
    }

    fn map_attr<R, F: FnMut(Option<&Self::Attr>) -> R>(&self, key: &str, mut func: F) -> R {
        func(self.0.get(key, AttrInspectOptions::All).as_ref())
    }

    fn inputs_for_each<E, F: FnMut(CellPath) -> Result<(), E>>(
        &self,
        mut func: F,
    ) -> Result<(), E> {
        for input in self.0.inputs() {
            func(input)?;
        }
        Ok(())
    }

    fn call_stack(&self) -> Option<String> {
        self.0.call_stack()
    }
}
