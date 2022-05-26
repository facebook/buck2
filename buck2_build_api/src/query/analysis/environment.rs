/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{borrow::Cow, collections::HashSet, marker::PhantomData};

use async_trait::async_trait;
use buck2_core::{cells::paths::CellPath, target::ConfiguredTargetLabel};
use buck2_interpreter::common::BuildFilePath;
use buck2_query::{
    query::{
        environment::{NodeLabel, QueryEnvironment, QueryTarget},
        syntax::simple::{
            eval::{error::QueryError, file_set::FileSet, set::TargetSet, values::QueryValue},
            functions::{
                helpers::{QueryBinaryOp, QueryFunction},
                DefaultQueryFunctions, QueryFunctions,
            },
        },
        traversal::{
            async_depth_limited_traversal, async_fast_depth_first_postorder_traversal,
            async_unordered_traversal, AsyncTraversalDelegate, ChildVisitor, NodeLookup,
        },
    },
    query_module,
};
use buck2_query_parser::BinaryOp;
use futures::Future;
use gazebo::prelude::*;
use thiserror::Error;

use crate::{
    analysis::configured_graph::ConfiguredGraphNode, attrs::configured_attr::ConfiguredAttr,
};

#[derive(Debug, Error)]
enum AnalysisQueryError {
    #[error("file literals aren't supported in query attributes (got `{0}`)")]
    FileLiteralsNotAllowed(String),
}

#[async_trait]
pub trait ConfiguredGraphQueryEnvironmentDelegate: Send + Sync {
    fn eval_literal(&self, literal: &str) -> anyhow::Result<ConfiguredGraphNode>;

    async fn get_template_info_provider(
        &self,
        configured_label: &ConfiguredTargetLabel,
        template_name: &str,
    ) -> anyhow::Result<Vec<ConfiguredTargetLabel>>;
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
            defaults: DefaultQueryFunctions<ConfiguredGraphQueryEnvironment<'a>>,
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
            defaults: DefaultQueryFunctions::new(),
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
            let mut labels: HashSet<ConfiguredTargetLabel> = HashSet::new();

            for target in targets.iter() {
                let target_labels = self
                    .delegate
                    .get_template_info_provider(target.label(), template_name)
                    .await?;
                labels.extend(target_labels);
            }

            struct TraversalDelegate {
                targets: TargetSet<ConfiguredGraphNodeRef>,
                labels_to_find: HashSet<ConfiguredTargetLabel>,
            }

            #[async_trait]
            impl AsyncTraversalDelegate<ConfiguredGraphNodeRef> for TraversalDelegate {
                fn visit(&mut self, target: ConfiguredGraphNodeRef) -> anyhow::Result<()> {
                    let label = &target.label();
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
                        func.visit(dep.node_ref().dupe())?;
                    }

                    Ok(())
                }
            }

            let mut delegate = TraversalDelegate {
                targets: TargetSet::new(),
                labels_to_find: labels,
            };
            async_unordered_traversal(self, targets.iter(), &mut delegate).await?;

            Ok(delegate.targets)
        }
    }
}

#[async_trait]
impl<'a> QueryEnvironment for ConfiguredGraphQueryEnvironment<'a> {
    type Target = ConfiguredGraphNodeRef;

    async fn get_node(&self, node_ref: ConfiguredGraphNodeRef) -> anyhow::Result<Self::Target> {
        Ok(node_ref)
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
            root.iter().map(|e| e.node_ref()),
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

#[derive(Debug, Dupe, Clone)]
pub struct ConfiguredGraphNodeRef(pub ConfiguredGraphNode);

impl ConfiguredGraphNodeRef {
    pub fn label(&self) -> &ConfiguredTargetLabel {
        self.0.label()
    }
}

impl std::fmt::Display for ConfiguredGraphNodeRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.label().fmt(f)
    }
}

impl PartialOrd for ConfiguredGraphNodeRef {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.0.label().partial_cmp(other.0.label())
    }
}

impl Ord for ConfiguredGraphNodeRef {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.label().cmp(other.0.label())
    }
}

impl PartialEq for ConfiguredGraphNodeRef {
    fn eq(&self, other: &Self) -> bool {
        self.0.label().eq(other.0.label())
    }
}

impl Eq for ConfiguredGraphNodeRef {}

impl std::hash::Hash for ConfiguredGraphNodeRef {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.label().hash(state)
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
        Cow::Borrowed(self.0.node().rule_type().name())
    }

    fn buildfile_path(&self) -> &BuildFilePath {
        self.0.node().buildfile_path()
    }

    // TODO(cjhopman): Use existential traits to remove the Box<> once they are stabilized.
    fn deps<'a>(&'a self) -> Box<dyn Iterator<Item = Self::NodeRef> + Send + 'a> {
        box self
            .0
            .graph_deps()
            .map(|l| ConfiguredGraphNodeRef(l.dupe()))
    }

    fn exec_deps<'a>(&'a self) -> Box<dyn Iterator<Item = Self::NodeRef> + Send + 'a> {
        // TODO(cjhopman): This should return a Result. It should also be implemented.
        unimplemented!("exec_deps() isn't implemented for query attrs")
    }

    fn target_deps<'a>(&'a self) -> Box<dyn Iterator<Item = Self::NodeRef> + Send + 'a> {
        // TODO(cjhopman): This should return a Result. It should also be implemented.
        unimplemented!("target_deps() isn't implemented for query attrs")
    }

    fn special_attrs_for_each<E, F: FnMut(&str, &Self::Attr) -> Result<(), E>>(
        &self,
        mut func: F,
    ) -> Result<(), E> {
        for (name, attr) in self.0.node().special_attrs() {
            func(&name, &attr)?;
        }
        Ok(())
    }

    fn attrs_for_each<E, F: FnMut(&str, &Self::Attr) -> Result<(), E>>(
        &self,
        mut func: F,
    ) -> Result<(), E> {
        for (name, attr) in self.0.node().attrs() {
            func(name, &attr)?;
        }
        Ok(())
    }

    fn map_attr<R, F: FnMut(Option<&Self::Attr>) -> R>(&self, key: &str, mut func: F) -> R {
        func(self.0.node().get(key).as_ref())
    }

    fn inputs_for_each<E, F: FnMut(CellPath) -> Result<(), E>>(
        &self,
        mut func: F,
    ) -> Result<(), E> {
        for input in self.0.node().inputs() {
            func(input)?;
        }
        Ok(())
    }

    fn call_stack(&self) -> Option<String> {
        self.0.node().call_stack()
    }
}
