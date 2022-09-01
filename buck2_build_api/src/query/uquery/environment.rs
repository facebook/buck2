/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::sync::Arc;

use anyhow::Context;
use async_trait::async_trait;
use buck2_common::pattern::resolve::ResolvedPattern;
use buck2_common::result::SharedError;
use buck2_common::result::SharedResult;
use buck2_common::result::ToSharedResultExt;
use buck2_core::bzl::ImportPath;
use buck2_core::cells::build_file_cell::BuildFileCell;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::CellName;
use buck2_core::fs::paths::FileName;
use buck2_core::fs::paths::FileNameBuf;
use buck2_core::package::Package;
use buck2_core::target::TargetLabel;
use buck2_core::target::TargetName;
use buck2_node::nodes::eval_result::EvaluationResult;
use buck2_node::nodes::unconfigured::TargetNode;
use buck2_query::query::environment::LabeledNode;
use buck2_query::query::environment::NodeLabel;
use buck2_query::query::environment::QueryEnvironment;
use buck2_query::query::environment::QueryEnvironmentError;
use buck2_query::query::environment::QueryTarget;
use buck2_query::query::syntax::simple::eval::file_set::FileNode;
use buck2_query::query::syntax::simple::eval::file_set::FileSet;
use buck2_query::query::syntax::simple::eval::set::TargetSet;
use buck2_query::query::syntax::simple::functions::docs::QueryEnvironmentDescription;
use buck2_query::query::syntax::simple::functions::DefaultQueryFunctionsModule;
use buck2_query::query::syntax::simple::functions::HasModuleDescription;
use buck2_query::query::traversal::async_depth_first_postorder_traversal;
use buck2_query::query::traversal::async_depth_limited_traversal;
use buck2_query::query::traversal::AsyncNodeLookup;
use buck2_query::query::traversal::AsyncTraversalDelegate;
use buck2_query::query::traversal::ChildVisitor;
use derive_more::Display;
use futures::stream::FuturesUnordered;
use futures::StreamExt;
use gazebo::prelude::*;
use indexmap::IndexSet;
use itertools::Itertools;
use ref_cast::RefCast;
use thiserror::Error;
use tracing::warn;

type ArcCellPath = Arc<CellPath>;

#[derive(Debug, Error)]
enum QueryLiteralResolutionError {
    #[error("literal `{0}` missing in pre-resolved literals")]
    LiteralMissing(String),
}

#[derive(Debug, Error)]
enum RBuildFilesError {
    #[error("no parent found for the file `{0}`")]
    ParentDoesNotExist(ArcCellPath),
    #[error("internal error: no buildfile names found for the cell `{0}`")]
    CellMissingBuildFileNames(CellName),
}

pub enum SpecialAttr {
    String(String),
}

/// UqueryDelegate resolves information needed by the QueryEnvironment.
#[async_trait]
pub trait UqueryDelegate: Send + Sync {
    /// Returns the EvaluationResult for evaluation of the buildfile.
    async fn eval_build_file(&self, packages: &Package) -> SharedResult<Arc<EvaluationResult>>;

    /// Get the imports from a LoadedModule corresponding to some path.
    async fn eval_module_imports(&self, path: &ImportPath) -> SharedResult<Vec<ImportPath>>;

    fn get_buildfile_names_by_cell(&self) -> anyhow::Result<HashMap<CellName, &[FileNameBuf]>>;

    /// Resolves a target pattern.
    async fn resolve_target_patterns(
        &self,
        pattern: &[&str],
    ) -> anyhow::Result<ResolvedPattern<TargetName>>;

    async fn eval_file_literal(&self, literal: &str) -> anyhow::Result<FileSet>;

    // Get all enclosing packages needed to compute owner function.
    // This always includes the immediate enclosing package of the path but can also include
    // all parent packages if the package matches `project.package_boundary_exceptions` buckconfig.
    async fn get_enclosing_packages(&self, path: &CellPath) -> anyhow::Result<Vec<Package>>;
}

#[async_trait]
pub trait QueryLiterals<T: QueryTarget>: Send + Sync {
    async fn eval_literals(&self, literals: &[&str]) -> anyhow::Result<TargetSet<T>>;
}

pub struct UqueryEnvironment<'c> {
    delegate: Arc<dyn UqueryDelegate + 'c>,
    literals: Arc<dyn QueryLiterals<TargetNode> + 'c>,
}

pub struct PreresolvedQueryLiterals<T: QueryTarget> {
    resolved_literals: HashMap<String, SharedResult<TargetSet<T>>>,
}

impl<T: QueryTarget> PreresolvedQueryLiterals<T> {
    pub fn new(resolved_literals: HashMap<String, SharedResult<TargetSet<T>>>) -> Self {
        Self { resolved_literals }
    }

    pub async fn pre_resolve(base: &dyn QueryLiterals<T>, literals: &[String]) -> Self {
        let futs = literals
            .iter()
            .map(|lit| async move { (lit.to_owned(), base.eval_literals(&[lit]).await) });
        let mut resolved_literals = HashMap::new();
        for (literal, result) in futures::future::join_all(futs).await {
            resolved_literals.insert(literal, result.shared_error());
        }
        Self { resolved_literals }
    }
}

#[async_trait]
impl<T: QueryTarget> QueryLiterals<T> for PreresolvedQueryLiterals<T> {
    async fn eval_literals(&self, literals: &[&str]) -> anyhow::Result<TargetSet<T>> {
        let mut targets = TargetSet::new();
        for lit in literals {
            let resolved = match self
                .resolved_literals
                .get(*lit)
                .ok_or_else(|| QueryLiteralResolutionError::LiteralMissing((*lit).to_owned()))?
            {
                Ok(v) => v,
                Err(e) => return Err(e.dupe().into()),
            };
            for target in resolved.iter() {
                targets.insert(target.dupe());
            }
        }
        Ok(targets)
    }
}

impl<'c> UqueryEnvironment<'c> {
    pub fn new(
        delegate: Arc<dyn UqueryDelegate + 'c>,
        literals: Arc<dyn QueryLiterals<TargetNode> + 'c>,
    ) -> Self {
        Self { delegate, literals }
    }

    pub fn describe() -> QueryEnvironmentDescription {
        QueryEnvironmentDescription {
            name: "Uquery Environment".to_owned(),
            mods: vec![DefaultQueryFunctionsModule::<Self>::describe()],
        }
    }

    async fn get_node(&self, target: &TargetLabel) -> anyhow::Result<TargetNode> {
        let package = self
            .delegate
            .eval_build_file(target.pkg())
            .await
            .with_context(|| format!("when looking up `{}``", target))?;
        let node = match package.targets().get(target.name()) {
            Some(target) => target,
            None => {
                return Err(QueryEnvironmentError::missing_target(
                    target,
                    package.targets().keys(),
                )
                .into());
            }
        };
        Ok(node.dupe())
    }
}

#[async_trait]
impl<'c> QueryEnvironment for UqueryEnvironment<'c> {
    type Target = TargetNode;

    async fn get_node(&self, node_ref: &TargetLabel) -> anyhow::Result<Self::Target> {
        UqueryEnvironment::get_node(self, node_ref).await
    }

    async fn eval_literals(&self, literals: &[&str]) -> anyhow::Result<TargetSet<TargetNode>> {
        self.literals.eval_literals(literals).await
    }

    async fn eval_file_literal(&self, literal: &str) -> anyhow::Result<FileSet> {
        self.delegate.eval_file_literal(literal).await
    }

    async fn dfs_postorder(
        &self,
        root: &TargetSet<TargetNode>,
        traversal_delegate: &mut dyn AsyncTraversalDelegate<TargetNode>,
    ) -> anyhow::Result<()> {
        async_depth_first_postorder_traversal(self, root.iter_names(), traversal_delegate).await
    }

    async fn depth_limited_traversal(
        &self,
        root: &TargetSet<Self::Target>,
        delegate: &mut dyn AsyncTraversalDelegate<Self::Target>,
        depth: u32,
    ) -> anyhow::Result<()> {
        async_depth_limited_traversal(self, root.iter_names(), delegate, depth).await
    }

    async fn allbuildfiles(&self, universe: &TargetSet<Self::Target>) -> anyhow::Result<FileSet> {
        return allbuildfiles(universe, &*self.delegate).await;
    }

    async fn rbuildfiles(&self, universe: &FileSet, argset: &FileSet) -> anyhow::Result<FileSet> {
        return rbuildfiles(universe, argset, &*self.delegate).await;
    }

    async fn owner(&self, paths: &FileSet) -> anyhow::Result<TargetSet<Self::Target>> {
        let mut result: TargetSet<Self::Target> = TargetSet::new();
        for path in paths.iter() {
            // need to explicitly track this rather than checking for changes to result set since the owner might
            // already be in the set.
            let mut found_owner = false;
            match self.delegate.get_enclosing_packages(path).await {
                Ok(packages) => {
                    let package_futs = packages.map(|package| async move {
                        // TODO(cjhopman): We should make sure that the file exists.
                        let targets = self.delegate.eval_build_file(package).await?;

                        let owner_targets: Vec<Self::Target> = targets
                            .targets()
                            .values()
                            .filter_map(|node| {
                                for input in node.inputs() {
                                    if &input == path {
                                        return Some(node.dupe());
                                        // this intentionally breaks out of the loop. We don't need to look at the
                                        // other inputs of this target, but it's possible for a single file to be owned by
                                        // multiple targets.
                                    }
                                }
                                None
                            })
                            .collect();
                        anyhow::Ok(owner_targets)
                    });

                    for nodes in futures::future::join_all(package_futs).await.into_iter() {
                        for node in nodes?.into_iter() {
                            found_owner = true;
                            result.insert(node);
                        }
                    }
                }
                Err(_) => {
                    // we don't consider this an error, it's usually the case that the user
                    // just wants to know the target owning the file if it exists.
                }
            };
            if !found_owner {
                warn!("No owner was found for {}", path);
            }
        }
        Ok(result)
    }
}

#[async_trait]
impl<'a> AsyncNodeLookup<TargetNode> for UqueryEnvironment<'a> {
    async fn get(&self, label: &TargetLabel) -> anyhow::Result<TargetNode> {
        self.get_node(label).await
    }
}

pub(crate) async fn allbuildfiles<'c, T: QueryTarget>(
    universe: &TargetSet<T>,
    delegate: &'c dyn UqueryDelegate,
) -> anyhow::Result<FileSet> {
    let mut paths = IndexSet::<FileNode>::new();

    let mut top_level_imports = Vec::<ImportPath>::new();

    for target in universe.iter() {
        paths.insert(FileNode(target.dupe().buildfile_path().path()));

        let eval_result = delegate
            .eval_build_file(target.buildfile_path().package())
            .await?; // TODO: no longer use eval_build_file, just parse imports directly (will solve async issue too)

        top_level_imports.extend(eval_result.imports().cloned());
    }

    let loads = get_transitive_loads(top_level_imports, delegate).await?;

    let mut new_paths = IndexSet::<FileNode>::new();
    for load in &loads {
        new_paths.insert(FileNode(load.path().clone()));
    }

    Ok(FileSet::new(paths).union(&FileSet::new(new_paths)))
}

pub(crate) async fn rbuildfiles<'c>(
    universe: &FileSet,
    argset: &FileSet,
    delegate: &'c dyn UqueryDelegate,
) -> anyhow::Result<FileSet> {
    let universe_paths: Vec<ArcCellPath> =
        universe.iter().map(|file| Arc::new(file.clone())).collect();
    // step 1: split the build files and bzl files
    let (buildfiles, bzlfiles) = split_universe_files(&universe_paths, delegate)?;

    // step 2: get all top level imports accordingly
    let top_level_import_by_build_file =
        top_level_imports_by_build_file(&buildfiles, &bzlfiles, delegate).await?;

    // step 3: get the first order imports for every loaded file, to lookup during traversal
    // TODO(benfoxman) this is actually unnecessary, since we can get the imports while traversing.
    // Will fix this in a separate diff.
    let all_top_level_imports: Vec<ImportPath> = top_level_import_by_build_file
        .iter()
        .flat_map(|(_, imports)| imports.iter().cloned())
        .collect();

    let first_order_import_map = first_order_imports(&all_top_level_imports, delegate).await?;

    // step 4: do rdeps()-like traversal from the top level imports down to bzls

    #[derive(Clone, Dupe)]
    struct Node(Arc<ImportPath>);

    impl Node {
        fn import_path(&self) -> &ImportPath {
            &self.0
        }
    }

    #[derive(Display, Debug, Hash, Eq, PartialEq, Clone, RefCast)]
    #[repr(transparent)]
    struct NodeRef(ImportPath);

    impl NodeLabel for NodeRef {}

    impl LabeledNode for Node {
        type NodeRef = NodeRef;

        fn node_ref(&self) -> &NodeRef {
            NodeRef::ref_cast(self.import_path())
        }
    }

    struct Lookup {}

    #[async_trait]
    impl AsyncNodeLookup<Node> for Lookup {
        async fn get(&self, label: &NodeRef) -> anyhow::Result<Node> {
            Ok(Node(Arc::new(label.0.clone())))
        }
    }

    struct Delegate<'c> {
        output_paths: Vec<ImportPath>,
        argset: &'c FileSet,
        first_order_import_map: HashMap<ImportPath, Vec<ImportPath>>,
    }

    #[async_trait]
    impl AsyncTraversalDelegate<Node> for Delegate<'_> {
        fn visit(&mut self, node: Node) -> anyhow::Result<()> {
            let node_import = node.import_path();
            if self.argset.iter().contains(node_import.path()) {
                self.output_paths.push(node_import.clone());
            } else {
                let loads = self
                    .first_order_import_map
                    .get(node_import)
                    .expect("import path should exist");
                for load in loads.iter() {
                    for arg in self.argset.iter() {
                        if load.path() == arg {
                            self.output_paths.push(node_import.clone());
                            return Ok(());
                        }
                    }
                }
            }
            Ok(())
        }
        async fn for_each_child(
            &mut self,
            node: &Node,
            func: &mut dyn ChildVisitor<Node>,
        ) -> anyhow::Result<()> {
            for import in self
                .first_order_import_map
                .get(node.import_path())
                .expect("import path should exist")
            {
                func.visit(NodeRef(import.clone()))?;
            }
            Ok(())
        }
    }

    let lookup = Lookup {};
    let mut delegate = Delegate {
        output_paths: vec![],
        argset,
        first_order_import_map,
    };

    // step 5: do traversal, get all modified imports
    async_depth_first_postorder_traversal(
        &lookup,
        all_top_level_imports.iter().map(NodeRef::ref_cast),
        &mut delegate,
    )
    .await?;

    let mut output_files = IndexSet::<FileNode>::new();
    for file in &delegate.output_paths {
        output_files.insert(FileNode(file.path().clone()));
    }

    // finally, since we didn't check the top level imports of buildfiles,
    // we do this right now
    for file in buildfiles {
        if argset.iter().contains(&*file) {
            output_files.insert(FileNode((*file).clone()));
        } else {
            let imports = top_level_import_by_build_file
                .get(&file)
                .expect("should have stored a universe file's input");

            for import in imports.iter() {
                if output_files
                    .iter()
                    .contains(&FileNode(import.path().clone()))
                {
                    output_files.insert(FileNode((*file).clone()));
                    break;
                }
            }
        }
    }

    Ok(FileSet::new(output_files))
}

fn split_universe_files<'c>(
    universe: &[ArcCellPath],
    delegate: &'c dyn UqueryDelegate,
) -> anyhow::Result<(Vec<ArcCellPath>, Vec<ArcCellPath>)> {
    let mut buildfiles = Vec::<ArcCellPath>::new();
    let mut bzlfiles = Vec::<ArcCellPath>::new();
    let buildfile_names_by_cell = delegate.get_buildfile_names_by_cell()?;
    for file in universe {
        let buildfile_names_for_file =
            buildfile_names_by_cell.get(file.cell()).ok_or_else(|| {
                anyhow::anyhow!(RBuildFilesError::CellMissingBuildFileNames(
                    file.cell().clone()
                ))
            })?;

        if let Some(name) = file.path().file_name() {
            if buildfile_names_for_file
                .iter()
                .map(<FileNameBuf as AsRef<FileName>>::as_ref)
                .contains(&name)
            {
                buildfiles.push(file.dupe());
            }
        } else {
            // TODO: right now we assume non-buildfiles are bzl's - we might want to handle error cases later.
            bzlfiles.push(file.dupe());
        }
    }
    Ok((buildfiles, bzlfiles))
}

async fn top_level_imports_by_build_file<'c>(
    buildfiles: &[ArcCellPath],
    bzlfiles: &[ArcCellPath],
    delegate: &'c dyn UqueryDelegate,
) -> anyhow::Result<HashMap<ArcCellPath, Vec<ImportPath>>> {
    let mut top_level_import_by_build_file = HashMap::<ArcCellPath, Vec<ImportPath>>::new();

    for file in bzlfiles {
        let imports = vec![ImportPath::new(
            file.as_ref().clone(),
            BuildFileCell::new(file.cell().clone()),
        )?];
        top_level_import_by_build_file.insert(file.dupe(), imports);
    }

    let mut buildfile_futs: FuturesUnordered<_> = buildfiles
        .iter()
        .map(|file| async move {
            if let Some(parent) = file.parent() {
                (
                    file.dupe(),
                    delegate
                        .eval_build_file(&Package::new(parent.cell(), parent.path()))
                        .await,
                )
            } else {
                (
                    file.dupe(),
                    Err(SharedError::new(RBuildFilesError::ParentDoesNotExist(
                        file.dupe(),
                    ))),
                )
            }
        })
        .collect();

    while let Some((file, eval_result)) = tokio::task::unconstrained(buildfile_futs.next()).await {
        top_level_import_by_build_file
            .insert(file.dupe(), eval_result?.imports().cloned().collect());
    }

    Ok(top_level_import_by_build_file)
}

// TODO: no need to get all the first order imports prior to the traversal - we can do this
// at the same time as the actual traversal.
async fn first_order_imports<'c>(
    all_top_level_imports: &[ImportPath],
    delegate: &'c dyn UqueryDelegate,
) -> anyhow::Result<HashMap<ImportPath, Vec<ImportPath>>> {
    let all_imports = get_transitive_loads(all_top_level_imports.to_vec(), delegate).await?;

    let mut all_first_order_futs: FuturesUnordered<_> = all_imports
        .iter()
        .map(|node| async move { (node, delegate.eval_module_imports(node).await) })
        .collect();

    let mut first_order_import_map = HashMap::<ImportPath, Vec<ImportPath>>::new();

    while let Some((import, first_order_imports)) =
        tokio::task::unconstrained(all_first_order_futs.next()).await
    {
        let prev = first_order_import_map.insert(import.clone(), first_order_imports?);
        assert!(prev.is_none());
    }
    Ok(first_order_import_map)
}

// Uquery and Cquery share ImportPath traversal logic, so we move the logic to this function.
pub(crate) async fn get_transitive_loads<'c>(
    top_level_imports: Vec<ImportPath>,
    delegate: &'c dyn UqueryDelegate,
) -> anyhow::Result<Vec<ImportPath>> {
    #[derive(Clone, Dupe)]
    struct Node(Arc<ImportPath>);

    impl Node {
        fn import_path(&self) -> &ImportPath {
            &self.0
        }
    }

    #[derive(Display, Debug, Hash, Eq, PartialEq, Clone, RefCast)]
    #[repr(transparent)]
    struct NodeRef(ImportPath);

    impl NodeLabel for NodeRef {}

    impl LabeledNode for Node {
        type NodeRef = NodeRef;

        fn node_ref(&self) -> &NodeRef {
            NodeRef::ref_cast(self.import_path())
        }
    }

    struct Lookup {}

    #[async_trait]
    impl AsyncNodeLookup<Node> for Lookup {
        async fn get(&self, label: &NodeRef) -> anyhow::Result<Node> {
            Ok(Node(Arc::new(label.0.clone())))
        }
    }

    struct Delegate<'c> {
        imports: Vec<ImportPath>,
        delegate: &'c dyn UqueryDelegate,
    }

    #[async_trait]
    impl AsyncTraversalDelegate<Node> for Delegate<'_> {
        fn visit(&mut self, target: Node) -> anyhow::Result<()> {
            self.imports.push(target.import_path().clone());
            Ok(())
        }

        async fn for_each_child(
            &mut self,
            target: &Node,
            func: &mut dyn ChildVisitor<Node>,
        ) -> anyhow::Result<()> {
            for import in self
                .delegate
                .eval_module_imports(target.import_path())
                .await?
            {
                func.visit(NodeRef(import.clone()))?;
            }
            Ok(())
        }
    }

    let mut traversal_delegate = Delegate {
        imports: vec![],
        delegate: delegate.dupe(),
    };
    let lookup = Lookup {};

    let import_nodes = top_level_imports.iter().map(NodeRef::ref_cast);

    async_depth_first_postorder_traversal(&lookup, import_nodes, &mut traversal_delegate).await?;

    Ok(traversal_delegate.imports)
}
