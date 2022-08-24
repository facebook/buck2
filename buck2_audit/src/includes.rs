/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::io::Write;
use std::path::Path;

use async_trait::async_trait;
use buck2_client::common::CommonBuildConfigurationOptions;
use buck2_client::common::CommonConsoleOptions;
use buck2_client::common::CommonDaemonCommandOptions;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::result::SharedResult;
use buck2_common::result::ToSharedResultExt;
use buck2_core::bzl::ImportPath;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::CellInstance;
use buck2_core::cells::CellResolver;
use buck2_core::fs::paths::AbsPath;
use buck2_core::fs::paths::AbsPathBuf;
use buck2_core::fs::paths::RelativePath;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::package::Package;
use buck2_interpreter::common::StarlarkModulePath;
use buck2_interpreter::file_loader::LoadedModule;
use buck2_interpreter_for_build::interpreter::calculation::InterpreterCalculation;
use buck2_interpreter_for_build::interpreter::module_internals::EvaluationResult;
use buck2_query::query::environment::LabeledNode;
use buck2_query::query::environment::NodeLabel;
use buck2_query::query::traversal::async_depth_first_postorder_traversal;
use buck2_query::query::traversal::AsyncNodeLookup;
use buck2_query::query::traversal::AsyncTraversalDelegate;
use buck2_query::query::traversal::ChildVisitor;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use cli_proto::ClientContext;
use derive_more::Display;
use dice::DiceComputations;
use futures::stream::FuturesOrdered;
use futures::StreamExt;
use gazebo::prelude::*;
use indexmap::indexmap;
use itertools::Itertools;
use ref_cast::RefCast;
use serde::ser::SerializeMap;
use serde::Serialize;
use serde::Serializer;
use thiserror::Error;

use crate::AuditSubcommand;

#[derive(Debug, Error)]
enum AuditIncludesError {
    #[error("When loading buildfile for `{0}` found a mismatched buildfile name (`{1}`)")]
    WrongBuildfilePath(CellPath, String),
    #[error("invalid buildfile path `{0}`")]
    InvalidPath(CellPath),
}

#[derive(Debug, clap::Parser, serde::Serialize, serde::Deserialize)]
#[clap(
    name = "audit-includes",
    about = "list build file extensions imported at parse time."
)]
pub struct AuditIncludesCommand {
    #[clap(flatten)]
    pub config_opts: CommonBuildConfigurationOptions,

    #[clap(flatten)]
    console_opts: CommonConsoleOptions,

    #[clap(flatten)]
    event_log_opts: CommonDaemonCommandOptions,

    /// Print json representation of outputs
    #[clap(long)]
    json: bool,

    #[clap(
        name = "BUILD_FILES",
        help = "Build files to audit. These are expected to be relative paths from the working dir cell."
    )]
    patterns: Vec<String>,
}

async fn get_transitive_includes(
    ctx: &DiceComputations,
    load_result: &EvaluationResult,
) -> anyhow::Result<Vec<ImportPath>> {
    // We define a simple graph of LoadedModules to traverse.
    #[derive(Clone, Dupe)]
    struct Node(LoadedModule);

    impl Node {
        fn import_path(&self) -> &ImportPath {
            *self
                .0
                .path()
                .unpack_load_file()
                .expect("only visit imports so only bzl files are expected")
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

    struct Lookup<'a> {
        ctx: &'a DiceComputations,
    }

    #[async_trait]
    impl AsyncNodeLookup<Node> for Lookup<'_> {
        async fn get(&self, label: &NodeRef) -> anyhow::Result<Node> {
            Ok(Node(
                self.ctx
                    .get_loaded_module(StarlarkModulePath::LoadFile(&label.0))
                    .await?,
            ))
        }
    }

    struct Delegate {
        imports: Vec<ImportPath>,
    }

    #[async_trait]
    impl AsyncTraversalDelegate<Node> for Delegate {
        fn visit(&mut self, target: Node) -> anyhow::Result<()> {
            self.imports.push(target.import_path().clone());
            Ok(())
        }

        async fn for_each_child(
            &mut self,
            target: &Node,
            func: &mut dyn ChildVisitor<Node>,
        ) -> anyhow::Result<()> {
            for import in target.0.imports() {
                func.visit(NodeRef(import.clone()))?;
            }
            Ok(())
        }
    }

    let mut delegate = Delegate { imports: vec![] };
    let lookup = Lookup { ctx };

    async_depth_first_postorder_traversal(
        &lookup,
        load_result.imports().map(NodeRef::ref_cast),
        &mut delegate,
    )
    .await?;
    Ok(delegate.imports)
}

async fn load_and_collect_includes(
    ctx: &DiceComputations,
    path: &CellPath,
) -> SharedResult<Vec<ImportPath>> {
    let parent = path
        .parent()
        .ok_or_else(|| anyhow::anyhow!(AuditIncludesError::InvalidPath(path.clone())))?;
    let package = Package::new(parent.cell(), parent.path());
    let load_result = ctx.get_interpreter_results(&package).await?;

    let buildfile_name = load_result.buildfile_path().filename();
    if buildfile_name
        != path
            .path()
            .file_name()
            .expect("checked that this has a parent above")
    {
        return Err(anyhow::anyhow!(AuditIncludesError::WrongBuildfilePath(
            path.clone(),
            buildfile_name.to_owned().into_inner(),
        )))
        .shared_error();
    }

    Ok(get_transitive_includes(ctx, &load_result).await?)
}

fn resolve_path(
    cells: &CellResolver,
    fs: &ProjectRoot,
    current_cell: &CellInstance,
    path: &str,
) -> anyhow::Result<CellPath> {
    let as_path = Path::new(&path);
    let abs_path: AbsPathBuf = if as_path.is_absolute() {
        AbsPath::new(as_path)?.to_buf()
    } else {
        // otherwise, to match buck1 it is treated as relative to the working dir cell root (not the working dir).
        // the easiest way to consistently handle non-forward relative paths is to just resolve to absolute here.
        fs.resolve(current_cell.path().as_project_relative_path())
            .join_normalized(RelativePath::from_path(as_path)?)?
    };

    let project_path = fs.relativize(&abs_path)?;
    cells.get_cell_path(&project_path)
}

#[async_trait]
impl AuditSubcommand for AuditIncludesCommand {
    async fn server_execute(
        &self,
        mut server_ctx: Box<dyn ServerCommandContextTrait>,
        _client_ctx: ClientContext,
    ) -> anyhow::Result<()> {
        let ctx = server_ctx.dice_ctx().await?;
        let cells = ctx.get_cell_resolver().await?;
        let cwd = server_ctx.working_dir();
        let current_cell = cells.get(cells.find(cwd)?)?;
        let fs = server_ctx.project_root();

        let futures: FuturesOrdered<_> = self
            .patterns
            .iter()
            .unique()
            .map(|path| {
                let path = path.to_owned();
                let ctx = ctx.dupe();
                let cell_path = resolve_path(&cells, fs, current_cell, &path);
                async move {
                    let load_result = try {
                        let cell_path = cell_path?;
                        load_and_collect_includes(&ctx, &cell_path).await?
                    };
                    (path, load_result)
                }
            })
            .collect();

        let results: Vec<(_, SharedResult<Vec<_>>)> = futures.collect().await;
        // This is expected to not return any errors, and so we're not careful about not propagating it.
        let to_absolute_path = move |include: ImportPath| -> anyhow::Result<_> {
            let include = include.path();
            let cell = cells.get(include.cell())?;
            let path = cell.path().join(include.path());
            Ok(fs.resolve(&path))
        };
        let absolutize_paths = |paths: Vec<ImportPath>| -> SharedResult<Vec<AbsPathBuf>> {
            Ok(paths.into_try_map(&to_absolute_path)?)
        };
        let results: Vec<(String, SharedResult<Vec<AbsPathBuf>>)> =
            results.into_map(|(path, includes)| (path, includes.and_then(&absolutize_paths)));

        let mut stdout = server_ctx.stdout()?;

        // For the printing of results, we don't need to propagate errors, just print
        // them. After we print the results, we'll propagate an error if there is one.
        if self.json {
            let mut ser = serde_json::Serializer::pretty(&mut stdout);
            // buck1 has a bug where it doesn't properly handle >1 arg when passed --json
            // it also, sadly, prints just a single list of outputs for that case. we match
            // buck1's behavior for 1 successful file and print a dictionary for multiple. This is
            // unfortunate, but we hope that users can migrate to the equivalent query commands instead.
            if let Some((_path, Ok(includes))) = results.as_singleton() {
                includes.serialize(&mut ser)?
            } else {
                let mut map = ser.serialize_map(Some(results.len()))?;
                for (path, includes) in &results {
                    match includes {
                        Ok(includes) => {
                            map.serialize_entry(path, &indexmap! {"includes" => &includes})?
                        }
                        Err(e) => {
                            map.serialize_entry(path, &indexmap! {"$error" => format!("{:#}", e)})?
                        }
                    }
                }
                map.end()?;
            }

            // flush a newline after serde output.
            writeln!(stdout)?;
        } else {
            for (path, includes) in &results {
                match includes {
                    Ok(includes) => {
                        // intentionally add a blank line after the header
                        writeln!(stdout, "# {}\n", path)?;
                        for include in includes {
                            // To match buck1, we print absolute paths.
                            writeln!(stdout, "{}", include)?;
                        }
                    }
                    Err(e) => {
                        // intentionally add a blank line after the header
                        writeln!(stdout, "! {}\n", path)?;
                        writeln!(stdout, "{:#}", e)?;
                    }
                }
            }
        }

        // propagate the first error.
        for (_, result) in results {
            result?;
        }

        Ok(())
    }

    fn config_opts(&self) -> Option<&CommonBuildConfigurationOptions> {
        Some(&self.config_opts)
    }

    fn console_opts(&self) -> Option<&CommonConsoleOptions> {
        Some(&self.console_opts)
    }

    fn event_log_opts(&self) -> Option<&CommonDaemonCommandOptions> {
        Some(&self.event_log_opts)
    }
}
