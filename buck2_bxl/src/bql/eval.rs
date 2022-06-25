/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Write;
use std::sync::Arc;

use buck2_build_api::query::dice::DiceQueryDelegate;
use buck2_build_api::query::uquery::environment::UqueryEnvironment;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::package_boundary::HasPackageBoundaryExceptions;
use buck2_common::target_aliases::HasTargetAliasResolver;
use buck2_core::fs::paths::AbsPathBuf;
use buck2_core::fs::project::ProjectRelativePathBuf;
use buck2_core::pattern::ParsedPattern;
use dice::*;
use gazebo::prelude::*;
use starlark::environment::GlobalsBuilder;
use starlark::environment::Module;
use starlark::eval::Evaluator;
use starlark::syntax::AstModule;
use starlark::syntax::Dialect;
use tokio::runtime::Handle;

use crate::bql::internals::QueryInternals;
use crate::bql::register_query_functions;

/// Evaluates some bql script. TargetNodes are resolved via the interpreter from
/// the provided DiceCtx.
pub async fn eval_bql(
    ctx: DiceTransaction,
    working_dir: ProjectRelativePathBuf,
    project_root: AbsPathBuf,
    global_target_platform: Option<String>,
    bql: String,
    args: Vec<String>,
) -> anyhow::Result<()> {
    let handle = Handle::try_current()?;

    let package_boundary_exceptions = ctx.get_package_boundary_exceptions().await?;

    let cell_resolver = ctx.get_cell_resolver().await?;
    let target_alias_resolver = ctx
        .target_alias_resolver_for_working_dir(&working_dir)
        .await?;

    // query evaluation lazily processes BUCK files as it needs results for new packages. The
    // evaluation itself is written expecting this to be synchronous.
    // To not block core threads, we do evaluation on a separate thread and then dispatch dice
    // async work back to the core threads and block on it.
    // We should consider making the query evaluation async so that we don't need to just through these hoops.

    tokio::task::spawn_blocking(move || -> anyhow::Result<_> {
        // TODO(cjhopman): We should consider having a script dialect or something that
        // would allow ifs/fors/etc at the top-level.
        // Currently we take the provided bql contents and stick them into the body of a
        // main() function and then invoke that function. This allows the bql file
        // to use ifs/fors/etc at the top level.
        let prologue = "def main():\n";
        let epilogue = "main()";

        let mut content = prologue.to_owned();

        for line in bql.lines() {
            writeln!(content, "    {}", line).unwrap();
        }
        content += epilogue;

        let global_env = GlobalsBuilder::extended()
            .with(register_query_functions)
            .build();

        let global_target_platform = match global_target_platform {
            Some(global_target_platform) => Some(
                ParsedPattern::parse_precise(
                    cell_resolver.root_cell_cell_alias_resolver(),
                    &global_target_platform,
                )?
                .as_target_label(&global_target_platform)?,
            ),
            None => None,
        };

        let uquery_delegate = Arc::new(DiceQueryDelegate::new(
            &ctx,
            &working_dir,
            project_root,
            cell_resolver,
            global_target_platform,
            package_boundary_exceptions,
            target_alias_resolver,
        )?);

        let uquery_env = UqueryEnvironment::new(uquery_delegate.dupe(), uquery_delegate);

        // TODO(cjhopman): Consider using the bql filename as the import id.
        let import_id = "<query>";
        let env = Module::new();
        let dialect = Dialect {
            enable_load: false,
            ..Dialect::Extended
        };
        let ast = AstModule::parse(import_id, content, &dialect)?;

        let mut eval = Evaluator::new(&env);
        let internals = QueryInternals::new(handle, Arc::new(uquery_env), args);
        eval.extra = Some(&internals);

        eval.eval_module(ast, &global_env)?;
        Ok(())
    })
    .await?
}
