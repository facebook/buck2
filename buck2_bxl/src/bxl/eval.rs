use std::cell::RefCell;
use std::collections::HashMap;

use anyhow::Context;
use buck2_build_api::bxl::result::BxlResult;
use buck2_build_api::bxl::types::BxlFunctionLabel;
use buck2_build_api::bxl::types::BxlKey;
use buck2_build_api::bxl::types::CliArgValue;
use buck2_build_api::calculation::Calculation;
use buck2_build_api::deferred::types::BaseDeferredKey;
use buck2_build_api::deferred::types::DeferredTable;
use buck2_build_api::path::BuckOutPath;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::dice::data::HasIoProvider;
use buck2_common::legacy_configs::dice::HasLegacyConfigs;
use buck2_common::target_aliases::BuckConfigTargetAliasResolver;
use buck2_core::cells::CellAliasResolver;
use buck2_core::fs::paths::ForwardRelativePathBuf;
use buck2_core::package::Package;
use buck2_events::dispatch::with_dispatcher;
use buck2_interpreter::common::StarlarkModulePath;
use buck2_interpreter::dice::HasEvents;
use buck2_interpreter::file_loader::LoadedModule;
use buck2_interpreter_for_build::interpreter::calculation::InterpreterCalculation;
use dice::DiceComputations;
use dice::DiceTransaction;
use gazebo::prelude::*;
use starlark::collections::SmallMap;
use starlark::environment::Module;
use starlark::eval::Evaluator;
use starlark::values::structs::Struct;
use starlark::values::OwnedFrozenValueTyped;
use starlark::values::Value;
use starlark::values::ValueTyped;
use thiserror::Error;

use crate::bxl::starlark_defs::cli_args::CliArgValueExt;
use crate::bxl::starlark_defs::context::starlark_async::BxlSafeDiceComputations;
use crate::bxl::starlark_defs::context::BxlContext;
use crate::bxl::starlark_defs::FrozenBxlFunction;

pub async fn eval(ctx: DiceTransaction, key: BxlKey) -> anyhow::Result<BxlResult> {
    let bxl_module = ctx
        .get_loaded_module(StarlarkModulePath::BxlFile(&key.label().bxl_path))
        .await?;

    let cell_resolver = ctx.get_cell_resolver().await?;

    let frozen_callable = get_bxl_callable(key.label(), &bxl_module)?;

    let bxl_cell = cell_resolver
        .get(key.label().bxl_path.cell())
        .with_context(|| format!("Cell does not exist: `{}`", key.label().bxl_path.cell()))?
        .dupe();

    let config = ctx
        .get_legacy_config_for_cell(key.label().bxl_path.cell())
        .await
        .with_context(|| {
            format!(
                "No configuration for cell: `{}`",
                key.label().bxl_path.cell()
            )
        })?;

    let target_alias_resolver = config.target_alias_resolver();

    let project_fs = ctx.global_data().get_io_provider().fs().dupe();
    let artifact_fs = ctx.get_artifact_fs().await?;

    // The bxl function may trigger async operations like builds, analysis, parsing etc, but those
    // will be blocking calls so that starlark can remain synchronous.
    // To avoid blocking a tokio thread, we spawn bxl as a blocking tokio task
    let dispatcher = ctx.per_transaction_data().get_dispatcher().dupe();
    tokio::task::spawn_blocking(with_dispatcher(dispatcher, || {
        move || {
            let env = Module::new();

            let resolved_args = env.heap().alloc(Struct::new(
                key.cli_args()
                    .iter()
                    .map(|(k, v)| (env.heap().alloc_str(k), v.as_starlark(env.heap())))
                    .collect(),
            ));

            // we put a file as our output stream cache. The file is associated with the `BxlKey`, which
            // is super important, as it HAS to be the SAME as the DiceKey so that DICE is keeping
            // the output file cache up to date.
            let output_stream = BuckOutPath::new(
                BaseDeferredKey::BxlLabel(key.clone()),
                ForwardRelativePathBuf::unchecked_new(
                    "__bxl_internal__/outputstream_cache".to_owned(),
                ),
            );
            let file_path = artifact_fs
                .buck_out_path_resolver()
                .resolve_gen(&output_stream);

            let file = RefCell::new(box project_fs.create_file(&file_path, false)?);

            let mut eval = Evaluator::new(&env);
            let bxl_ctx = BxlContext::new(
                eval.heap(),
                key,
                resolved_args,
                target_alias_resolver,
                project_fs,
                artifact_fs,
                bxl_cell,
                BxlSafeDiceComputations::new(&ctx),
                file,
            );
            let bxl_ctx = ValueTyped::<BxlContext>::new(env.heap().alloc(bxl_ctx)).unwrap();

            let result = eval_bxl(&mut eval, &frozen_callable, bxl_ctx.to_value())?;

            if !result.is_none() {
                return Err(anyhow::anyhow!(NotAValidReturnType(result.get_type())));
            }

            let (actions, ensured_artifacts) = BxlContext::take_state(bxl_ctx)?;

            match actions {
                Some(registry) => {
                    // this bxl registered actions, so extract the deferreds from it
                    let (_, deferred) = registry.finalize(&env)(env)?;

                    let deferred_table = DeferredTable::new(deferred.take_result()?);

                    anyhow::Ok(BxlResult::new(
                        output_stream,
                        ensured_artifacts,
                        deferred_table,
                    ))
                }
                None => {
                    // this bxl did not try to build anything, so we don't have any deferreds
                    anyhow::Ok(BxlResult::new(
                        output_stream,
                        ensured_artifacts,
                        DeferredTable::new(HashMap::new()),
                    ))
                }
            }
        }
    }))
    .await?
}

fn eval_bxl<'a>(
    eval: &'a mut Evaluator<'a, '_>,
    frozen_callable: &'a FrozenBxlFunction,
    ctx: Value<'a>,
) -> anyhow::Result<Value<'a>> {
    let bxl_impl = frozen_callable.get_impl();
    eval.eval_function(bxl_impl.to_value(), &[ctx], &[])
}

pub fn get_bxl_callable<'a>(
    spec: &BxlFunctionLabel,
    bxl_module: &'a LoadedModule,
) -> anyhow::Result<OwnedFrozenValueTyped<FrozenBxlFunction>> {
    let callable = bxl_module.env().get_any_visibility(&spec.name)?.0;

    Ok(callable
        .downcast::<FrozenBxlFunction>()
        .unwrap_or_else(|e| {
            panic!(
                "A bxl function should be a BxlFunction. It was a {}",
                e.value().get_type(),
            )
        }))
}

pub struct CliResolutionCtx<'a> {
    pub target_alias_resolver: BuckConfigTargetAliasResolver,
    pub cell_resolver: CellAliasResolver,
    pub relative_dir: Package,
    pub dice: &'a DiceComputations,
}

pub async fn resolve_cli_args<'a>(
    spec: &BxlFunctionLabel,
    cli_ctx: &CliResolutionCtx<'a>,
    bxl_args: Vec<String>,
    frozen_callable: &'a FrozenBxlFunction,
) -> anyhow::Result<SmallMap<String, CliArgValue>> {
    frozen_callable
        .parse_clap(
            frozen_callable
                .to_clap(clap::Command::new(&spec.name).no_binary_name(true))
                .try_get_matches_from(bxl_args)?,
            cli_ctx,
        )
        .await
}

#[derive(Debug, Error)]
#[error("Expected `NoneType` to be returned from bxl. Got return value `{0}`")]
struct NotAValidReturnType(&'static str);
