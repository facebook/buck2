use anyhow::Context;
use buck2_common::{dice::cells::HasCellResolver, legacy_configs::dice::HasLegacyConfigs};
use buck2_interpreter::{common::StarlarkModulePath, file_loader::LoadedModule};
use dice::DiceTransaction;
use gazebo::prelude::*;
use starlark::{
    collections::SmallMap,
    environment::Module,
    eval::Evaluator,
    values::{structs::Struct, OwnedFrozenValueTyped, Value, ValueTyped},
};

use crate::{
    bxl::{
        common::CliResolutionCtx,
        result::BxlResult,
        starlark_defs::{
            cli_args::{CliArgValue, CliArgValueExt},
            context::{starlark_async::BxlSafeDiceComputations, BxlContext},
            FrozenBxlFunction,
        },
        BxlFunctionLabel, BxlKey,
    },
    calculation::Calculation,
    deferred::DeferredTable,
};

pub async fn eval(ctx: DiceTransaction, key: BxlKey) -> anyhow::Result<BxlResult> {
    let bxl_module = ctx
        .get_loaded_module(StarlarkModulePath::BxlFile(&key.label().bxl_path))
        .await?;

    let cell_resolver = ctx.get_cell_resolver().await;

    let frozen_callable = get_bxl_callable(key.label(), &bxl_module);

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

    // The bxl function may trigger async operations like builds, analysis, parsing etc, but those
    // will be blocking calls so that starlark can remain synchronous.
    // To avoid blocking a tokio thread, we spawn bxl as a blocking tokio task
    tokio::task::spawn_blocking(move || {
        let env = Module::new();

        let resolved_args = env.heap().alloc(Struct::new(
            key.cli_args()
                .iter()
                .map(|(k, v)| (env.heap().alloc_str(k), v.as_starlark(env.heap())))
                .collect(),
        ));

        let mut eval = Evaluator::new(&env);
        let bxl_ctx = BxlContext::new(
            eval.heap(),
            key,
            resolved_args,
            target_alias_resolver,
            bxl_cell,
            BxlSafeDiceComputations::new(&ctx),
        );
        let bxl_ctx = ValueTyped::<BxlContext>::new(env.heap().alloc(bxl_ctx)).unwrap();

        let result = eval_bxl(&mut eval, &frozen_callable, bxl_ctx.to_value())?;
        env.set("", result);

        let maybe_state = BxlContext::take_state(bxl_ctx);
        let has_print = BxlContext::has_print(bxl_ctx);

        match maybe_state {
            Some(registry) => {
                // this bxl registered actions, so extract the deferreds from it
                let (frozen, deferred) = registry.finalize(&env)(env)?;
                let result = frozen.get("").unwrap();

                let deferred_table = DeferredTable::new(deferred.take_result()?);

                anyhow::Ok(BxlResult::new(has_print, result.value(), deferred_table))
            }
            None => {
                // this bxl did not try to build anything, so we don't have any deferreds
                anyhow::Ok(BxlResult::new(
                    has_print,
                    result,
                    DeferredTable::new(hashmap! {}),
                ))
            }
        }
    })
    .await??
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
) -> OwnedFrozenValueTyped<FrozenBxlFunction> {
    let callable = bxl_module
        .env()
        .get_any_visibility(&spec.name)
        .unwrap_or_else(|| {
            unreachable!(
                "Expected a bxl {}, only {:?}, but there was no value with that name.",
                spec.name,
                bxl_module.env().names().collect::<Vec<_>>()
            )
        })
        .0;

    callable
        .downcast::<FrozenBxlFunction>()
        .unwrap_or_else(|e| {
            panic!(
                "A bxl function should be a BxlFunction. It was a {}",
                e.value().get_type(),
            )
        })
}

pub fn resolve_cli_args<'a>(
    spec: &BxlFunctionLabel,
    cli_ctx: &CliResolutionCtx,
    bxl_args: Vec<String>,
    frozen_callable: &'a FrozenBxlFunction,
) -> anyhow::Result<SmallMap<String, CliArgValue>> {
    frozen_callable.parse_clap(
        frozen_callable
            .to_clap(clap::Command::new(&spec.name).no_binary_name(true))
            .try_get_matches_from(bxl_args)?,
        cli_ctx,
    )
}
