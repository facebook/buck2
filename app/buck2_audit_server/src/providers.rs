/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::io::Write;

use async_trait::async_trait;
use buck2_audit::providers::AuditProvidersCommand;
use buck2_build_api::analysis::calculation::RuleAnalysisCalculation;
use buck2_build_api::interpreter::rule_defs::provider::collection::FrozenProviderCollectionValue;
use buck2_cli_proto::ClientContext;
use buck2_core::pattern::pattern_type::ProvidersPatternExtra;
use buck2_node::target_calculation::ConfiguredTargetCalculation;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::ctx::ServerCommandDiceContext;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;
use buck2_server_ctx::pattern::global_cfg_options_from_client_context;
use buck2_server_ctx::pattern::parse_and_resolve_patterns_to_targets_from_cli_args;
use buck2_util::indent::indent;
use dice::DiceComputations;
use dice::DiceTransaction;
use futures::stream::FuturesOrdered;
use futures::FutureExt;
use futures::StreamExt;
use gazebo::prelude::*;

use crate::AuditSubcommand;

#[async_trait]
impl AuditSubcommand for AuditProvidersCommand {
    async fn server_execute(
        &self,
        server_ctx: &dyn ServerCommandContextTrait,
        stdout: PartialResultDispatcher<buck2_cli_proto::StdoutBytes>,
        client_ctx: ClientContext,
    ) -> anyhow::Result<()> {
        server_ctx
            .with_dice_ctx(move |server_ctx, ctx| {
                server_execute_with_dice(self, client_ctx, server_ctx, stdout, ctx)
            })
            .await
    }
}

#[derive(Debug, buck2_error::Error)]
#[buck2(user)]
enum AuditProvidersError {
    #[error("Evaluation of at least one target providers failed")]
    AtLeastOneFailed,
}

async fn server_execute_with_dice(
    command: &AuditProvidersCommand,
    client_ctx: ClientContext,
    server_ctx: &dyn ServerCommandContextTrait,
    mut stdout: PartialResultDispatcher<buck2_cli_proto::StdoutBytes>,
    mut ctx: DiceTransaction,
) -> anyhow::Result<()> {
    let global_cfg_options =
        global_cfg_options_from_client_context(&client_ctx, server_ctx, &mut ctx).await?;

    let targets = parse_and_resolve_patterns_to_targets_from_cli_args::<ProvidersPatternExtra>(
        &mut ctx,
        &command
            .patterns
            .map(|pat| buck2_data::TargetPattern { value: pat.clone() }),
        server_ctx.working_dir(),
    )
    .await?;

    let mut futs = Vec::new();
    for (target_label, providers) in targets {
        let label = providers.into_providers_label(target_label.pkg(), target_label.name());
        let providers_label = ctx
            .get_configured_provider_label(&label, &global_cfg_options)
            .await?;

        futs.push(DiceComputations::declare_closure(|ctx| {
            async move {
                let result = ctx.get_providers(&providers_label).await;
                (providers_label, result)
            }
            .boxed()
        }));
    }

    let mut futs: FuturesOrdered<_> = ctx.compute_many(futs).into_iter().collect();

    let mut stdout = stdout.as_writer();
    let mut stderr = server_ctx.stderr()?;

    let mut at_least_one_error = false;
    while let Some((target, result)) = futs.next().await {
        match result {
            Ok(v) => {
                let v: FrozenProviderCollectionValue = v.require_compatible()?;

                if command.quiet {
                    writeln!(&mut stdout, "{}", target)?
                } else if command.list {
                    let mut provider_names = v.provider_collection().provider_names();
                    // Create a deterministic output.
                    provider_names.sort();
                    write!(
                        &mut stdout,
                        "{}:\n{}",
                        target,
                        indent(
                            "  ",
                            &provider_names
                                .iter()
                                .fold(String::new(), |acc, arg| acc + &format!("- {}\n", arg))
                        )
                    )?;
                } else if command.print_debug {
                    write!(
                        &mut stdout,
                        "{}:\n{}",
                        target,
                        indent("  ", &format!("{:?}", v.provider_collection()))
                    )?;
                } else {
                    write!(
                        &mut stdout,
                        "{}:\n{}",
                        target,
                        indent("  ", &format!("{:#}", v.provider_collection()))
                    )?;
                }
            }
            Err(e) => {
                write!(
                    &mut stderr,
                    "{}: failed:\n{}",
                    target,
                    indent("  ", &format!("{:?}", e))
                )?;
                at_least_one_error = true;
            }
        }
    }

    stdout.flush()?;
    stderr.flush()?;

    // FIXME(JakobDegen): We should try preserving error metadata here
    if at_least_one_error {
        Err(AuditProvidersError::AtLeastOneFailed.into())
    } else {
        Ok(())
    }
}
