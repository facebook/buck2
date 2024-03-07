/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;
use std::fmt::Display;
use std::io::Write;

use async_trait::async_trait;
use buck2_audit::subtargets::AuditSubtargetsCommand;
use buck2_build_api::analysis::calculation::RuleAnalysisCalculation;
use buck2_build_api::interpreter::rule_defs::provider::collection::FrozenProviderCollection;
use buck2_cli_proto::ClientContext;
use buck2_common::pattern::resolve::ResolveTargetPatterns;
use buck2_core::pattern::pattern_type::ProvidersPatternExtra;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersName;
use buck2_node::nodes::frontend::TargetGraphCalculation;
use buck2_node::target_calculation::ConfiguredTargetCalculation;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::ctx::ServerCommandDiceContext;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;
use buck2_server_ctx::pattern::global_cfg_options_from_client_context;
use buck2_server_ctx::pattern::parse_patterns_from_cli_args;
use buck2_server_ctx::stdout_partial_output::StdoutPartialOutput;
use buck2_util::indent::indent;
use dice::DiceTransaction;
use dupe::Dupe;
use futures::stream::FuturesOrdered;
use futures::StreamExt;
use gazebo::prelude::*;

use crate::AuditSubcommand;

#[async_trait]
impl AuditSubcommand for AuditSubtargetsCommand {
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

async fn server_execute_with_dice(
    command: &AuditSubtargetsCommand,
    client_ctx: ClientContext,
    server_ctx: &dyn ServerCommandContextTrait,
    mut stdout: PartialResultDispatcher<buck2_cli_proto::StdoutBytes>,
    mut ctx: DiceTransaction,
) -> anyhow::Result<()> {
    // TODO(raulgarcia4): Extract function where possible, shares a lot of code with audit providers.
    let global_cfg_options =
        global_cfg_options_from_client_context(&client_ctx, server_ctx, &mut ctx).await?;

    let parsed_patterns = parse_patterns_from_cli_args::<ProvidersPatternExtra>(
        &mut ctx,
        command
            .patterns
            .map(|pat| buck2_data::TargetPattern { value: pat.clone() })
            .as_slice(),
        server_ctx.working_dir(),
    )
    .await?;
    let resolved_pattern = ResolveTargetPatterns::resolve(&mut ctx, &parsed_patterns).await?;

    let mut futs = FuturesOrdered::new();
    for (package, spec) in resolved_pattern.specs {
        let targets = match spec {
            buck2_core::pattern::PackageSpec::Targets(targets) => targets,
            buck2_core::pattern::PackageSpec::All => {
                let interpreter_results = ctx.get_interpreter_results(package.dupe()).await?;
                interpreter_results
                    .targets()
                    .keys()
                    .map(|target| {
                        (
                            target.to_owned(),
                            ProvidersPatternExtra {
                                providers: ProvidersName::Default,
                            },
                        )
                    })
                    .collect()
            }
        };

        for (target_name, providers) in targets {
            let label = providers.into_providers_label(package.dupe(), target_name.as_ref());
            let providers_label = ctx
                .get_configured_provider_label(&label, &global_cfg_options)
                .await?;

            // `.push` is deprecated in newer `futures`,
            // but we did not updated vendored `futures` yet.
            let mut ctx = ctx.clone();
            #[allow(deprecated)]
            futs.push(async move {
                let result = ctx.get_providers(&providers_label).await;
                (providers_label, result)
            });
        }
    }

    let mut stdout = stdout.as_writer();
    let mut stderr = server_ctx.stderr()?;
    let recursive = !command.shallow;
    let json_format = command.json;
    let mut subtargets_map = serde_json::Map::new();

    let mut at_least_one_evaluation_error = false;
    while let Some((target, result)) = futs.next().await {
        match result {
            Ok(v) => {
                if recursive {
                    if json_format {
                        fn serialize_nested_subtargets(
                            providers: &FrozenProviderCollection,
                        ) -> serde_json::Value {
                            let mut entries = serde_json::Map::new();
                            for (subtarget, providers) in
                                providers.default_info().sub_targets().iter()
                            {
                                entries.insert(
                                    subtarget.to_string(),
                                    serialize_nested_subtargets(providers),
                                );
                            }
                            serde_json::Value::Object(entries)
                        }
                        subtargets_map.insert(
                            target.to_string(),
                            serialize_nested_subtargets(
                                v.require_compatible()?.provider_collection(),
                            ),
                        );
                    } else {
                        fn recursive_iterate(
                            providers: &FrozenProviderCollection,
                            stdout: &mut StdoutPartialOutput,
                            label: &mut Subtarget,
                        ) -> anyhow::Result<()> {
                            for (subtarget, providers) in
                                providers.default_info().sub_targets().iter()
                            {
                                label.push(subtarget.to_string());
                                writeln!(stdout, "{}", label)?;
                                recursive_iterate(providers, stdout, label)?;
                                label.pop();
                            }
                            Ok(())
                        }
                        recursive_iterate(
                            v.require_compatible()?.provider_collection(),
                            &mut stdout,
                            &mut Subtarget::new(target),
                        )?
                    }
                } else {
                    let mut label = Subtarget::new(target);
                    for sub in v
                        .require_compatible()?
                        .provider_collection()
                        .default_info()
                        .sub_targets()
                        .keys()
                    {
                        label.push(sub.to_string());
                        writeln!(&mut stdout, "{}", label)?;
                        label.pop();
                    }
                }
            }
            Err(e) => {
                write!(
                    &mut stderr,
                    "{}: failed:\n{}",
                    target,
                    indent("  ", &format!("{:?}", e))
                )?;
                at_least_one_evaluation_error = true;
            }
        }
    }

    if json_format {
        write!(
            &mut stdout,
            "{}",
            serde_json::to_string_pretty(&subtargets_map)?
        )?;
    }

    stdout.flush()?;
    stderr.flush()?;

    if at_least_one_evaluation_error {
        Err(anyhow::anyhow!(
            "Evaluation of at least one target provider failed"
        ))
    } else {
        Ok(())
    }
}

struct Subtarget {
    target: ConfiguredProvidersLabel,
    subtargets: Vec<String>,
}

impl Subtarget {
    fn new(target: ConfiguredProvidersLabel) -> Self {
        Self {
            target,
            subtargets: Vec::new(),
        }
    }

    fn push(&mut self, subtarget: String) {
        self.subtargets.push(subtarget);
    }

    fn pop(&mut self) {
        self.subtargets.pop();
    }
}

impl Display for Subtarget {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let subtargets = self
            .subtargets
            .iter()
            .map(|s| format!("[{}]", s))
            .collect::<Vec<_>>()
            .join("");
        write!(
            f,
            "{}{} ({})",
            self.target.unconfigured(),
            subtargets,
            self.target.cfg()
        )?;
        Ok(())
    }
}
