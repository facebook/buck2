/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use anyhow::private::Err;
use async_trait::async_trait;
use buck2_build_api::calculation::load_patterns;
use buck2_build_api::calculation::Calculation;
use buck2_core::pattern::TargetPattern;
use buck2_core::result::SharedResult;
use buck2_core::result::ToUnsharedResultExt;
use buck2_core::target::TargetLabel;
use buck2_node::nodes::unconfigured::TargetNode;
use buck2_node::visibility::VisibilityError;
use buck2_query::query::syntax::simple::eval::set::TargetSet;
use buck2_query::query::traversal::async_depth_first_postorder_traversal;
use buck2_query::query::traversal::AsyncNodeLookup;
use buck2_query::query::traversal::AsyncTraversalDelegate;
use buck2_query::query::traversal::ChildVisitor;
use cli_proto::ClientContext;
use dice::DiceTransaction;
use gazebo::prelude::*;

use crate::commands::audit::AuditSubcommand;
use crate::commands::common::CommonBuildConfigurationOptions;
use crate::commands::common::CommonConsoleOptions;
use crate::commands::common::CommonDaemonCommandOptions;
use crate::daemon::common::parse_patterns_from_cli_args;
use crate::daemon::server::ServerCommandContext;

#[derive(thiserror::Error, Debug)]
enum VisibilityCommandError {
    #[error(
        "Internal Error: The dependency `{0}` of the target `{1}` was not found during the traversal."
    )]
    DepNodeNotFound(String, String),
}

#[derive(Debug, clap::Parser, serde::Serialize, serde::Deserialize)]
#[clap(
    name = "audit-visibility",
    about = "Verify the visibility for transitive deps of the specified target(s) on the unconfigured target graph"
)]
pub(crate) struct AuditVisibilityCommand {
    #[clap(flatten)]
    config_opts: CommonBuildConfigurationOptions,

    #[clap(flatten)]
    console_opts: CommonConsoleOptions,

    #[clap(flatten)]
    event_log_opts: CommonDaemonCommandOptions,

    #[clap(name = "TARGET_PATTERNS", help = "Target pattern(s) to analyze.")]
    patterns: Vec<String>,
}

impl AuditVisibilityCommand {
    async fn verify_visibility(
        ctx: DiceTransaction,
        targets: TargetSet<TargetNode>,
    ) -> anyhow::Result<()> {
        struct Lookup {
            ctx: DiceTransaction,
        }
        struct Delegate {
            targets: TargetSet<TargetNode>,
        }

        #[async_trait]
        impl AsyncNodeLookup<TargetNode> for Lookup {
            async fn get(&self, label: &TargetLabel) -> anyhow::Result<TargetNode> {
                Ok(self.ctx.get_target_node(label).await?)
            }
        }

        #[async_trait]
        impl AsyncTraversalDelegate<TargetNode> for Delegate {
            fn visit(&mut self, target: TargetNode) -> anyhow::Result<()> {
                self.targets.insert(target);
                Ok(())
            }
            async fn for_each_child(
                &mut self,
                target: &TargetNode,
                func: &mut dyn ChildVisitor<TargetNode>,
            ) -> anyhow::Result<()> {
                for dep in target.deps() {
                    func.visit(dep.dupe())?;
                }
                Ok(())
            }
        }

        let lookup = Lookup { ctx: ctx.dupe() };

        let mut delegate = Delegate {
            targets: TargetSet::<TargetNode>::new(),
        };

        async_depth_first_postorder_traversal(&lookup, targets.iter_names(), &mut delegate).await?;

        let mut visibility_errors = Vec::new();

        for target in delegate.targets.iter() {
            for dep in target.deps() {
                match delegate.targets.get(dep) {
                    Some(val) => {
                        if !val.is_visible_to(target.label()) {
                            visibility_errors.push(VisibilityError::NotVisibleTo(
                                dep.dupe(),
                                target.label().dupe(),
                            ));
                        }
                    }
                    None => {
                        return Err(anyhow::Error::new(VisibilityCommandError::DepNodeNotFound(
                            dep.to_string(),
                            target.label().name().to_string(),
                        )));
                    }
                }
            }
        }

        for err in &visibility_errors {
            crate::eprintln!("{}", err)?;
        }

        if !visibility_errors.is_empty() {
            return Err(anyhow::anyhow!("{}", 1));
        }

        crate::eprintln!("audit visibility succeeded")?;
        Ok(())
    }
}

#[async_trait]
impl AuditSubcommand for AuditVisibilityCommand {
    async fn server_execute(
        &self,
        server_ctx: ServerCommandContext,
        _client_ctx: ClientContext,
    ) -> anyhow::Result<()> {
        let ctx = server_ctx.dice_ctx().await?;

        let parsed_patterns = parse_patterns_from_cli_args::<TargetPattern>(
            &self
                .patterns
                .map(|pat| buck2_data::TargetPattern { value: pat.clone() }),
            &ctx,
            &server_ctx.working_dir,
        )
        .await?;

        let parsed_target_patterns = load_patterns(&ctx, parsed_patterns).await?;

        let mut nodes = TargetSet::<TargetNode>::new();
        for (_package, result) in parsed_target_patterns.iter() {
            match result {
                Ok(res) => {
                    for node in res.values() {
                        nodes.insert(node.dupe());
                    }
                }
                Err(e) => {
                    return SharedResult::unshared_error(Err(e.dupe()));
                }
            }
        }

        AuditVisibilityCommand::verify_visibility(ctx, nodes).await?;
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
