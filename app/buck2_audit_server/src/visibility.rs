/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use buck2_audit::visibility::AuditVisibilityCommand;
use buck2_cli_proto::ClientContext;
use buck2_core::pattern::pattern_type::TargetPatternExtra;
use buck2_node::load_patterns::load_patterns;
use buck2_node::load_patterns::MissingTargetBehavior;
use buck2_node::nodes::lookup::TargetNodeLookup;
use buck2_node::nodes::unconfigured::TargetNode;
use buck2_node::visibility::VisibilityError;
use buck2_query::query::syntax::simple::eval::set::TargetSet;
use buck2_query::query::traversal::async_depth_first_postorder_traversal;
use buck2_query::query::traversal::AsyncTraversalDelegate;
use buck2_query::query::traversal::ChildVisitor;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::ctx::ServerCommandDiceContext;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;
use buck2_server_ctx::pattern::parse_patterns_from_cli_args;
use dice::DiceTransaction;
use dupe::Dupe;
use gazebo::prelude::SliceExt;

use crate::AuditSubcommand;

#[derive(buck2_error::Error, Debug)]
enum VisibilityCommandError {
    #[error(
        "Internal Error: The dependency `{0}` of the target `{1}` was not found during the traversal."
    )]
    DepNodeNotFound(String, String),
}

async fn verify_visibility(
    ctx: DiceTransaction,
    targets: TargetSet<TargetNode>,
) -> anyhow::Result<()> {
    struct Delegate {
        targets: TargetSet<TargetNode>,
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
            func: &mut impl ChildVisitor<TargetNode>,
        ) -> anyhow::Result<()> {
            for dep in target.deps() {
                func.visit(dep.dupe())?;
            }
            Ok(())
        }
    }

    let lookup = TargetNodeLookup(&ctx);

    let mut delegate = Delegate {
        targets: TargetSet::<TargetNode>::new(),
    };

    async_depth_first_postorder_traversal(&lookup, targets.iter_names(), &mut delegate).await?;

    let mut visibility_errors = Vec::new();

    for target in delegate.targets.iter() {
        for dep in target.deps() {
            match delegate.targets.get(dep) {
                Some(val) => {
                    if !val.is_visible_to(target.label())? {
                        visibility_errors.push(VisibilityError::NotVisibleTo(
                            dep.dupe(),
                            target.label().dupe(),
                        ));
                    }
                }
                None => {
                    return Err(anyhow::Error::from(
                        VisibilityCommandError::DepNodeNotFound(
                            dep.to_string(),
                            target.label().name().to_string(),
                        ),
                    ));
                }
            }
        }
    }

    for err in &visibility_errors {
        buck2_client_ctx::eprintln!("{}", err)?;
    }

    if !visibility_errors.is_empty() {
        return Err(anyhow::anyhow!("{}", 1));
    }

    buck2_client_ctx::eprintln!("audit visibility succeeded")?;
    Ok(())
}

#[async_trait]
impl AuditSubcommand for AuditVisibilityCommand {
    async fn server_execute(
        &self,
        server_ctx: &dyn ServerCommandContextTrait,
        _stdout: PartialResultDispatcher<buck2_cli_proto::StdoutBytes>,
        _client_ctx: ClientContext,
    ) -> anyhow::Result<()> {
        server_ctx
            .with_dice_ctx(async move |server_ctx, mut ctx| {
                let parsed_patterns = parse_patterns_from_cli_args::<TargetPatternExtra>(
                    &mut ctx,
                    &self
                        .patterns
                        .map(|pat| buck2_data::TargetPattern { value: pat.clone() }),
                    server_ctx.working_dir(),
                )
                .await?;

                let parsed_target_patterns =
                    load_patterns(&ctx, parsed_patterns, MissingTargetBehavior::Fail).await?;

                let mut nodes = TargetSet::<TargetNode>::new();
                for (_package, result) in parsed_target_patterns.iter() {
                    let res = result.as_ref().map_err(Dupe::dupe)?;
                    nodes.extend(res.values());
                }

                verify_visibility(ctx, nodes).await?;
                Ok(())
            })
            .await
    }
}
