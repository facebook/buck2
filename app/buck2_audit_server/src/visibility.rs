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
use buck2_common::pattern::parse_from_cli::parse_patterns_from_cli_args;
use buck2_core::pattern::pattern_type::TargetPatternExtra;
use buck2_node::load_patterns::load_patterns;
use buck2_node::load_patterns::MissingTargetBehavior;
use buck2_node::nodes::lookup::TargetNodeLookup;
use buck2_node::nodes::unconfigured::TargetNode;
use buck2_node::visibility::VisibilityError;
use buck2_query::query::environment::QueryTargetDepsSuccessors;
use buck2_query::query::syntax::simple::eval::set::TargetSet;
use buck2_query::query::traversal::async_depth_first_postorder_traversal;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::ctx::ServerCommandDiceContext;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;
use dice::DiceTransaction;
use dupe::Dupe;

use crate::ServerAuditSubcommand;

#[derive(buck2_error::Error, Debug)]
#[buck2(tag = Tier0)]
enum VisibilityCommandError {
    #[error(
        "Internal Error: The dependency `{0}` of the target `{1}` was not found during the traversal."
    )]
    DepNodeNotFound(String, String),
}

async fn verify_visibility(
    mut ctx: DiceTransaction,
    targets: TargetSet<TargetNode>,
) -> buck2_error::Result<()> {
    let mut new_targets: TargetSet<TargetNode> = TargetSet::new();

    let visit = |target| {
        new_targets.insert(target);
        Ok(())
    };

    ctx.with_linear_recompute(|ctx| async move {
        let lookup = TargetNodeLookup(&ctx);

        async_depth_first_postorder_traversal(
            &lookup,
            targets.iter_names(),
            QueryTargetDepsSuccessors,
            visit,
        )
        .await
    })
    .await?;

    let mut visibility_errors = Vec::new();

    for target in new_targets.iter() {
        for dep in target.deps() {
            match new_targets.get(dep) {
                Some(val) => {
                    if !val.is_visible_to(target.label())? {
                        visibility_errors.push(VisibilityError::NotVisibleTo(
                            dep.dupe(),
                            target.label().dupe(),
                        ));
                    }
                }
                None => {
                    return Err(buck2_error::Error::from(
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
        return Err(buck2_error::buck2_error!(
            buck2_error::ErrorTag::Input,
            "{}",
            1
        ));
    }

    buck2_client_ctx::eprintln!("audit visibility succeeded")?;
    Ok(())
}

#[async_trait]
impl ServerAuditSubcommand for AuditVisibilityCommand {
    async fn server_execute(
        &self,
        server_ctx: &dyn ServerCommandContextTrait,
        _stdout: PartialResultDispatcher<buck2_cli_proto::StdoutBytes>,
        _client_ctx: ClientContext,
    ) -> buck2_error::Result<()> {
        Ok(server_ctx
            .with_dice_ctx(|server_ctx, mut ctx| async move {
                let parsed_patterns = parse_patterns_from_cli_args::<TargetPatternExtra>(
                    &mut ctx,
                    &self.patterns,
                    server_ctx.working_dir(),
                )
                .await?;

                let parsed_target_patterns =
                    load_patterns(&mut ctx, parsed_patterns, MissingTargetBehavior::Fail).await?;

                let mut nodes = TargetSet::<TargetNode>::new();
                for (_package, result) in parsed_target_patterns.iter() {
                    let res = result.as_ref().map_err(Dupe::dupe)?;
                    nodes.extend(res.values().map(|n| n.to_owned()));
                }

                verify_visibility(ctx, nodes).await?;
                Ok(())
            })
            .await?)
    }
}
