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
use buck2_audit::package_values::PackageValuesCommand;
use buck2_common::dice::cells::HasCellResolver;
use buck2_core::package::PackageLabel;
use buck2_core::pattern::parse_package::parse_package;
use buck2_events::dispatch::console_message;
use buck2_node::metadata::key::MetadataKey;
use buck2_node::package_values_calculation::PACKAGE_VALUES_CALCULATION;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::ctx::ServerCommandDiceContext;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;
use dupe::Dupe;
use futures::FutureExt;
use gazebo::prelude::SliceExt;
use starlark_map::small_map::SmallMap;

use crate::AuditSubcommand;

#[async_trait]
impl AuditSubcommand for PackageValuesCommand {
    async fn server_execute(
        &self,
        server_ctx: &dyn ServerCommandContextTrait,
        mut stdout: PartialResultDispatcher<buck2_cli_proto::StdoutBytes>,
        _client_server_ctx: buck2_cli_proto::ClientContext,
    ) -> anyhow::Result<()> {
        if self.packages.is_empty() {
            console_message("No packages specified".to_owned());
        }

        server_ctx
            .with_dice_ctx(async move |server_ctx, mut dice_ctx| {
                let cell_resolver = dice_ctx.get_cell_resolver().await?;
                let cell_alias_resolver =
                    cell_resolver.get_cwd_cell_alias_resolver(server_ctx.working_dir())?;

                let packages = self
                    .packages
                    .try_map(|package| parse_package(package.dupe(), cell_alias_resolver))?;

                let package_values_by_package = dice_ctx
                    .try_compute_join(packages, |ctx, package| {
                        async move {
                            let package_values = PACKAGE_VALUES_CALCULATION
                                .get()?
                                .package_values(ctx, package.dupe())
                                .await?;
                            anyhow::Ok((package, package_values))
                        }
                        .boxed()
                    })
                    .await?;
                let package_values_by_package: SmallMap<
                    PackageLabel,
                    SmallMap<MetadataKey, serde_json::Value>,
                > = package_values_by_package.into_iter().collect();

                let mut stdout = stdout.as_writer();
                serde_json::to_writer_pretty(&mut stdout, &package_values_by_package)?;
                // Because serde does not write a trailing newline.
                writeln!(stdout)?;
                Ok(())
            })
            .await
    }
}
