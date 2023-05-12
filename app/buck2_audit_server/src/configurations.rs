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
use buck2_audit::configurations::AuditConfigurationsCommand;
use buck2_cli_proto::ClientContext;
use buck2_core::configuration::bound_id::BoundConfigurationId;
use buck2_core::configuration::data::ConfigurationData;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;
use itertools::Itertools;

use crate::AuditSubcommand;

#[async_trait]
impl AuditSubcommand for AuditConfigurationsCommand {
    async fn server_execute(
        &self,
        _server_ctx: &dyn ServerCommandContextTrait,
        mut stdout: PartialResultDispatcher<buck2_cli_proto::StdoutBytes>,
        _client_ctx: ClientContext,
    ) -> anyhow::Result<()> {
        let mut stdout = stdout.as_writer();

        if self.configs.is_empty() {
            for cfg in ConfigurationData::iter_existing()
                .filter(|c| c.is_bound())
                .sorted_by_cached_key(|c| c.full_name().to_owned())
            {
                print_cfg(&mut stdout, &cfg)?;
            }
        } else {
            for cfg in &self.configs {
                let cfg = BoundConfigurationId::parse(cfg)?;
                let cfg = ConfigurationData::lookup_bound(cfg)?;
                print_cfg(&mut stdout, &cfg)?;
            }
        }

        Ok(())
    }
}

fn print_cfg(stdout: &mut impl Write, cfg: &ConfigurationData) -> anyhow::Result<()> {
    writeln!(stdout, "{}:", cfg.full_name())?;
    let data = cfg.data()?;
    for (constraint_key, constraint_value) in data
        .constraints
        .iter()
        .sorted_by(|(k1, v1), (k2, v2)| (v1, k1).cmp(&(v2, k2)))
    {
        // to_string() on the value is required to get the format string behavior.
        writeln!(
            stdout,
            "  {:<59} ({})",
            constraint_value.to_string(),
            constraint_key
        )?;
    }

    Ok(())
}
