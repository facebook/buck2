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
use buck2_core::configuration::Configuration;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use cli_proto::ClientContext;
use itertools::Itertools;

use crate::AuditCommandCommonOptions;
use crate::AuditSubcommand;

#[derive(Debug, clap::Parser, serde::Serialize, serde::Deserialize)]
#[clap(
    name = "audit-configuration",
    about = "prints the constraints for configuration IDs"
)]
pub struct AuditConfigurationsCommand {
    #[clap(flatten)]
    common_opts: AuditCommandCommonOptions,

    #[clap(
        name = "configurations",
        multiple_values = true,
        help = "configurations to audit (example: `cell//package:target-105fe3389fc7e436`). If none provided, will print information about all known configurations."
    )]
    configs: Vec<String>,
}

#[async_trait]
impl AuditSubcommand for AuditConfigurationsCommand {
    async fn server_execute(
        &self,
        mut server_ctx: Box<dyn ServerCommandContextTrait>,
        _client_ctx: ClientContext,
    ) -> anyhow::Result<()> {
        let mut stdout = server_ctx.stdout()?;

        if self.configs.is_empty() {
            for cfg in Configuration::iter_existing()?
                .filter(|c| c.is_bound())
                .sorted_by_cached_key(|c| c.full_name().to_owned())
            {
                print_cfg(&mut stdout, &cfg)?;
            }
        } else {
            for cfg in &self.configs {
                let cfg = Configuration::lookup_from_string(cfg)?;
                print_cfg(&mut stdout, &cfg)?;
            }
        }

        Ok(())
    }

    fn common_opts(&self) -> &AuditCommandCommonOptions {
        &self.common_opts
    }
}

fn print_cfg(stdout: &mut impl Write, cfg: &Configuration) -> anyhow::Result<()> {
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

    for (config_key, config_value) in data.buckconfigs.iter().sorted() {
        writeln!(stdout, "  {}={}", config_key, config_value)?;
    }

    Ok(())
}
