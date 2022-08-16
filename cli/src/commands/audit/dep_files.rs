/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Cow;
use std::io::Write;

use anyhow::Context as _;
use async_trait::async_trait;
use buck2_build_api::actions::run::dep_files::get_dep_files;
use buck2_build_api::actions::run::dep_files::DepFilesKey;
use buck2_build_api::actions::run::dep_files::StoredFingerprints;
use buck2_build_api::calculation::Calculation;
use buck2_build_api::deferred::BaseDeferredKey;
use buck2_build_api::execute::materializer::HasMaterializer;
use buck2_common::dice::cells::HasCellResolver;
use buck2_core::category::Category;
use buck2_core::directory::Directory;
use buck2_core::directory::DirectoryIterator;
use buck2_core::pattern::TargetPattern;
use cli_proto::ClientContext;

use crate::commands::audit::AuditSubcommand;
use crate::commands::common::CommonBuildConfigurationOptions;
use crate::commands::common::CommonConsoleOptions;
use crate::commands::common::CommonDaemonCommandOptions;
use crate::daemon::common::parse_patterns_from_cli_args;
use crate::daemon::common::target_platform_from_client_context;
use crate::daemon::server::ctx::ServerCommandContext;

#[derive(Debug, clap::Parser, serde::Serialize, serde::Deserialize)]
#[clap(
    name = "audit-dep-files",
    about = "prints out the select files for a command"
)]
pub(crate) struct AuditDepFilesCommand {
    #[clap(flatten)]
    pub config_opts: CommonBuildConfigurationOptions,

    #[clap(flatten)]
    console_opts: CommonConsoleOptions,

    #[clap(flatten)]
    event_log_opts: CommonDaemonCommandOptions,

    #[clap(help = "Target to query dep files for")]
    pattern: String,

    #[clap(help = "Action category")]
    category: String,

    #[clap(help = "Action identifier")]
    identifier: Option<String>,
}

#[async_trait]
impl AuditSubcommand for AuditDepFilesCommand {
    async fn server_execute(
        &self,
        mut server_ctx: ServerCommandContext,
        client_ctx: ClientContext,
    ) -> anyhow::Result<()> {
        let ctx = server_ctx.dice_ctx().await?;
        let cells = ctx.get_cell_resolver().await?;

        let target_platform =
            target_platform_from_client_context(Some(&client_ctx), &cells, &server_ctx.working_dir)
                .await?;

        let label = parse_patterns_from_cli_args::<TargetPattern>(
            &[buck2_data::TargetPattern {
                value: self.pattern.clone(),
            }],
            &ctx,
            &server_ctx.working_dir,
        )
        .await?
        .into_iter()
        .next()
        .context("Parsing patterns returned nothing")?
        .as_target_label(&self.pattern)?;

        let label = ctx
            .get_configured_target(&label, target_platform.as_ref())
            .await?;

        let category = Category::try_from(self.category.as_str())?;

        let key = DepFilesKey::new(
            BaseDeferredKey::TargetLabel(label),
            category,
            self.identifier.clone(),
        );

        let state = get_dep_files(&key).context("Failed to find dep files")?;

        let dep_files = state
            .read_dep_files(
                &ctx.get_artifact_fs().await?,
                ctx.per_transaction_data().get_materializer().as_ref(),
            )
            .await
            .context("Failed to read dep files")?
            .context("Dep fils have expired")?;

        let fingerprints = state.locked_compute_fingerprints(Cow::Owned(dep_files), true);

        let dirs = match &*fingerprints {
            StoredFingerprints::Digests(..) => {
                // This is bit awkward but this only for testing right now so that's OK
                return Err(anyhow::anyhow!("Fingerprints were stored as digests!"));
            }
            StoredFingerprints::Dirs(dirs) => dirs,
        };

        let mut stdout = server_ctx.stdout()?;

        for (path, ..) in dirs
            .untagged
            .ordered_walk()
            .with_paths()
            .filter_map(|(p, e)| Some((p, e.into_leaf()?)))
        {
            writeln!(stdout, "untagged\t{}", path)?;
        }

        for (tag, dir) in dirs.tagged.iter() {
            for (path, ..) in dir
                .ordered_walk()
                .with_paths()
                .filter_map(|(p, e)| Some((p, e.into_leaf()?)))
            {
                writeln!(stdout, "{}\t{}", tag, path)?;
            }
        }

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
