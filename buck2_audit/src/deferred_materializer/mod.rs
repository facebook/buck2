use std::io::Write;

use anyhow::Context;
use async_trait::async_trait;
use buck2_execute::materialize::materializer::HasMaterializer;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::ctx::ServerCommandDiceContext;
use cli_proto::ClientContext;
use futures::stream::StreamExt;

use crate::AuditCommandCommonOptions;
use crate::AuditSubcommand;

#[derive(Debug, clap::Parser, serde::Serialize, serde::Deserialize)]
#[clap(
    name = "deferred-materializer",
    about = "Access and interact with the deferred materializer"
)]
pub struct DeferredMaterializerCommand {
    #[clap(flatten)]
    common_opts: AuditCommandCommonOptions,

    #[clap(subcommand)]
    subcommand: DeferredMaterializerSubcommand,
}

#[derive(Debug, clap::Subcommand, serde::Serialize, serde::Deserialize)]
enum DeferredMaterializerSubcommand {
    List,
}

#[async_trait]
impl AuditSubcommand for DeferredMaterializerCommand {
    async fn server_execute(
        &self,
        server_ctx: Box<dyn ServerCommandContextTrait>,
        _client_ctx: ClientContext,
    ) -> anyhow::Result<()> {
        match self.subcommand {
            DeferredMaterializerSubcommand::List => {
                server_ctx
                    .with_dice_ctx(move |mut server_ctx, dice| async move {
                        let materializer = dice.per_transaction_data().get_materializer();

                        let deferred_materializer = materializer
                            .as_deferred_materializer_extension()
                            .context("Deferred materializer is not in use")?;

                        let mut stream = deferred_materializer
                            .iterate()
                            .context("Failed to start iterating")?;

                        let mut stdout = server_ctx.stdout()?;

                        while let Some(path) = stream.next().await {
                            writeln!(stdout, "{}", path)?;
                        }

                        anyhow::Ok(())
                    })
                    .await
            }
        }
    }

    fn common_opts(&self) -> &AuditCommandCommonOptions {
        &self.common_opts
    }
}
