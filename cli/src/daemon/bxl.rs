use cli_proto::{BuildTarget, BxlRequest};

use crate::daemon::server::ServerCommandContext;

#[derive(Debug)]
pub struct BxlResult {
    pub built: Vec<BuildTarget>,
    pub serialized_build_report: Option<String>,
    pub error_messages: Vec<String>,
}

pub async fn bxl(
    _server_ctx: ServerCommandContext,
    _request: BxlRequest,
) -> anyhow::Result<BxlResult> {
    unimplemented!("TODO(bobyf) T116849868")
}
