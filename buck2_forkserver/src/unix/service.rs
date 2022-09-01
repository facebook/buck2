use std::ffi::OsStr;
use std::os::unix::ffi::OsStrExt;
use std::pin::Pin;
use std::process::Stdio;

use anyhow::Context as _;
use buck2_common::convert::ProstDurationExt;
use buck2_core::process::async_background_command;
use buck2_grpc::to_tonic;
use forkserver_proto::forkserver_server::Forkserver;
use forkserver_proto::CommandRequest;
use futures::stream::Stream;
use tonic::Request;
use tonic::Response;
use tonic::Status;

use crate::convert::encode_event_stream;
use crate::run::stream_command_events;

// Not quite BoxStream: it has to be Sync (...)
type RunStream =
    Pin<Box<dyn Stream<Item = Result<forkserver_proto::CommandEvent, Status>> + Send + Sync>>;

pub struct UnixForkserverService;

#[async_trait::async_trait]
impl Forkserver for UnixForkserverService {
    type RunStream = RunStream;

    async fn run(&self, req: Request<CommandRequest>) -> Result<Response<Self::RunStream>, Status> {
        to_tonic(async move {
            let CommandRequest {
                exe,
                argv,
                env,
                env_clear,
                cwd,
                timeout,
            } = req.into_inner();

            let exe = OsStr::from_bytes(&exe);
            let cwd = cwd.as_ref().map(|c| OsStr::from_bytes(&c.path));
            let argv = argv.iter().map(|a| OsStr::from_bytes(a));
            let timeout = timeout
                .map(|t| t.try_into_duration())
                .transpose()
                .context("Invalid timeout")?;

            let mut cmd = async_background_command(exe);
            if let Some(cwd) = cwd {
                cmd.current_dir(cwd);
            }
            cmd.args(argv);

            if env_clear {
                cmd.env_clear();
            }

            for var in env {
                cmd.env(OsStr::from_bytes(&var.key), OsStr::from_bytes(&var.value));
            }

            cmd.stdin(Stdio::null())
                .stdout(Stdio::piped())
                .stderr(Stdio::piped());

            let child = cmd.spawn().context("Spawn failed")?;

            let stream = stream_command_events(child, timeout)?;
            let stream = encode_event_stream(stream);
            Ok(Box::pin(stream) as _)
        })
        .await
    }
}
