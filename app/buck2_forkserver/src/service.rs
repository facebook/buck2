/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::ffi::OsStr;
use std::fs::File;
use std::fs::OpenOptions;
use std::io::Write;
use std::os::unix::ffi::OsStrExt;
use std::os::unix::fs::OpenOptionsExt;
use std::path::Path;
use std::path::PathBuf;
use std::pin::Pin;
use std::sync::Arc;

use buck2_common::convert::ProstDurationExt;
use buck2_core::fs::fs_util;
use buck2_core::fs::paths::abs_norm_path::AbsNormPath;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::paths::abs_path::AbsPath;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePath;
use buck2_core::logging::LogConfigurationReloadHandle;
use buck2_error::BuckErrorContext;
use buck2_execute_local::DefaultKillProcess;
use buck2_execute_local::GatherOutputStatus;
use buck2_execute_local::maybe_absolutize_exe;
use buck2_execute_local::process_group::ProcessCommand;
use buck2_execute_local::spawn_command_and_stream_events;
use buck2_execute_local::status_decoder::DefaultStatusDecoder;
use buck2_execute_local::status_decoder::MiniperfStatusDecoder;
use buck2_execute_local::timeout_into_cancellation;
use buck2_forkserver_proto::CommandRequest;
use buck2_forkserver_proto::RequestEvent;
use buck2_forkserver_proto::SetLogFilterRequest;
use buck2_forkserver_proto::SetLogFilterResponse;
use buck2_forkserver_proto::command_request::StdRedirectPaths;
use buck2_forkserver_proto::forkserver_server::Forkserver;
use buck2_grpc::to_tonic;
use buck2_resource_control::cgroup::CgroupMinimal;
use buck2_resource_control::path::CgroupPathBuf;
use buck2_util::process::background_command;
use futures::future::FutureExt;
use futures::future::select;
use futures::stream::Stream;
use futures::stream::StreamExt;
use rand::distributions::Alphanumeric;
use rand::distributions::DistString;
use tonic::Request;
use tonic::Response;
use tonic::Status;
use tonic::Streaming;

use crate::convert::encode_event_stream;

// Not quite BoxStream: it has to be Sync (...)
type RunStream =
    Pin<Box<dyn Stream<Item = Result<buck2_forkserver_proto::CommandEvent, Status>> + Send>>;

struct ValidatedCommand {
    exe: PathBuf,
    argv: Vec<Vec<u8>>,
    env: Vec<buck2_forkserver_proto::EnvDirective>,
    cwd: PathBuf,
    timeout: Option<std::time::Duration>,
    enable_miniperf: bool,
    std_redirects: Option<StdRedirectPaths>,
    graceful_shutdown_timeout_s: Option<u32>,
    command_cgroup: Option<CgroupPathBuf>,
}

impl ValidatedCommand {
    fn try_from(cmd_request: CommandRequest) -> buck2_error::Result<Self> {
        let CommandRequest {
            exe,
            argv,
            env,
            cwd,
            timeout,
            enable_miniperf,
            std_redirects,
            graceful_shutdown_timeout_s,
            command_cgroup,
        } = cmd_request;

        let exe = OsStr::from_bytes(&exe);
        let cwd = OsStr::from_bytes(&cwd.as_ref().buck_error_context("Missing cwd")?.path);
        let cwd = AbsPath::new(Path::new(cwd)).buck_error_context("Invalid cwd")?;

        let timeout = timeout
            .map(|t| t.try_into_duration())
            .transpose()
            .buck_error_context("Invalid timeout")?;

        let exe = maybe_absolutize_exe(exe, cwd)?;

        let command_cgroup = command_cgroup.map(|cg| {
            CgroupPathBuf::new(AbsNormPathBuf::new(cg.into()).expect("Set correctly by caller"))
        });

        Ok(ValidatedCommand {
            exe: exe.into_owned(),
            argv,
            env,
            cwd: cwd.to_path_buf(),
            timeout,
            enable_miniperf,
            std_redirects,
            graceful_shutdown_timeout_s,
            command_cgroup,
        })
    }
}

pub(crate) struct UnixForkserverService {
    log_reload_handle: Arc<dyn LogConfigurationReloadHandle>,

    /// State for Miniperf.
    miniperf: Option<MiniperfContainer>,
}

impl UnixForkserverService {
    pub(crate) fn new(
        log_reload_handle: Arc<dyn LogConfigurationReloadHandle>,
        state_dir: &AbsNormPath,
    ) -> buck2_error::Result<Self> {
        let miniperf = MiniperfContainer::new(state_dir)?;
        Ok(Self {
            log_reload_handle,
            miniperf,
        })
    }

    async fn parse_command_request(
        stream: &mut Streaming<RequestEvent>,
    ) -> buck2_error::Result<CommandRequest> {
        let cmd_request = stream
            .message()
            .await?
            .and_then(|m| m.data)
            .and_then(|m| m.into_command_request())
            .buck_error_context("RequestEvent was not a CommandRequest!")?;
        Ok(cmd_request)
    }

    fn configure_environment(
        cmd: &mut std::process::Command,
        env_directives: &[buck2_forkserver_proto::EnvDirective],
    ) -> buck2_error::Result<()> {
        use buck2_forkserver_proto::env_directive::Data;

        for directive in env_directives {
            match directive
                .data
                .as_ref()
                .buck_error_context("EnvDirective is missing data")?
            {
                Data::Clear(..) => {
                    cmd.env_clear();
                }
                Data::Set(var) => {
                    cmd.env(OsStr::from_bytes(&var.key), OsStr::from_bytes(&var.value));
                }
                Data::Remove(var) => {
                    cmd.env_remove(OsStr::from_bytes(&var.key));
                }
            }
        }
        Ok(())
    }

    fn setup_process_command(
        &self,
        validated_cmd: &ValidatedCommand,
    ) -> buck2_error::Result<(ProcessCommand, Option<AbsNormPathBuf>)> {
        let (mut cmd, miniperf_output) = match (validated_cmd.enable_miniperf, &self.miniperf) {
            // Wraps the user command with miniperf for performance monitoring
            (true, Some(miniperf)) => {
                let mut cmd = background_command(miniperf.miniperf.as_path());
                let output_path = miniperf.allocate_output_path();
                cmd.arg(output_path.as_path());
                cmd.arg(&validated_cmd.exe);
                (cmd, Some(output_path))
            }
            // Direct execution of the command
            _ => (background_command(&validated_cmd.exe), None),
        };

        if let Some(cgroup_path) = validated_cmd.command_cgroup.clone() {
            let cgroup = CgroupMinimal::try_from_path(cgroup_path.clone())?;
            cgroup.setup_command(&mut cmd)?;
        }

        cmd.current_dir(&validated_cmd.cwd);
        cmd.args(validated_cmd.argv.iter().map(|a| OsStr::from_bytes(a)));

        Self::configure_environment(&mut cmd, &validated_cmd.env)?;

        // Some actions clear env and don't pass XDG_RUNTIME_DIR
        // This env var is required for systemd-run,
        // without passing it systemd returns "Failed to connect to bus: No medium found"
        #[cfg(fbcode_build)]
        if let Ok(value) = std::env::var("XDG_RUNTIME_DIR") {
            cmd.env("XDG_RUNTIME_DIR", value);
        }

        let mut cmd = ProcessCommand::new(cmd);
        if let Some(std_redirects) = &validated_cmd.std_redirects {
            cmd.stdout(File::create(OsStr::from_bytes(&std_redirects.stdout))?);
            cmd.stderr(File::create(OsStr::from_bytes(&std_redirects.stderr))?);
        }

        // cmd: ready-to-spawn process command
        // miniperf_output: path to miniperf output file (if monitoring)
        Ok((cmd, miniperf_output))
    }

    async fn create_command_stream(
        cmd: ProcessCommand,
        cancellation: impl futures::Future<Output = buck2_error::Result<GatherOutputStatus>>
        + Send
        + 'static,
        miniperf_output: Option<AbsNormPathBuf>,
        graceful_shutdown_timeout_s: Option<u32>,
        stream_stdio: bool,
    ) -> buck2_error::Result<RunStream> {
        let stream = match miniperf_output {
            Some(out) => spawn_command_and_stream_events(
                cmd,
                cancellation,
                MiniperfStatusDecoder::new(out),
                DefaultKillProcess {
                    graceful_shutdown_timeout_s,
                },
                stream_stdio,
                false,
            )
            .await?
            .left_stream(),
            None => spawn_command_and_stream_events(
                cmd,
                cancellation,
                DefaultStatusDecoder,
                DefaultKillProcess {
                    graceful_shutdown_timeout_s,
                },
                stream_stdio,
                false,
            )
            .await?
            .right_stream(),
        };

        let stream = encode_event_stream(stream);
        Ok(Box::pin(stream) as _)
    }
}

#[async_trait::async_trait]
impl Forkserver for UnixForkserverService {
    type RunStream = RunStream;

    async fn run(
        &self,
        req: Request<Streaming<RequestEvent>>,
    ) -> Result<Response<Self::RunStream>, Status> {
        to_tonic(async move {
            let mut stream = req.into_inner();

            let cmd_request = Self::parse_command_request(&mut stream).await?;
            let validated_cmd = ValidatedCommand::try_from(cmd_request)?;

            let cancel = async move {
                stream
                    .message()
                    .await?
                    .and_then(|m| m.data)
                    .and_then(|m| m.into_cancel_request())
                    .buck_error_context("RequestEvent was not a CancelRequest!")?;

                Ok(GatherOutputStatus::Cancelled)
            };

            let (cmd, miniperf_output) = self.setup_process_command(&validated_cmd)?;

            let timeout = timeout_into_cancellation(validated_cmd.timeout);
            let cancellation = select(timeout.boxed(), cancel.boxed()).map(|r| r.factor_first().0);

            let stream_stdio = validated_cmd.std_redirects.is_none();

            let stream = Self::create_command_stream(
                cmd,
                cancellation,
                miniperf_output,
                validated_cmd.graceful_shutdown_timeout_s,
                stream_stdio,
            )
            .await?;

            Ok(stream)
        })
        .await
    }

    async fn set_log_filter(
        &self,
        req: Request<SetLogFilterRequest>,
    ) -> Result<Response<SetLogFilterResponse>, Status> {
        self.log_reload_handle
            .update_log_filter(&req.get_ref().log_filter)
            .buck_error_context("Error updating forkserver filter")
            .map_err(|e| Status::invalid_argument(format!("{e:#}")))?;

        Ok(Response::new(SetLogFilterResponse {}))
    }
}

struct MiniperfContainer {
    /// The Miniperf binary
    miniperf: AbsNormPathBuf,

    /// The directory where Miniperf outputs go.
    output_dir: AbsNormPathBuf,
}

impl MiniperfContainer {
    fn new(forkserver_state_dir: &AbsNormPath) -> buck2_error::Result<Option<Self>> {
        let miniperf_bin: Option<&'static [u8]>;

        #[cfg(all(fbcode_build, target_os = "linux"))]
        {
            miniperf_bin = Some(buck2_miniperf_data::get());
        }

        #[cfg(not(all(fbcode_build, target_os = "linux")))]
        {
            miniperf_bin = None;
        }

        let miniperf_bin = match miniperf_bin {
            Some(m) => m,
            None => return Ok(None),
        };

        let miniperf = forkserver_state_dir.join(ForwardRelativePath::unchecked_new("miniperf"));
        let output_dir = forkserver_state_dir.join(ForwardRelativePath::unchecked_new("out"));

        fs_util::remove_all(&miniperf)?;
        fs_util::remove_all(&output_dir)?;
        fs_util::create_dir_all(&output_dir)?;

        let mut opts = OpenOptions::new();
        opts.create_new(true);
        opts.write(true);

        opts.mode(0o755);

        let mut miniperf_writer = opts
            .open(miniperf.as_path())
            .with_buck_error_context(|| format!("Error opening: `{}`", miniperf.display()))?;

        miniperf_writer
            .write_all(miniperf_bin)
            .and_then(|()| miniperf_writer.flush())
            .with_buck_error_context(|| {
                format!("Error writing miniperf to `{}`", miniperf.display())
            })?;

        Ok(Some(Self {
            miniperf,
            output_dir,
        }))
    }

    fn allocate_output_path(&self) -> AbsNormPathBuf {
        let name = Alphanumeric.sample_string(&mut rand::thread_rng(), 16);
        self.output_dir
            .join(ForwardRelativePath::unchecked_new(&name))
    }
}
