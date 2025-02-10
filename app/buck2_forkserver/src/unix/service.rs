/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::ffi::OsStr;
use std::fs::File;
use std::fs::OpenOptions;
use std::io::Write;
use std::os::unix::ffi::OsStrExt;
use std::path::Path;
use std::pin::Pin;
use std::sync::Arc;

use buck2_common::convert::ProstDurationExt;
use buck2_common::init::ResourceControlConfig;
use buck2_common::systemd::replace_unit_delimiter;
use buck2_common::systemd::ParentSlice;
use buck2_common::systemd::SystemdRunner;
use buck2_common::systemd::SystemdRunnerConfig;
use buck2_core::fs::fs_util;
use buck2_core::fs::paths::abs_norm_path::AbsNormPath;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::paths::abs_path::AbsPath;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePath;
use buck2_core::logging::LogConfigurationReloadHandle;
use buck2_error::BuckErrorContext;
use buck2_forkserver_proto::forkserver_server::Forkserver;
use buck2_forkserver_proto::CommandRequest;
use buck2_forkserver_proto::RequestEvent;
use buck2_forkserver_proto::SetLogFilterRequest;
use buck2_forkserver_proto::SetLogFilterResponse;
use buck2_grpc::to_tonic;
use buck2_util::process::background_command;
use futures::future::select;
use futures::future::FutureExt;
use futures::stream::Stream;
use futures::stream::StreamExt;
use rand::distributions::Alphanumeric;
use rand::distributions::DistString;
use tonic::Request;
use tonic::Response;
use tonic::Status;
use tonic::Streaming;

use crate::convert::encode_event_stream;
use crate::run::maybe_absolutize_exe;
use crate::run::process_group::ProcessCommand;
use crate::run::status_decoder::DefaultStatusDecoder;
use crate::run::status_decoder::MiniperfStatusDecoder;
use crate::run::stream_command_events;
use crate::run::timeout_into_cancellation;
use crate::run::DefaultKillProcess;
use crate::run::GatherOutputStatus;

// Not quite BoxStream: it has to be Sync (...)
type RunStream =
    Pin<Box<dyn Stream<Item = Result<buck2_forkserver_proto::CommandEvent, Status>> + Send>>;

pub struct UnixForkserverService {
    log_reload_handle: Arc<dyn LogConfigurationReloadHandle>,

    /// State for Miniperf.
    miniperf: Option<MiniperfContainer>,

    /// Systemd runner for resource control
    systemd_runner: Option<SystemdRunner>,
}

impl UnixForkserverService {
    pub fn new(
        log_reload_handle: Arc<dyn LogConfigurationReloadHandle>,
        state_dir: &AbsNormPath,
        resource_control: ResourceControlConfig,
    ) -> buck2_error::Result<Self> {
        let miniperf = MiniperfContainer::new(state_dir)?;
        let systemd_runner =
            SystemdRunner::create_if_enabled(&SystemdRunnerConfig::action_runner_config(
                &resource_control,
                // we want to create forkserver in the same hierarchy where buck-daemon scope
                // for this we inherit slice
                ParentSlice::Inherit("forkserver".to_owned()),
            ))?;
        Ok(Self {
            log_reload_handle,
            miniperf,
            systemd_runner,
        })
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

            let msg = stream
                .message()
                .await?
                .and_then(|m| m.data)
                .and_then(|m| m.into_command_request())
                .buck_error_context("RequestEvent was not a CommandRequest!")?;

            let cancel = async move {
                stream
                    .message()
                    .await?
                    .and_then(|m| m.data)
                    .and_then(|m| m.into_cancel_request())
                    .buck_error_context("RequestEvent was not a CancelRequest!")?;

                Ok(GatherOutputStatus::Cancelled)
            };

            let CommandRequest {
                exe,
                argv,
                env,
                cwd,
                timeout,
                enable_miniperf,
                std_redirects,
                graceful_shutdown_timeout_s,
                action_digest,
            } = msg;

            let exe = OsStr::from_bytes(&exe);
            let cwd = OsStr::from_bytes(&cwd.as_ref().buck_error_context("Missing cwd")?.path);
            let cwd = AbsPath::new(Path::new(cwd)).buck_error_context("Inalid cwd")?;

            let argv = argv.iter().map(|a| OsStr::from_bytes(a));
            let timeout = timeout
                .map(|t| t.try_into_duration())
                .transpose()
                .buck_error_context("Invalid timeout")?;

            let exe = maybe_absolutize_exe(exe, cwd)?;
            let systemd_context = self.systemd_runner.as_ref().zip(action_digest);

            let (mut cmd, miniperf_output) =
                match (enable_miniperf, &self.miniperf, &systemd_context) {
                    (true, Some(miniperf), None) => {
                        let mut cmd = background_command(miniperf.miniperf.as_path());
                        let output_path = miniperf.allocate_output_path();
                        cmd.arg(output_path.as_path());
                        cmd.arg(exe.as_ref());
                        (cmd, Some(output_path))
                    }
                    (_, Some(miniperf), Some((runner, action_digest))) => {
                        let mut cmd = runner.background_command_linux(
                            miniperf.miniperf.as_path(),
                            &replace_unit_delimiter(&action_digest),
                            &AbsNormPath::new(cwd)?,
                        );
                        let output_path = miniperf.allocate_output_path();
                        cmd.arg(output_path.as_path());
                        cmd.arg(exe.as_ref());
                        (cmd, Some(output_path))
                    }
                    _ => (background_command(exe.as_ref()), None),
                };

            cmd.current_dir(cwd);
            cmd.args(argv);

            {
                use buck2_forkserver_proto::env_directive::Data;

                for directive in env {
                    match directive
                        .data
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
            }

            // Some actions clear env and don't pass XDG_RUNTIME_DIR
            // This env var is required for systemd-run,
            // without passing it systemd returns "Failed to connect to bus: No medium found"
            #[cfg(fbcode_build)]
            if let Ok(value) = std::env::var("XDG_RUNTIME_DIR") {
                cmd.env("XDG_RUNTIME_DIR", value);
            }

            if systemd_context.is_some() {
                // we set env var to enable reading peak memory from cgroup in miniperf
                cmd.env("MINIPERF_READ_CGROUP", "1");
            }

            let stream_stdio = std_redirects.is_none();
            let mut cmd = ProcessCommand::new(cmd);
            if let Some(std_redirects) = std_redirects {
                cmd.stdout(File::create(OsStr::from_bytes(&std_redirects.stdout))?);
                cmd.stderr(File::create(OsStr::from_bytes(&std_redirects.stderr))?);
            }

            let process_group = cmd.spawn().map_err(buck2_error::Error::from);

            let timeout = timeout_into_cancellation(timeout);

            let cancellation = select(timeout.boxed(), cancel.boxed()).map(|r| r.factor_first().0);

            let stream = match miniperf_output {
                Some(out) => stream_command_events(
                    process_group,
                    cancellation,
                    MiniperfStatusDecoder::new(out),
                    DefaultKillProcess {
                        graceful_shutdown_timeout_s,
                    },
                    stream_stdio,
                )?
                .left_stream(),
                None => stream_command_events(
                    process_group,
                    cancellation,
                    DefaultStatusDecoder,
                    DefaultKillProcess {
                        graceful_shutdown_timeout_s,
                    },
                    stream_stdio,
                )?
                .right_stream(),
            };
            let stream = encode_event_stream(stream);
            Ok(Box::pin(stream) as _)
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
            .map_err(|e| Status::invalid_argument(format!("{:#}", e)))?;

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

        #[cfg(unix)]
        {
            use std::os::unix::fs::OpenOptionsExt;
            opts.mode(0o755);
        }

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
