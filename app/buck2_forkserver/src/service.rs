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
use std::path::Path;
use std::path::PathBuf;
use std::pin::Pin;
use std::sync::Arc;

use buck2_common::cgroup_pool::cgroup::CgroupID;
use buck2_common::cgroup_pool::pool::CgroupPool;
use buck2_common::convert::ProstDurationExt;
use buck2_common::init::ResourceControlConfig;
use buck2_common::resource_control::ParentSlice;
use buck2_common::resource_control::ResourceControlRunner;
use buck2_common::resource_control::ResourceControlRunnerConfig;
use buck2_common::resource_control::replace_unit_delimiter;
use buck2_core::fs::fs_util;
use buck2_core::fs::paths::abs_norm_path::AbsNormPath;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::paths::abs_path::AbsPath;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePath;
use buck2_core::logging::LogConfigurationReloadHandle;
use buck2_error::BuckErrorContext;
use buck2_execute_local::CommandEvent;
use buck2_execute_local::DefaultKillProcess;
use buck2_execute_local::GatherOutputStatus;
use buck2_execute_local::maybe_absolutize_exe;
use buck2_execute_local::process_group::ProcessCommand;
use buck2_execute_local::status_decoder::DefaultStatusDecoder;
use buck2_execute_local::status_decoder::MiniperfStatusDecoder;
use buck2_execute_local::stream_command_events;
use buck2_execute_local::timeout_into_cancellation;
use buck2_forkserver_proto::CommandRequest;
use buck2_forkserver_proto::GetCgroupRequest;
use buck2_forkserver_proto::GetCgroupResponse;
use buck2_forkserver_proto::RequestEvent;
use buck2_forkserver_proto::SetLogFilterRequest;
use buck2_forkserver_proto::SetLogFilterResponse;
use buck2_forkserver_proto::command_request::StdRedirectPaths;
use buck2_forkserver_proto::forkserver_server::Forkserver;
use buck2_grpc::to_tonic;
use buck2_util::cgroup_info::CGroupInfo;
use buck2_util::process::background_command;
use dupe::Dupe;
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
    cgroup_command_id: Option<String>,
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
            cgroup_command_id,
            action_digest: _,
        } = cmd_request;

        let exe = OsStr::from_bytes(&exe);
        let cwd = OsStr::from_bytes(&cwd.as_ref().buck_error_context("Missing cwd")?.path);
        let cwd = AbsPath::new(Path::new(cwd)).buck_error_context("Invalid cwd")?;

        let timeout = timeout
            .map(|t| t.try_into_duration())
            .transpose()
            .buck_error_context("Invalid timeout")?;

        let exe = maybe_absolutize_exe(exe, cwd)?;

        Ok(ValidatedCommand {
            exe: exe.into_owned(),
            argv,
            env,
            cwd: cwd.to_path_buf(),
            timeout,
            enable_miniperf,
            std_redirects,
            graceful_shutdown_timeout_s,
            cgroup_command_id,
        })
    }
}

pub(crate) enum ForkserverResourceControlRunner {
    Systemd(ResourceControlRunner),
    CgroupPool(CgroupPool),
    None,
}

pub(crate) struct UnixForkserverService {
    log_reload_handle: Arc<dyn LogConfigurationReloadHandle>,

    /// State for Miniperf.
    miniperf: Option<MiniperfContainer>,

    /// Systemd runner for resource control
    resource_control_runner: ForkserverResourceControlRunner,

    /// Whether this forkserver is running in a cgroup
    has_cgroup: bool,
}

impl UnixForkserverService {
    pub(crate) fn new(
        log_reload_handle: Arc<dyn LogConfigurationReloadHandle>,
        state_dir: &AbsNormPath,
        resource_control: ResourceControlConfig,
        has_cgroup: bool,
    ) -> buck2_error::Result<Self> {
        let miniperf = MiniperfContainer::new(state_dir)?;
        let resource_control_runner =
            if ResourceControlRunner::is_enabled(&resource_control.status)? {
                if resource_control.enable_action_cgroup_pool == Some(true) {
                    let capacity = resource_control
                        .cgroup_pool_size
                        .map(|x| x as usize)
                        .unwrap_or(buck2_util::threads::available_parallelism_fresh());
                    let cgroup_pool = CgroupPool::new(
                        capacity,
                        resource_control.memory_high_per_action.as_deref(),
                        resource_control.memory_high_action_cgroup_pool.as_deref(),
                    )
                    .buck_error_context("Failed to create cgroup pool")?;
                    ForkserverResourceControlRunner::CgroupPool(cgroup_pool)
                } else {
                    ForkserverResourceControlRunner::Systemd(ResourceControlRunner::create(
                        &ResourceControlRunnerConfig::action_runner_config(
                            &resource_control,
                            // we want to create forkserver in the same hierarchy where buck-daemon scope
                            // for this we inherit slice
                            ParentSlice::Inherit("forkserver".to_owned()),
                        ),
                    )?)
                }
            } else {
                ForkserverResourceControlRunner::None
            };
        Ok(Self {
            log_reload_handle,
            miniperf,
            resource_control_runner,
            has_cgroup,
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
        cgroup_id: Option<CgroupID>,
    ) -> buck2_error::Result<(ProcessCommand, Option<AbsNormPathBuf>)> {
        let (mut cmd, miniperf_output, cgroup_path, has_resource_control) = match (
            validated_cmd.enable_miniperf,
            &self.miniperf,
            &self.resource_control_runner,
            validated_cmd.cgroup_command_id.as_ref(),
            cgroup_id,
        ) {
            // Wraps the user command with miniperf for performance monitoring
            (true, Some(miniperf), ForkserverResourceControlRunner::None, _, _) => {
                let mut cmd = background_command(miniperf.miniperf.as_path());
                let output_path = miniperf.allocate_output_path();
                cmd.arg(output_path.as_path());
                cmd.arg(&validated_cmd.exe);
                (cmd, Some(output_path), None, false)
            }
            (
                _,
                Some(miniperf),
                ForkserverResourceControlRunner::CgroupPool(cgroup_pool),
                _,
                Some(cgroup_id),
            ) => {
                let mut cmd = background_command(miniperf.miniperf.as_path());
                let cgroup_path = cgroup_pool.setup_command(cgroup_id, &mut cmd)?;
                let output_path = miniperf.allocate_output_path();
                cmd.arg(output_path.as_path());
                cmd.arg(&validated_cmd.exe);
                (cmd, Some(output_path), Some(cgroup_path), true)
            }
            // Uses systemd-run + miniperf for resource control + monitoring
            // systemd-run --scope --unit=<cgroup_command_id> miniperf <output_path> <user_executable>
            (
                _,
                Some(miniperf),
                ForkserverResourceControlRunner::Systemd(resource_control_runner),
                Some(cgroup_command_id),
                None,
            ) => {
                let workding_dir = AbsNormPath::new(validated_cmd.cwd.as_path())?;
                let mut cmd = resource_control_runner.cgroup_scoped_command(
                    miniperf.miniperf.as_path(),
                    &replace_unit_delimiter(cgroup_command_id),
                    workding_dir,
                );
                let output_path = miniperf.allocate_output_path();
                cmd.arg(output_path.as_path());
                cmd.arg(&validated_cmd.exe);
                (cmd, Some(output_path), None, true)
            }
            // Direct execution of the command
            _ => (background_command(&validated_cmd.exe), None, None, false),
        };

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

        if has_resource_control {
            // we set env var to enable reading peak memory from cgroup in miniperf
            cmd.env("MINIPERF_READ_CGROUP", "1");
        }

        let mut cmd = ProcessCommand::new(cmd, cgroup_path);
        if let Some(std_redirects) = &validated_cmd.std_redirects {
            cmd.stdout(File::create(OsStr::from_bytes(&std_redirects.stdout))?);
            cmd.stderr(File::create(OsStr::from_bytes(&std_redirects.stderr))?);
        }

        // cmd: ready-to-spawn process command
        // miniperf_output: path to miniperf output file (if monitoring)
        Ok((cmd, miniperf_output))
    }

    fn create_command_stream(
        process_group: buck2_error::Result<buck2_execute_local::process_group::ProcessGroup>,
        cancellation: impl futures::Future<Output = buck2_error::Result<GatherOutputStatus>>
        + Send
        + 'static,
        miniperf_output: Option<AbsNormPathBuf>,
        graceful_shutdown_timeout_s: Option<u32>,
        stream_stdio: bool,
        on_exit: Option<Box<dyn FnOnce() + Send + 'static>>,
    ) -> buck2_error::Result<RunStream> {
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

        let stream = if on_exit.is_some() {
            stream
                .inspect({
                    let mut on_exit = on_exit;
                    move |event| {
                        if let Ok(CommandEvent::Exit(_status)) = event {
                            if let Some(cleanup) = on_exit.take() {
                                cleanup();
                            }
                        }
                    }
                })
                .left_stream()
        } else {
            stream.right_stream()
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
        let id_and_pool = if let ForkserverResourceControlRunner::CgroupPool(cgroup_pool) =
            &self.resource_control_runner
        {
            let id = cgroup_pool.acquire().map_err(|e| {
                Status::failed_precondition(format!(
                    "Cannot acquire cgroup from cgroup pool: {}",
                    e
                ))
            })?;
            Some((cgroup_pool.dupe(), id))
        } else {
            None
        };

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

            let (mut cmd, miniperf_output) =
                self.setup_process_command(&validated_cmd, id_and_pool.as_ref().map(|x| x.1))?;
            let process_group = cmd.spawn().map_err(buck2_error::Error::from);

            let timeout = timeout_into_cancellation(validated_cmd.timeout);
            let cancellation = select(timeout.boxed(), cancel.boxed()).map(|r| r.factor_first().0);

            let stream_stdio = validated_cmd.std_redirects.is_none();

            let on_exit = id_and_pool.map(|(pool, cgroup_id)| {
                Box::new(move || {
                    // realse cgroup on exit
                    pool.release(cgroup_id);
                }) as Box<dyn FnOnce() + Send>
            });

            let stream = Self::create_command_stream(
                process_group,
                cancellation,
                miniperf_output,
                validated_cmd.graceful_shutdown_timeout_s,
                stream_stdio,
                on_exit,
            )?;

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

    async fn get_cgroup(
        &self,
        _req: Request<GetCgroupRequest>,
    ) -> Result<Response<GetCgroupResponse>, Status> {
        if !self.has_cgroup {
            return Ok(Response::new(GetCgroupResponse { cgroup_path: None }));
        }

        let cgroup_info = CGroupInfo::read()
            .buck_error_context("Failed to read cgroup info")
            .map_err(|e| Status::internal(format!("{e:#}")))?;

        Ok(Response::new(GetCgroupResponse {
            cgroup_path: Some(cgroup_info.path),
        }))
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
