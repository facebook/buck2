/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::path::PathBuf;

#[cfg(fbcode_build)]
use gazebo::dupe::Dupe;

use crate::client_ctx::ClientCommandContext;
use crate::subscribers::subscriber::EventSubscriber;

#[cfg(fbcode_build)]
mod imp {
    use std::collections::HashMap;
    use std::future::Future;
    use std::path::PathBuf;
    use std::sync::Arc;
    use std::time::Duration;
    use std::time::Instant;
    use std::time::SystemTime;

    use anyhow::Context;
    use async_trait::async_trait;
    use buck2_common::convert::ProstDurationExt;
    use buck2_core::fs::paths::AbsPathBuf;
    use buck2_events::sink::scribe::ThriftScribeSink;
    use buck2_events::BuckEvent;
    use buck2_events::EventSink;
    use buck2_events::TraceId;
    use futures::FutureExt;
    use gazebo::dupe::Dupe;
    use termwiz::istty::IsTty;

    use crate::build_count::BuildCountManager;
    use crate::cleanup_ctx::AsyncCleanupContext;
    use crate::subscribers::last_command_execution_kind;
    use crate::subscribers::last_command_execution_kind::LastCommandExecutionKind;
    use crate::subscribers::recorder::is_eden_dir;
    use crate::subscribers::subscriber::EventSubscriber;

    pub struct InvocationRecorder {
        cli_args: Vec<String>,
        start_time: Instant,
        async_cleanup_context: AsyncCleanupContext,
        scribe: Arc<ThriftScribeSink>,
        build_count_manager: BuildCountManager,
        trace_id: Option<TraceId>,
        command_start: Option<buck2_data::CommandStart>,
        command_end: Option<buck2_data::CommandEnd>,
        command_critical_start: Option<buck2_data::CommandCriticalStart>,
        command_critical_end: Option<buck2_data::CommandCriticalEnd>,
        command_duration: Option<prost_types::Duration>,
        re_session_id: Option<String>,
        re_experiment_name: Option<String>,
        critical_path_duration: Option<Duration>,
        tags: Vec<String>,
        run_local_count: u64,
        run_remote_count: u64,
        run_action_cache_count: u64,
        run_skipped_count: u64,
        first_snapshot: Option<buck2_data::Snapshot>,
        last_snapshot: Option<buck2_data::Snapshot>,
        branched_from_revision: Option<String>,
        min_build_count_since_rebase: u64,
        cache_upload_count: u64,
        cache_upload_attempt_count: u64,
        resolved_target_patterns: Option<buck2_data::ResolvedTargetPatterns>,
        invocation_root_path: AbsPathBuf,
        filesystem: Option<String>,
    }

    impl InvocationRecorder {
        pub fn new(
            async_cleanup_context: AsyncCleanupContext,
            scribe: ThriftScribeSink,
            sanitized_argv: Vec<String>,
            build_count_manager: BuildCountManager,
            invocation_root_path: AbsPathBuf,
        ) -> Self {
            Self {
                cli_args: sanitized_argv,
                start_time: Instant::now(),
                async_cleanup_context,
                scribe: Arc::new(scribe),
                build_count_manager,
                trace_id: None,
                command_start: None,
                command_end: None,
                command_critical_start: None,
                command_critical_end: None,
                command_duration: None,
                re_session_id: None,
                re_experiment_name: None,
                critical_path_duration: None,
                tags: vec![],
                run_local_count: 0,
                run_remote_count: 0,
                run_action_cache_count: 0,
                run_skipped_count: 0,
                first_snapshot: None,
                last_snapshot: None,
                branched_from_revision: None,
                min_build_count_since_rebase: 0,
                cache_upload_count: 0,
                cache_upload_attempt_count: 0,
                resolved_target_patterns: None,
                invocation_root_path,
                filesystem: None,
            }
        }

        async fn build_count(
            &mut self,
            target_patterns: &[buck2_data::TargetPattern],
        ) -> anyhow::Result<u64> {
            if let Some(merge_base) = &self.branched_from_revision {
                self.build_count_manager
                    .min_build_count(
                        merge_base,
                        self.resolved_target_patterns
                            .as_ref()
                            .map(|d| &d.target_patterns[..])
                            .unwrap_or(target_patterns),
                    )
                    .await
                    .context("Error recording build count")
            } else {
                Ok(0)
            }
        }

        fn exit(&mut self) -> Option<impl Future<Output = ()> + 'static + Send> {
            if let Some(trace_id) = self.trace_id.take() {
                let record = buck2_data::InvocationRecord {
                    command_start: self.command_start.take(),
                    command_end: self.command_end.take(),
                    command_critical_start: self.command_critical_start.take(),
                    command_critical_end: self.command_critical_end.take(),
                    command_duration: self.command_duration.take(),
                    client_walltime: Some(self.start_time.elapsed().into()),
                    re_session_id: self.re_session_id.take().unwrap_or_default(),
                    re_experiment_name: self.re_experiment_name.take().unwrap_or_default(),
                    cli_args: self.cli_args.clone(),
                    critical_path_duration: self.critical_path_duration.map(Into::into),
                    client_metadata: Some(Self::collect_client_metadata()),
                    tags: self.tags.drain(..).collect(),
                    run_local_count: self.run_local_count,
                    run_remote_count: self.run_remote_count,
                    run_action_cache_count: self.run_action_cache_count,
                    run_skipped_count: self.run_skipped_count,
                    first_snapshot: self.first_snapshot.take(),
                    last_snapshot: self.last_snapshot.take(),
                    branched_from_revision: self.branched_from_revision.take().unwrap_or_default(),
                    min_build_count_since_rebase: self.min_build_count_since_rebase,
                    cache_upload_count: self.cache_upload_count,
                    cache_upload_attempt_count: self.cache_upload_attempt_count,
                    resolved_target_patterns: self.resolved_target_patterns.take(),
                    filesystem: self.filesystem.take().unwrap_or_default(),
                };
                let event = BuckEvent {
                    timestamp: SystemTime::now(),
                    trace_id: trace_id.dupe(),
                    span_id: None,
                    parent_id: None,
                    data: buck2_data::RecordEvent {
                        data: Some(record.into()),
                    }
                    .into(),
                };
                tracing::info!("Recording invocation to Scribe: {:?}", &event);
                self.scribe.send(event);
                let scribe = self.scribe.dupe();
                Some(async move {
                    scribe.flush_blocking().await;
                })
            } else {
                None
            }
        }

        // Collects client-side state and data, suitable for telemetry.
        // NOTE: If data is visible from the daemon, put it in cli::metadata::collect()
        fn collect_client_metadata() -> buck2_data::TypedMetadata {
            let mut ints = HashMap::new();
            ints.insert("is_tty".to_owned(), std::io::stderr().is_tty() as i64);
            buck2_data::TypedMetadata { ints }
        }
    }

    impl Drop for InvocationRecorder {
        fn drop(&mut self) {
            if let Some(fut) = self.exit() {
                self.async_cleanup_context
                    .register("sending invocation to Scribe", fut.boxed());
            }
        }
    }

    #[async_trait]
    impl EventSubscriber for InvocationRecorder {
        async fn handle_command_start(
            &mut self,
            command: &buck2_data::CommandStart,
            event: &BuckEvent,
        ) -> anyhow::Result<()> {
            self.command_start = Some(command.clone());
            self.trace_id = Some(event.trace_id.dupe());
            Ok(())
        }

        async fn handle_command_end(
            &mut self,
            command: &buck2_data::CommandEnd,
            event: &BuckEvent,
        ) -> anyhow::Result<()> {
            // Awkwardly unpacks the SpanEnd event so we can read its duration.
            let command_end = match event.data {
                buck2_data::buck_event::Data::SpanEnd(ref end) => end.clone(),
                _ => {
                    return Err(anyhow::anyhow!(
                        "handle_command_end was passed a CommandEnd not contained in a SpanEndEvent"
                    ));
                }
            };
            self.command_duration = command_end.duration;
            self.command_end = Some(command.clone());
            self.min_build_count_since_rebase =
                match command.data.as_ref().context("Missing command data")? {
                    buck2_data::command_end::Data::Build(cmd) => {
                        self.build_count(&cmd.unresolved_target_patterns).await?
                    }
                    buck2_data::command_end::Data::Test(cmd) => {
                        self.build_count(&cmd.unresolved_target_patterns).await?
                    }
                    buck2_data::command_end::Data::Install(cmd) => {
                        self.build_count(&cmd.unresolved_target_patterns).await?
                    }
                    // other events don't have target patterns
                    _ => 0,
                };
            let root = PathBuf::to_owned(self.invocation_root_path.as_ref());
            if is_eden_dir(root).unwrap_or(false) {
                self.filesystem = Some("eden".to_owned());
            } else {
                self.filesystem = Some("default".to_owned());
            }
            Ok(())
        }
        async fn handle_command_critical_start(
            &mut self,
            command: &buck2_data::CommandCriticalStart,
            _event: &BuckEvent,
        ) -> anyhow::Result<()> {
            self.command_critical_start = Some(command.clone());
            Ok(())
        }
        async fn handle_command_critical_end(
            &mut self,
            command: &buck2_data::CommandCriticalEnd,
            _event: &BuckEvent,
        ) -> anyhow::Result<()> {
            self.command_critical_end = Some(command.clone());
            Ok(())
        }

        async fn handle_action_execution_end(
            &mut self,
            action: &buck2_data::ActionExecutionEnd,
            _event: &BuckEvent,
        ) -> anyhow::Result<()> {
            if action.kind == buck2_data::ActionKind::Run as i32 {
                match last_command_execution_kind::get_last_command_execution_kind(action) {
                    LastCommandExecutionKind::Local => {
                        self.run_local_count += 1;
                    }
                    LastCommandExecutionKind::Cached => {
                        self.run_action_cache_count += 1;
                    }
                    LastCommandExecutionKind::Remote => {
                        self.run_remote_count += 1;
                    }
                    LastCommandExecutionKind::NoCommand => {
                        self.run_skipped_count += 1;
                    }
                }
            }
            Ok(())
        }

        async fn handle_cache_upload_end(
            &mut self,
            cache_upload: &buck2_data::CacheUploadEnd,
            _event: &BuckEvent,
        ) -> anyhow::Result<()> {
            if cache_upload.success {
                self.cache_upload_count += 1;
            }
            self.cache_upload_attempt_count += 1;
            Ok(())
        }

        async fn handle_re_session_created(
            &mut self,
            session: &buck2_data::RemoteExecutionSessionCreated,
            _event: &BuckEvent,
        ) -> anyhow::Result<()> {
            self.re_session_id = Some(session.session_id.clone());
            self.re_experiment_name = Some(session.experiment_name.clone());
            Ok(())
        }

        async fn handle_build_graph_info(
            &mut self,
            info: &buck2_data::BuildGraphExecutionInfo,
            _event: &BuckEvent,
        ) -> anyhow::Result<()> {
            let durations = info
                .critical_path
                .iter()
                .filter_map(|x| x.duration.as_ref())
                .map(|d| d.try_into_duration())
                .collect::<Result<Vec<_>, _>>()?;
            self.critical_path_duration = Some(durations.iter().sum());
            Ok(())
        }

        async fn handle_tag(&mut self, tag: &buck2_data::TagEvent) -> anyhow::Result<()> {
            self.tags.extend(tag.tags.iter().cloned());
            Ok(())
        }

        async fn handle_snapshot(
            &mut self,
            update: &buck2_data::Snapshot,
            _event: &BuckEvent,
        ) -> anyhow::Result<()> {
            if self.first_snapshot.is_none() {
                self.first_snapshot = Some(update.clone());
            } else {
                self.last_snapshot = Some(update.clone());
            }
            Ok(())
        }

        async fn handle_watchman_end(
            &mut self,
            watchman: &buck2_data::WatchmanEnd,
            _event: &BuckEvent,
        ) -> anyhow::Result<()> {
            if let Some(stats) = &watchman.stats {
                self.branched_from_revision = stats.branched_from_revision.clone();
            }
            Ok(())
        }

        async fn handle_resolved_target_patterns(
            &mut self,
            patterns: &buck2_data::ResolvedTargetPatterns,
        ) -> anyhow::Result<()> {
            self.resolved_target_patterns = Some(patterns.clone());
            Ok(())
        }
    }
}

#[cfg(fbcode_build)]
pub(crate) fn try_get_invocation_recorder(
    ctx: &ClientCommandContext,
    sanitized_argv: Vec<String>,
) -> anyhow::Result<Option<Box<dyn EventSubscriber>>> {
    use buck2_common::events;

    use crate::build_count::BuildCountManager;

    if buck2_events::sink::scribe::is_enabled() && ctx.replayer.is_none() {
        let recorder = imp::InvocationRecorder::new(
            ctx.async_cleanup_context().dupe(),
            buck2_events::sink::scribe::ThriftScribeSink::new(
                ctx.fbinit(),
                events::scribe_category()?,
                1,
            )?,
            sanitized_argv,
            BuildCountManager::new(ctx.paths()?.build_count_dir()),
            ctx.paths()?.project_root().root().to_buf(),
        );
        return Ok(Some(Box::new(recorder)));
    }
    Ok(None)
}

#[cfg(not(fbcode_build))]
pub(crate) fn try_get_invocation_recorder(
    _ctx: &ClientCommandContext,
    _sanitized_argv: Vec<String>,
) -> anyhow::Result<Option<Box<dyn EventSubscriber>>> {
    Ok(None)
}

// TODO: is_eden_dir() should probably be placed in buck2_common/src/eden/mod.rs as a public function,
// but current Windows build limitations make importing that module difficult
// https://www.internalfb.com/intern/wiki/EdenFS/detecting-an-eden-mount/
#[cfg(not(windows))]
pub fn is_eden_dir(mut dir: PathBuf) -> anyhow::Result<bool> {
    dir.push(".eden");
    dir.push("root");
    Ok(std::fs::read_link(&dir).is_ok())
}

#[cfg(windows)]
pub fn is_eden_dir(mut dir: PathBuf) -> anyhow::Result<bool> {
    // https://docs.microsoft.com/en-us/windows/win32/fileio/determining-whether-a-directory-is-a-volume-mount-point
    fn is_mount_point(mut temp_dir: PathBuf) -> Result<bool, std::io::Error> {
        use std::mem::MaybeUninit;
        use std::os::windows::ffi::OsStrExt;
        use std::os::windows::fs::MetadataExt;

        // Append a `\` to the end of the directory path, per Windows documentation requirement
        if !temp_dir.ends_with("") {
            temp_dir.push("");
        }
        let mut encoded = temp_dir.as_os_str().encode_wide().collect::<Vec<u16>>();
        encoded.push(0);

        unsafe {
            let metadata = std::fs::metadata(&temp_dir)?;
            if metadata.file_attributes() & winapi::um::winnt::FILE_ATTRIBUTE_REPARSE_POINT == 0 {
                return Ok(false);
            }

            let mut data: MaybeUninit<winapi::um::minwinbase::WIN32_FIND_DATAW> =
                MaybeUninit::uninit();
            let data_ptr = data.as_mut_ptr();
            let handle = winapi::um::fileapi::FindFirstFileW(encoded.as_ptr(), data_ptr);
            if handle == winapi::um::handleapi::INVALID_HANDLE_VALUE {
                return Err(std::io::Error::new(
                    std::io::ErrorKind::Other,
                    "invalid handle value".to_owned(),
                ));
            }
            winapi::um::fileapi::FindClose(handle);

            let data = data.assume_init();
            Ok(data.dwReserved0 == winapi::um::winnt::IO_REPARSE_TAG_MOUNT_POINT)
        }
    }

    dir = std::fs::canonicalize(&dir)?;
    if is_mount_point(dir.clone())? {
        return Ok(false);
    }

    dir.push(".eden");
    dir.push("config");
    if std::fs::metadata(&dir).map_or(false, |metadata| metadata.is_file()) {
        Ok(true)
    } else {
        Ok(false)
    }
}
