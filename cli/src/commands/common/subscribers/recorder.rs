/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_client::client_ctx::ClientCommandContext;
use buck2_events::subscriber::EventSubscriber;
#[cfg(fbcode_build)]
use gazebo::dupe::Dupe;

#[cfg(fbcode_build)]
mod imp {
    use std::collections::HashMap;
    use std::future::Future;
    use std::sync::Arc;
    use std::time::Duration;
    use std::time::Instant;
    use std::time::SystemTime;

    use async_trait::async_trait;
    use buck2_client::cleanup_ctx::AsyncCleanupContext;
    use buck2_client::subscribers::last_command_execution_kind;
    use buck2_client::subscribers::last_command_execution_kind::LastCommandExecutionKind;
    use buck2_common::convert::ProstDurationExt;
    use buck2_events::sink::scribe::ThriftScribeSink;
    use buck2_events::subscriber::EventSubscriber;
    use buck2_events::BuckEvent;
    use buck2_events::EventSink;
    use buck2_events::TraceId;
    use futures::FutureExt;
    use gazebo::dupe::Dupe;
    use termwiz::istty::IsTty;

    pub struct InvocationRecorder {
        start_time: Instant,
        async_cleanup_context: AsyncCleanupContext,
        scribe: Arc<ThriftScribeSink>,
        trace_id: Option<TraceId>,
        command_start: Option<buck2_data::CommandStart>,
        command_end: Option<buck2_data::CommandEnd>,
        command_duration: Option<prost_types::Duration>,
        re_session_id: Option<String>,
        critical_path_duration: Option<Duration>,
        tags: Vec<String>,
        run_local_count: u64,
        run_remote_count: u64,
        run_action_cache_count: u64,
        first_snapshot: Option<buck2_data::Snapshot>,
        last_snapshot: Option<buck2_data::Snapshot>,
        branched_from_revision: Option<String>,
    }

    impl InvocationRecorder {
        pub(crate) fn new(
            async_cleanup_context: AsyncCleanupContext,
            scribe: ThriftScribeSink,
        ) -> Self {
            Self {
                start_time: Instant::now(),
                async_cleanup_context,
                scribe: Arc::new(scribe),
                trace_id: None,
                command_start: None,
                command_end: None,
                command_duration: None,
                re_session_id: None,
                critical_path_duration: None,
                tags: vec![],
                run_local_count: 0,
                run_remote_count: 0,
                run_action_cache_count: 0,
                first_snapshot: None,
                last_snapshot: None,
                branched_from_revision: None,
            }
        }

        fn exit(&mut self) -> Option<impl Future<Output = ()> + 'static + Send> {
            if let Some(trace_id) = self.trace_id.take() {
                let record = buck2_data::InvocationRecord {
                    command_start: self.command_start.take(),
                    command_end: self.command_end.take(),
                    command_duration: self.command_duration.take(),
                    client_walltime: Some(self.start_time.elapsed().into()),
                    re_session_id: self.re_session_id.take().unwrap_or_default(),
                    cli_args: std::env::args().collect::<Vec<String>>(),
                    critical_path_duration: self.critical_path_duration.map(Into::into),
                    client_metadata: Some(Self::collect_client_metadata()),
                    tags: self.tags.drain(..).collect(),
                    run_local_count: self.run_local_count,
                    run_remote_count: self.run_remote_count,
                    run_action_cache_count: self.run_action_cache_count,
                    first_snapshot: self.first_snapshot.take(),
                    last_snapshot: self.last_snapshot.take(),
                    branched_from_revision: self.branched_from_revision.take().unwrap_or_default(),
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
                    is_global_dispatcher_diff: false,
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
                    LastCommandExecutionKind::NoCommand => {}
                }
            }
            Ok(())
        }

        async fn handle_re_session_created(
            &mut self,
            session: &buck2_data::RemoteExecutionSessionCreated,
            _event: &BuckEvent,
        ) -> anyhow::Result<()> {
            self.re_session_id = Some(session.session_id.clone());
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
                self.branched_from_revision = Some(stats.branched_from_revision.clone());
            }
            Ok(())
        }
    }
}

#[cfg(fbcode_build)]
pub(crate) fn try_get_invocation_recorder(
    ctx: &ClientCommandContext,
) -> anyhow::Result<Option<Box<dyn EventSubscriber>>> {
    if buck2_events::sink::scribe::is_enabled() && ctx.replayer.is_none() {
        let recorder = imp::InvocationRecorder::new(
            ctx.async_cleanup_context().dupe(),
            buck2_events::sink::scribe::ThriftScribeSink::new(
                ctx.fbinit(),
                "buck2_events".to_owned(),
                1,
            )?,
        );
        return Ok(Some(Box::new(recorder)));
    }
    Ok(None)
}

#[cfg(not(fbcode_build))]
pub(crate) fn try_get_invocation_recorder(
    _ctx: &ClientCommandContext,
) -> anyhow::Result<Option<Box<dyn EventSubscriber>>> {
    Ok(None)
}
