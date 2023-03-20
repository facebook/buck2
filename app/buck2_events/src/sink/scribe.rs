/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! A Sink for forwarding events directly to Scribe.
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;
use std::time::Duration;

use buck2_core::env_helper::EnvHelper;
use fbinit::FacebookInit;

#[cfg(fbcode_build)]
mod fbcode {

    use std::time::Duration;
    use std::time::SystemTime;

    use buck2_core::truncate::truncate;
    use buck2_data::InstantEvent;
    use buck2_data::Location;
    use buck2_data::Panic;
    use fbinit::FacebookInit;
    use prost::Message;

    use crate::metadata;
    use crate::BuckEvent;
    use crate::ControlEvent;
    use crate::EventSink;
    use crate::EventSinkStats;
    use crate::TraceId;

    // 1 MiB limit
    static SCRIBE_MESSAGE_SIZE_LIMIT: usize = 1024 * 1024;
    // 50k characters
    static TRUNCATED_SCRIBE_MESSAGE_SIZE: usize = 50000;

    /// ThriftScribeSink is a ScribeSink backed by the Thrift-based client in the `buck2_scribe_client` crate.
    pub struct ThriftScribeSink {
        category: String,
        client: scribe_client::ScribeClient,
    }

    impl ThriftScribeSink {
        /// Creates a new ThriftScribeSink that forwards messages onto the Thrift-backed Scribe client.
        pub fn new(
            fb: FacebookInit,
            category: String,
            buffer_size: usize,
            retry_backoff: Duration,
            retry_attempts: usize,
            message_batch_size: Option<usize>,
        ) -> anyhow::Result<ThriftScribeSink> {
            let client = scribe_client::ScribeClient::new(
                fb,
                buffer_size,
                retry_backoff,
                retry_attempts,
                message_batch_size,
            )?;
            Ok(ThriftScribeSink { category, client })
        }

        // Send this event now, bypassing internal message queue.
        pub async fn send_now(&self, event: BuckEvent) {
            let message_key = event.trace_id().unwrap().hash();
            if let Some(bytes) = self.encode_message(event, false) {
                self.client
                    .send_now(scribe_client::Message {
                        category: self.category.clone(),
                        message: bytes,
                        message_key: Some(message_key),
                    })
                    .await;
            }
        }

        // Send this event by placing it on the internal message queue.
        pub fn offer(&self, event: BuckEvent) {
            let message_key = event.trace_id().unwrap().hash();
            if let Some(bytes) = self.encode_message(event, false) {
                self.client.offer(scribe_client::Message {
                    category: self.category.clone(),
                    message: bytes,
                    message_key: Some(message_key),
                });
            }
        }

        // Encodes message into something scribe understands.
        fn encode_message(&self, mut event: BuckEvent, is_truncated: bool) -> Option<Vec<u8>> {
            Self::smart_truncate_event(event.data_mut());
            let proto: Box<buck2_data::BuckEvent> = event.into();

            // Add a header byte to indicate this is _not_ base64 encoding.
            let mut buf = Vec::with_capacity(proto.encoded_len() + 1);
            buf.push(b'!');
            let mut proto_bytes = proto.encode_to_vec();
            buf.append(&mut proto_bytes);

            if buf.len() > SCRIBE_MESSAGE_SIZE_LIMIT {
                // if this BuckEvent is already a truncated one but the buffer byte size exceeds the limit,
                // do not send Scribe another truncated version
                if is_truncated {
                    return None;
                }
                let json = serde_json::to_string(&proto).unwrap();

                self.encode_message(
                    BuckEvent::new(
                        SystemTime::now(),
                        TraceId::new(),
                        None,
                        None,
                        buck2_data::buck_event::Data::Instant(InstantEvent {
                            data: Some(
                                Panic {
                                    location: Some(Location {
                                        file: file!().to_string(),
                                        line: line!(),
                                        column: column!(),
                                    }),
                                    payload: format!("Soft Error: oversized_scribe: Message is oversized. Event data: {}. Original message size: {}", truncate(&json, TRUNCATED_SCRIBE_MESSAGE_SIZE),
                                    buf.len()),
                                    metadata: metadata::collect(),
                                    backtrace: Vec::new(),
                                    quiet: false,
                                    soft_error_category: Some("oversized_scribe".to_owned()),
                                }
                                .into(),
                            ),
                        }),
                    ),
                    true,
                )
            } else {
                Some(buf)
            }
        }

        fn smart_truncate_event(d: &mut buck2_data::buck_event::Data) {
            use buck2_data::buck_event::Data;

            match d {
                Data::SpanEnd(ref mut s) => {
                    use buck2_data::span_end_event::Data;

                    match &mut s.data {
                        Some(Data::ActionExecution(ref mut action_execution)) => {
                            // truncate(...) can panic if asked to truncate too short.
                            const MIN_CMD_TRUNCATION: usize = 20;
                            let per_command_size_budget = ((500 * 1024)
                                / action_execution.commands.len().max(1))
                            .max(MIN_CMD_TRUNCATION);

                            let truncate_cmd =
                                |cmd: &mut buck2_data::CommandExecution, truncate_all: bool| {
                                    if let Some(details) = &mut cmd.details {
                                        details.stderr = if truncate_all {
                                            "<<omitted>>".to_owned()
                                        } else {
                                            truncate(&details.stderr, per_command_size_budget)
                                        };
                                    }
                                };

                            if let Some((last_command, retries)) =
                                action_execution.commands.split_last_mut()
                            {
                                for retried in retries {
                                    truncate_cmd(retried, false);
                                }
                                // Current Scribe tailers don't read stderr of successful actions.
                                // Save some bytes.
                                truncate_cmd(last_command, !action_execution.failed);
                            }
                        }
                        Some(Data::Command(ref mut command_end)) => {
                            use buck2_data::command_end::Data;
                            match &mut command_end.data {
                                Some(Data::Build(ref mut build_command_end)) => {
                                    Self::truncate_target_patterns(
                                        &mut build_command_end.unresolved_target_patterns,
                                    );
                                }
                                Some(Data::Test(ref mut test_command_end)) => {
                                    Self::truncate_target_patterns(
                                        &mut test_command_end.unresolved_target_patterns,
                                    );
                                }
                                Some(Data::Install(ref mut install_command_end)) => {
                                    Self::truncate_target_patterns(
                                        &mut install_command_end.unresolved_target_patterns,
                                    );
                                }
                                _ => {}
                            }
                        }
                        _ => {}
                    };
                }
                Data::Instant(ref mut inst) => {
                    use buck2_data::instant_event::Data;
                    match &mut inst.data {
                        Some(Data::TestResult(ref mut test_result)) => {
                            const TRUNCATED_DETAILS_LENGTH: usize = 512 * 1024; // 512Kb
                            test_result.details =
                                truncate(&test_result.details, TRUNCATED_DETAILS_LENGTH);
                        }
                        Some(Data::TargetPatterns(ref mut target_patterns)) => {
                            Self::truncate_target_patterns(&mut target_patterns.target_patterns);
                        }
                        _ => {}
                    }
                }
                Data::Record(ref mut rec) => {
                    if let Some(buck2_data::record_event::Data::InvocationRecord(
                        ref mut invocation_record,
                    )) = rec.data
                    {
                        if let Some(ref mut file_watcher_stats) =
                            invocation_record.file_watcher_stats
                        {
                            const MAX_FILE_CHANGE_BYTES: usize = 100 * 1024;
                            let mut bytes: usize = 0;
                            for (index, ev) in file_watcher_stats.events.iter().enumerate() {
                                bytes += ev.path.len();
                                if bytes > MAX_FILE_CHANGE_BYTES {
                                    file_watcher_stats.events.truncate(index);
                                    file_watcher_stats.incomplete_events_reason = Some(format!(
                                        "Too long file change records ({} bytes, max {} bytes)",
                                        bytes, MAX_FILE_CHANGE_BYTES
                                    ));
                                    break;
                                }
                            }
                        }
                        if let Some(ref mut resolved_target_patterns) =
                            invocation_record.resolved_target_patterns
                        {
                            Self::truncate_target_patterns(
                                &mut resolved_target_patterns.target_patterns,
                            );
                        }
                        if let Some(ref mut command_end) = invocation_record.command_end {
                            use buck2_data::command_end::Data;
                            match &mut command_end.data {
                                Some(Data::Build(ref mut build_command_end)) => {
                                    Self::truncate_target_patterns(
                                        &mut build_command_end.unresolved_target_patterns,
                                    );
                                }
                                Some(Data::Test(ref mut test_command_end)) => {
                                    Self::truncate_target_patterns(
                                        &mut test_command_end.unresolved_target_patterns,
                                    );
                                }
                                Some(Data::Install(ref mut install_command_end)) => {
                                    Self::truncate_target_patterns(
                                        &mut install_command_end.unresolved_target_patterns,
                                    );
                                }
                                _ => {}
                            }
                        }
                    }
                }
                _ => {}
            };
        }

        fn truncate_target_patterns(target_patterns: &mut Vec<buck2_data::TargetPattern>) {
            const MAX_TARGET_PATTERNS_BYTES: usize = 512 * 1024;
            let orig_len = target_patterns.len();
            let mut bytes: usize = 0;
            for (index, target) in target_patterns.iter().enumerate() {
                bytes += target.value.len();
                if bytes > MAX_TARGET_PATTERNS_BYTES {
                    target_patterns.truncate(index);
                    let warn = format!("<<Truncated (reported {} / {})>>", index, orig_len);
                    target_patterns.push(buck2_data::TargetPattern { value: warn });
                    break;
                }
            }
        }
    }

    impl EventSink for ThriftScribeSink {
        fn send(&self, event: BuckEvent) {
            if !should_send_event(event.data()) {
                return;
            }
            self.offer(event);
        }

        fn send_control(&self, _control_event: ControlEvent) {}

        fn stats(&self) -> Option<EventSinkStats> {
            self.client
                .export_counters()
                .map(|counters| EventSinkStats {
                    successes: counters.successes,
                    failures: counters.failures,
                    buffered: counters.queue_depth,
                    dropped: counters.dropped,
                })
        }
    }

    fn should_send_event(d: &buck2_data::buck_event::Data) -> bool {
        use buck2_data::buck_event::Data;
        use buck2_data::ActionKind;

        match d {
            Data::SpanStart(s) => {
                use buck2_data::span_start_event::Data;

                match &s.data {
                    Some(Data::Command(..)) => true, // used in CommandReporterProcessor
                    Some(Data::ActionExecution(a)) => match ActionKind::from_i32(a.kind) {
                        // Simple actions are not useful for most log analysis cases
                        Some(ActionKind::Copy)
                        | Some(ActionKind::SymlinkedDir)
                        | Some(ActionKind::Write)
                        | Some(ActionKind::WriteMacrosToFile) => false,
                        _ => true,
                    },
                    Some(Data::Analysis(..)) => false,
                    Some(Data::AnalysisStage(..)) => false,
                    Some(Data::FinalMaterialization(..)) => false,
                    Some(Data::Load(..)) => false,
                    Some(Data::LoadPackage(..)) => false,
                    Some(Data::ExecutorStage(..)) => false,
                    Some(Data::TestDiscovery(..)) => false,
                    Some(Data::TestStart(..)) => false,
                    Some(Data::FileWatcher(..)) => false,
                    Some(Data::MatchDepFiles(..)) => false,
                    Some(Data::SharedTask(..)) => false,
                    Some(Data::CacheUpload(..)) => false,
                    Some(Data::CreateOutputSymlinks(..)) => false,
                    Some(Data::CommandCritical(..)) => false,
                    Some(Data::InstallEventInfo(..)) => false,
                    Some(Data::DiceStateUpdate(_)) => false,
                    Some(Data::Materialization(..)) => false,
                    Some(Data::DiceCriticalSection(_)) => false,
                    Some(Data::DiceBlockConcurrentCommand(_)) => false,
                    Some(Data::DiceSynchronizeSection(_)) => false,
                    Some(Data::DiceCleanup(_)) => false,
                    Some(Data::ExclusiveCommandWait(_)) => false,
                    Some(Data::DeferredPreparationStage(_)) => false,
                    Some(Data::DynamicLambda(_)) => true,
                    Some(Data::BxlExecution(_)) => false,
                    Some(Data::BxlDiceInvocation(_)) => false,
                    Some(Data::ReUpload(_)) => false,
                    Some(Data::ConnectToInstaller(_)) => false,
                    Some(Data::Fake(..)) => false,
                    None => false,
                }
            }
            Data::SpanEnd(s) => {
                use buck2_data::span_end_event::Data;
                use buck2_data::ActionExecutionKind;

                match &s.data {
                    Some(Data::Command(..)) => true,
                    Some(Data::ActionExecution(a)) => {
                        match ActionExecutionKind::from_i32(a.execution_kind) {
                            // Not useful for most log analysis cases
                            Some(ActionExecutionKind::Simple) => false,
                            _ => true,
                        }
                    }
                    Some(Data::Analysis(..)) => true,
                    Some(Data::AnalysisStage(..)) => false,
                    Some(Data::FinalMaterialization(..)) => true,
                    Some(Data::Load(..)) => true,
                    Some(Data::LoadPackage(..)) => true,
                    Some(Data::ExecutorStage(..)) => false,
                    Some(Data::TestDiscovery(..)) => true,
                    Some(Data::TestEnd(..)) => true,
                    Some(Data::SpanCancelled(..)) => false,
                    Some(Data::FileWatcher(..)) => true,
                    Some(Data::MatchDepFiles(..)) => false,
                    Some(Data::SharedTask(..)) => false,
                    Some(Data::CacheUpload(..)) => true,
                    Some(Data::CreateOutputSymlinks(..)) => false,
                    Some(Data::CommandCritical(..)) => false,
                    Some(Data::InstallEventInfo(..)) => false,
                    Some(Data::DiceStateUpdate(_)) => false,
                    Some(Data::Materialization(..)) => true, // used in MaterializationProcessor
                    Some(Data::DiceCriticalSection(_)) => false,
                    Some(Data::DiceBlockConcurrentCommand(_)) => false,
                    Some(Data::DiceSynchronizeSection(_)) => false,
                    Some(Data::DiceCleanup(_)) => false,
                    Some(Data::ExclusiveCommandWait(_)) => false,
                    Some(Data::DeferredPreparationStage(_)) => false,
                    Some(Data::DeferredEvaluation(_)) => false,
                    Some(Data::BxlExecution(_)) => false,
                    Some(Data::BxlDiceInvocation(_)) => false,
                    Some(Data::ReUpload(_)) => false,
                    Some(Data::ConnectToInstaller(_)) => false,
                    Some(Data::Fake(..)) => true,
                    None => false,
                }
            }
            Data::Instant(i) => {
                use buck2_data::instant_event::Data;

                match i.data {
                    Some(Data::Snapshot(..)) => false,
                    Some(Data::DiceStateSnapshot(..)) => false,
                    Some(Data::DiceEqualityCheck(..)) => false,
                    Some(Data::NoActiveDiceState(..)) => false,
                    None => false,
                    _ => true,
                }
            }
            Data::Record(_) => true,
        }
    }
}

#[cfg(not(fbcode_build))]
mod fbcode {
    use crate::BuckEvent;
    use crate::ControlEvent;
    use crate::EventSink;
    use crate::EventSinkStats;

    pub struct ThriftScribeSink;

    impl ThriftScribeSink {
        pub async fn send_now(&self, _event: BuckEvent) {}
    }

    impl EventSink for ThriftScribeSink {
        fn send(&self, _event: BuckEvent) {}

        fn send_control(&self, _control_event: ControlEvent) {}

        fn stats(&self) -> Option<EventSinkStats> {
            None
        }
    }
}

pub use fbcode::*;

fn new_thrift_scribe_sink_if_fbcode(
    fb: FacebookInit,
    buffer_size: usize,
    retry_backoff: Duration,
    retry_attempts: usize,
    message_batch_size: Option<usize>,
) -> anyhow::Result<Option<ThriftScribeSink>> {
    #[cfg(fbcode_build)]
    {
        Ok(Some(ThriftScribeSink::new(
            fb,
            scribe_category()?,
            buffer_size,
            retry_backoff,
            retry_attempts,
            message_batch_size,
        )?))
    }
    #[cfg(not(fbcode_build))]
    {
        let _ = (
            fb,
            buffer_size,
            retry_backoff,
            retry_attempts,
            message_batch_size,
        );
        Ok(None)
    }
}

pub fn new_thrift_scribe_sink_if_enabled(
    fb: FacebookInit,
    buffer_size: usize,
    retry_backoff: Duration,
    retry_attempts: usize,
    message_batch_size: Option<usize>,
) -> anyhow::Result<Option<ThriftScribeSink>> {
    if is_enabled() {
        new_thrift_scribe_sink_if_fbcode(
            fb,
            buffer_size,
            retry_backoff,
            retry_attempts,
            message_batch_size,
        )
    } else {
        Ok(None)
    }
}

/// Whether or not Scribe logging is enabled for this process. It must be explicitly disabled via `disable()`.
static SCRIBE_ENABLED: AtomicBool = AtomicBool::new(true);

/// Returns whether this process should actually write to Scribe, even if it is fully supported by the platform and
/// binary.
pub fn is_enabled() -> bool {
    SCRIBE_ENABLED.load(Ordering::Relaxed)
}

/// Disables Scribe logging for this process. Scribe logging must be disabled explicitly on startup, otherwise it is
/// on by default.
pub fn disable() {
    SCRIBE_ENABLED.store(false, Ordering::Relaxed);
}

pub fn scribe_category() -> anyhow::Result<String> {
    const DEFAULT_SCRIBE_CATEGORY: &str = "buck2_events";
    // Note that both daemon and client are emitting events, and that changing this variable has
    // no effect on the daemon until buckd is restarted but has effect on the client.
    static SCRIBE_CATEGORY: EnvHelper<String> = EnvHelper::new("BUCK2_SCRIBE_CATEGORY");
    Ok(SCRIBE_CATEGORY
        .get()?
        .map_or_else(|| DEFAULT_SCRIBE_CATEGORY.to_owned(), |c| c.clone()))
}
