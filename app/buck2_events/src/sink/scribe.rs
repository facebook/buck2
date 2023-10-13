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

    use std::sync::Arc;
    use std::time::Duration;
    use std::time::SystemTime;

    use buck2_data::InstantEvent;
    use buck2_data::Location;
    use buck2_data::StructuredError;
    use buck2_util::truncate::truncate;
    use fbinit::FacebookInit;
    use prost::Message;

    use crate::metadata;
    use crate::BuckEvent;
    use crate::Event;
    use crate::EventSink;
    use crate::EventSinkStats;
    use crate::EventSinkWithStats;
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
            self.send_messages_now(vec![event]).await;
        }

        // Send multiple events now, bypassing internal message queue.
        pub async fn send_messages_now(&self, events: Vec<BuckEvent>) {
            let messages = events
                .into_iter()
                .filter_map(|e| {
                    let message_key = e.trace_id().unwrap().hash();
                    Self::encode_message(e, false).map(|bytes| scribe_client::Message {
                        category: self.category.clone(),
                        message: bytes,
                        message_key: Some(message_key),
                    })
                })
                .collect();
            self.client.send_messages_now(messages).await;
        }

        // Send this event by placing it on the internal message queue.
        pub fn offer(&self, event: BuckEvent) {
            let message_key = event.trace_id().unwrap().hash();
            if let Some(bytes) = Self::encode_message(event, false) {
                self.client.offer(scribe_client::Message {
                    category: self.category.clone(),
                    message: bytes,
                    message_key: Some(message_key),
                });
            }
        }

        // Encodes message into something scribe understands.
        fn encode_message(mut event: BuckEvent, is_truncated: bool) -> Option<Vec<u8>> {
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

                Self::encode_message(
                    BuckEvent::new(
                        SystemTime::now(),
                        TraceId::new(),
                        None,
                        None,
                        buck2_data::buck_event::Data::Instant(InstantEvent {
                            data: Some(
                                StructuredError {
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
                                    task: Some(true),
                                    soft_error_category: Some("oversized_scribe".to_owned()),
                                    daemon_in_memory_state_is_corrupted: false,
                                    daemon_materializer_state_is_corrupted: false,
                                    action_cache_is_corrupted: false,
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
                            Self::truncate_action_execution_end(action_execution);
                        }
                        Some(Data::Command(ref mut command_end)) => {
                            Self::truncate_command_end(command_end, false);
                        }
                        Some(Data::TestEnd(ref mut test_end)) => {
                            Self::truncate_test_end(test_end);
                        }
                        _ => {}
                    };
                }
                Data::Instant(ref mut inst) => {
                    use buck2_data::instant_event::Data;
                    match &mut inst.data {
                        Some(Data::TestResult(ref mut test_result)) => {
                            Self::truncate_test_result(test_result);
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
                            Self::truncate_file_watcher_stats(file_watcher_stats);
                        }
                        if let Some(ref mut resolved_target_patterns) =
                            invocation_record.parsed_target_patterns
                        {
                            Self::truncate_target_patterns(
                                &mut resolved_target_patterns.target_patterns,
                            );
                            // Clear `unresolved_traget_patterns` to save bandwidth. It has less information
                            // than `resolved` one does, and will never be used if `resolved` one is available.
                            if let Some(ref mut command_end) = invocation_record.command_end {
                                Self::truncate_command_end(command_end, true);
                            }
                        } else if let Some(ref mut command_end) = invocation_record.command_end {
                            Self::truncate_command_end(command_end, false);
                        }

                        const MAX_CLI_ARGS_BYTES: usize = 512 * 1024;
                        let orig_len = invocation_record.cli_args.len();
                        let mut bytes: usize = 0;
                        for (index, arg) in invocation_record.cli_args.iter().enumerate() {
                            bytes += arg.len();
                            if bytes > MAX_CLI_ARGS_BYTES {
                                invocation_record.cli_args.truncate(index);
                                invocation_record.cli_args.push(format!(
                                    "<<Truncated (reported {} / {})>>",
                                    index, orig_len
                                ));
                                break;
                            }
                        }
                    }
                }
                _ => {}
            };
        }

        fn truncate_action_execution_end(
            action_execution_end: &mut buck2_data::ActionExecutionEnd,
        ) {
            // truncate(...) can panic if asked to truncate too short.
            const MIN_CMD_TRUNCATION: usize = 20;
            let per_command_size_budget =
                ((500 * 1024) / action_execution_end.commands.len().max(1)).max(MIN_CMD_TRUNCATION);

            let truncate_cmd = |cmd: &mut buck2_data::CommandExecution, truncate_all: bool| {
                if let Some(details) = &mut cmd.details {
                    details.stderr = if truncate_all {
                        "<<omitted>>".to_owned()
                    } else {
                        truncate(&details.stderr, per_command_size_budget)
                    };
                }
            };

            if let Some((last_command, retries)) = action_execution_end.commands.split_last_mut() {
                for retried in retries {
                    truncate_cmd(retried, false);
                }
                // Current Scribe tailers don't read stderr of successful actions.
                // Save some bytes.
                truncate_cmd(last_command, !action_execution_end.failed);
            }
        }

        fn truncate_command_end(
            command_end: &mut buck2_data::CommandEnd,
            clear_target_patterns: bool,
        ) {
            use buck2_data::command_end::Data;

            if let Some(ref mut target_patterns) = match &mut command_end.data {
                Some(Data::Build(build_command_end)) => {
                    Some(&mut build_command_end.unresolved_target_patterns)
                }
                Some(Data::Test(test_command_end)) => {
                    Some(&mut test_command_end.unresolved_target_patterns)
                }
                Some(Data::Install(install_command_end)) => {
                    Some(&mut install_command_end.unresolved_target_patterns)
                }
                Some(Data::Targets(targets_command_end)) => {
                    Some(&mut targets_command_end.unresolved_target_patterns)
                }
                _ => None,
            } {
                if clear_target_patterns {
                    target_patterns.clear();
                } else {
                    Self::truncate_target_patterns(target_patterns);
                }
            }
        }

        fn truncate_file_watcher_stats(file_watcher_stats: &mut buck2_data::FileWatcherStats) {
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

        fn truncate_test_result(test_result: &mut buck2_data::TestResult) {
            const TRUNCATED_DETAILS_LENGTH: usize = 512 * 1024; // 512Kb
            test_result.details = truncate(&test_result.details, TRUNCATED_DETAILS_LENGTH);
        }

        fn truncate_test_end(test_end: &mut buck2_data::TestRunEnd) {
            const MAX_TEST_NAMES_BYTES: usize = 512 * 1024;
            if let Some(ref mut suite) = test_end.suite {
                let orig_len = suite.test_names.len();
                let mut bytes: usize = 0;
                for (index, test_name) in suite.test_names.iter().enumerate() {
                    bytes += test_name.len();
                    if bytes > MAX_TEST_NAMES_BYTES {
                        suite.test_names.truncate(index);
                        let warn = format!("<<Truncated (reported {} / {})>>", index, orig_len);
                        suite.test_names.push(warn);
                        break;
                    }
                }
            }
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
        fn send(&self, event: Event) {
            match event {
                Event::Buck(event) => {
                    if should_send_event(event.data()) {
                        self.offer(event);
                    }
                }
                Event::CommandResult(..) => {}
                Event::PartialResult(..) => {}
            }
        }
    }

    impl EventSinkWithStats for ThriftScribeSink {
        fn to_event_sync(self: Arc<Self>) -> Arc<dyn EventSink> {
            self as _
        }

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
                    Some(Data::Command(..)) => true,
                    Some(Data::ActionExecution(a)) => match ActionKind::from_i32(a.kind) {
                        // Simple actions are not useful for most log analysis cases
                        Some(ActionKind::Copy)
                        | Some(ActionKind::SymlinkedDir)
                        | Some(ActionKind::Write)
                        | Some(ActionKind::WriteMacrosToFile) => false,
                        _ => true,
                    },
                    None => false,
                    _ => false,
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
                    Some(Data::Load(..)) => true,
                    Some(Data::CacheUpload(..)) => true,
                    Some(Data::Materialization(..)) => true,
                    Some(Data::TestDiscovery(..)) => true,
                    Some(Data::TestEnd(..)) => true,
                    None => false,
                    _ => false,
                }
            }
            Data::Instant(i) => {
                use buck2_data::instant_event::Data;

                match i.data {
                    Some(Data::BuildGraphInfo(..)) => true,
                    Some(Data::RageResult(..)) => true,
                    Some(Data::ReSession(..)) => true,
                    Some(Data::StructuredError(..)) => true,
                    Some(Data::TestResult(..)) => true,
                    Some(Data::PersistSubprocess(..)) => true,
                    None => false,
                    _ => false,
                }
            }
            Data::Record(r) => {
                use buck2_data::record_event::Data;

                match r.data {
                    Some(Data::InvocationRecord(..)) => true,
                    Some(Data::BuildGraphStats(..)) => true,
                    None => false,
                }
            }
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        fn make_invocation_record(
            data: buck2_data::InvocationRecord,
        ) -> buck2_data::buck_event::Data {
            buck2_data::buck_event::Data::Record(buck2_data::RecordEvent {
                data: Some(buck2_data::record_event::Data::InvocationRecord(Box::new(
                    data,
                ))),
            })
        }

        fn make_action_execution_end(
            data: buck2_data::ActionExecutionEnd,
        ) -> buck2_data::buck_event::Data {
            buck2_data::buck_event::Data::SpanEnd(buck2_data::SpanEndEvent {
                data: Some(buck2_data::span_end_event::Data::ActionExecution(Box::new(
                    data,
                ))),
                ..Default::default()
            })
        }

        fn make_command_end(data: buck2_data::CommandEnd) -> buck2_data::buck_event::Data {
            buck2_data::buck_event::Data::SpanEnd(buck2_data::SpanEndEvent {
                data: Some(buck2_data::span_end_event::Data::Command(data)),
                ..Default::default()
            })
        }

        fn make_build_command_end(
            unresolved_target_patterns: Vec<buck2_data::TargetPattern>,
        ) -> buck2_data::CommandEnd {
            buck2_data::CommandEnd {
                data: Some(buck2_data::command_end::Data::Build(
                    buck2_data::BuildCommandEnd {
                        unresolved_target_patterns,
                    },
                )),
                ..Default::default()
            }
        }

        fn make_test_end(data: buck2_data::TestRunEnd) -> buck2_data::buck_event::Data {
            buck2_data::buck_event::Data::SpanEnd(buck2_data::SpanEndEvent {
                data: Some(buck2_data::span_end_event::Data::TestEnd(data)),
                ..Default::default()
            })
        }

        fn make_command_execution_with_stderr(stderr: String) -> buck2_data::CommandExecution {
            buck2_data::CommandExecution {
                details: Some(buck2_data::CommandExecutionDetails {
                    stderr,
                    ..Default::default()
                }),
                ..Default::default()
            }
        }

        #[test]
        fn smart_truncate_resolved_target_patterns_clears_unresolved_one() {
            let mut record = buck2_data::InvocationRecord::default();
            let mut record_expected = record.clone();

            let resolved_target_patterns = vec![buck2_data::TargetPattern {
                value: "some_resolved_target".to_owned(),
            }];
            record.parsed_target_patterns = Some(buck2_data::ParsedTargetPatterns {
                target_patterns: resolved_target_patterns.clone(),
            });
            // resolved_target_patterns is expected to be unchanged.
            record_expected.parsed_target_patterns = Some(buck2_data::ParsedTargetPatterns {
                target_patterns: resolved_target_patterns,
            });

            let unresolved_target_patterns = vec![buck2_data::TargetPattern {
                value: "some_unresolved_target".to_owned(),
            }];
            record.command_end = Some(make_build_command_end(unresolved_target_patterns));

            // unresolved_target_patterns is expected to be empty.
            record_expected.command_end = Some(make_build_command_end(vec![]));

            let mut event_data = make_invocation_record(record);
            let event_data_expected = make_invocation_record(record_expected);

            ThriftScribeSink::smart_truncate_event(&mut event_data);

            assert_eq!(event_data, event_data_expected);
        }

        #[test]
        fn smart_truncate_unresolved_target_used_when_resolved_one_unavailable() {
            let mut record = buck2_data::InvocationRecord::default();
            let mut record_expected = record.clone();

            record.parsed_target_patterns = None;
            record_expected.parsed_target_patterns = None;

            let unresolved_target_patterns = vec![buck2_data::TargetPattern {
                value: "some_unresolved_target".to_owned(),
            }];
            let command_end = make_build_command_end(unresolved_target_patterns);

            record.command_end = Some(command_end.clone());
            // unresolved_target_patterns is expected to be unchanged.
            record_expected.command_end = Some(command_end);

            let mut event_data = make_invocation_record(record);
            let event_data_expected = make_invocation_record(record_expected);

            ThriftScribeSink::smart_truncate_event(&mut event_data);

            assert_eq!(event_data, event_data_expected);
        }

        #[test]
        fn smart_truncate_action_execution_end_one_last_command_truncated() {
            let command_execution_with_stderr =
                make_command_execution_with_stderr("this is a test".to_owned());
            let command_execution_stderr_omitted =
                make_command_execution_with_stderr("<<omitted>>".to_owned());

            let action_execution_end_with_stderrs = buck2_data::ActionExecutionEnd {
                commands: vec![command_execution_with_stderr],
                ..Default::default()
            };
            let action_execution_end_last_stderr_omitted = buck2_data::ActionExecutionEnd {
                commands: vec![command_execution_stderr_omitted],
                ..Default::default()
            };
            let mut event_data = make_action_execution_end(action_execution_end_with_stderrs);
            let event_data_expected =
                make_action_execution_end(action_execution_end_last_stderr_omitted);

            ThriftScribeSink::smart_truncate_event(&mut event_data);

            assert_eq!(event_data, event_data_expected);
        }

        #[test]
        fn smart_truncate_action_execution_end_long_stderr_command_truncated() {
            let command_execution_with_stderr =
                make_command_execution_with_stderr("this is a test".to_owned());
            let mut over_sized_str = "0123456789".repeat(10 * 1024);
            over_sized_str.push_str("0123456789"); // 100k + 10; 10-byte over
            let command_execution_with_long_stderr =
                make_command_execution_with_stderr(over_sized_str);
            let mut omitted_str = "0123456789".repeat(10 * 1024);
            omitted_str.replace_range((50 * 1024 - 6)..(50 * 1024 + 6), "<<omitted>>");
            let command_execution_stderr_partially_omitted =
                make_command_execution_with_stderr(omitted_str);
            let command_execution_stderr_all_omitted =
                make_command_execution_with_stderr("<<omitted>>".to_owned());

            let action_execution_end_with_stderrs = buck2_data::ActionExecutionEnd {
                commands: vec![
                    command_execution_with_stderr.clone(),
                    command_execution_with_long_stderr.clone(),
                    command_execution_with_stderr.clone(),
                    command_execution_with_long_stderr,
                    command_execution_with_stderr.clone(),
                ],
                ..Default::default()
            };
            let action_execution_end_last_stderr_omitted = buck2_data::ActionExecutionEnd {
                commands: vec![
                    command_execution_with_stderr.clone(),
                    command_execution_stderr_partially_omitted.clone(),
                    command_execution_with_stderr,
                    command_execution_stderr_partially_omitted,
                    command_execution_stderr_all_omitted,
                ],
                ..Default::default()
            };
            let mut event_data = make_action_execution_end(action_execution_end_with_stderrs);
            let event_data_expected =
                make_action_execution_end(action_execution_end_last_stderr_omitted);

            ThriftScribeSink::smart_truncate_event(&mut event_data);

            assert_eq!(event_data, event_data_expected);
        }

        #[test]
        fn smart_truncate_build_command_end_short_target_patterns_not_truncated() {
            let unresolved_target_patterns = vec![
                buck2_data::TargetPattern {
                    value: "hello".to_owned(),
                },
                buck2_data::TargetPattern {
                    value: "world".to_owned(),
                },
                buck2_data::TargetPattern {
                    value: "!\n".to_owned(),
                },
            ];
            let command_end = make_build_command_end(unresolved_target_patterns);

            let mut event_data = make_command_end(command_end);
            let event_data_expected = event_data.clone();

            ThriftScribeSink::smart_truncate_event(&mut event_data);

            assert_eq!(event_data, event_data_expected);
        }

        #[test]
        fn smart_truncate_build_command_end_long_target_patterns_truncated() {
            let unresolved_target_patterns = vec![
                buck2_data::TargetPattern {
                    value: "0123456789".repeat(20 * 1024),
                },
                buck2_data::TargetPattern {
                    value: "0123456789".repeat(20 * 1024),
                },
                buck2_data::TargetPattern {
                    value: "0123456789".repeat(20 * 1024), // 600k in total; 88k-byte over
                },
            ];
            let command_end = make_build_command_end(unresolved_target_patterns);

            let unresolved_target_patterns_truncated = vec![
                buck2_data::TargetPattern {
                    value: "0123456789".repeat(20 * 1024),
                },
                buck2_data::TargetPattern {
                    value: "0123456789".repeat(20 * 1024),
                },
                buck2_data::TargetPattern {
                    value: "<<Truncated (reported 2 / 3)>>".to_owned(),
                },
            ];
            let command_end_truncated =
                make_build_command_end(unresolved_target_patterns_truncated);

            let mut event_data = make_command_end(command_end);
            let event_data_expected = make_command_end(command_end_truncated);

            ThriftScribeSink::smart_truncate_event(&mut event_data);

            assert_eq!(event_data, event_data_expected);
        }

        #[test]
        fn smart_truncate_long_file_watcher_stats_truncated() {
            let file_watcher_event = buck2_data::FileWatcherEvent {
                path: "0123456789".repeat(3 * 1024),
                ..Default::default()
            };
            let file_watcher_stats = buck2_data::FileWatcherStats {
                events: vec![
                    file_watcher_event.clone(),
                    file_watcher_event.clone(),
                    file_watcher_event.clone(),
                    file_watcher_event.clone(), // 120k in total; 20k-byte over
                ],
                ..Default::default()
            };
            let file_watcher_stats_truncated = buck2_data::FileWatcherStats {
                events: vec![
                    file_watcher_event.clone(),
                    file_watcher_event.clone(),
                    file_watcher_event,
                ],
                incomplete_events_reason: Some(format!(
                    "Too long file change records ({} bytes, max {} bytes)",
                    120 * 1024,
                    100 * 1024
                )),
                ..Default::default()
            };
            let record = buck2_data::InvocationRecord {
                file_watcher_stats: Some(file_watcher_stats),
                ..Default::default()
            };
            let record_truncated = buck2_data::InvocationRecord {
                file_watcher_stats: Some(file_watcher_stats_truncated),
                ..Default::default()
            };
            let mut event_data = make_invocation_record(record);
            let event_data_expected = make_invocation_record(record_truncated);

            ThriftScribeSink::smart_truncate_event(&mut event_data);

            assert_eq!(event_data, event_data_expected);
        }

        #[test]
        fn smart_truncate_short_file_watcher_stats_not_truncated() {
            let file_watcher_event = buck2_data::FileWatcherEvent {
                path: "this is a test".to_owned(),
                ..Default::default()
            };
            let file_watcher_stats = buck2_data::FileWatcherStats {
                events: vec![
                    file_watcher_event.clone(),
                    file_watcher_event.clone(),
                    file_watcher_event,
                ],
                ..Default::default()
            };
            let record = buck2_data::InvocationRecord {
                file_watcher_stats: Some(file_watcher_stats),
                ..Default::default()
            };
            let mut event_data = make_invocation_record(record);
            let event_data_expected = event_data.clone();

            ThriftScribeSink::smart_truncate_event(&mut event_data);

            assert_eq!(event_data, event_data_expected);
        }

        #[test]
        fn smart_truncate_invocation_record_long_cli_args_truncated() {
            let cli_args = vec![
                "0123456789".repeat(20 * 1024),
                "0123456789".repeat(20 * 1024),
                "0123456789".repeat(20 * 1024), // 600k in total; 88k-byte over
            ];
            let cli_args_truncated = vec![
                "0123456789".repeat(20 * 1024),
                "0123456789".repeat(20 * 1024),
                "<<Truncated (reported 2 / 3)>>".to_owned(),
            ];

            let record = buck2_data::InvocationRecord {
                cli_args,
                ..Default::default()
            };
            let record_truncated = buck2_data::InvocationRecord {
                cli_args: cli_args_truncated,
                ..Default::default()
            };

            let mut event_data = make_invocation_record(record);
            let event_data_expected = make_invocation_record(record_truncated);

            ThriftScribeSink::smart_truncate_event(&mut event_data);

            assert_eq!(event_data, event_data_expected);
        }

        #[test]
        fn smart_truncate_invocation_record_short_cli_args_truncated() {
            let cli_args = vec!["this is".to_owned(), "a test".to_owned()];

            let record = buck2_data::InvocationRecord {
                cli_args,
                ..Default::default()
            };

            let mut event_data = make_invocation_record(record);
            let event_data_expected = event_data.clone();

            ThriftScribeSink::smart_truncate_event(&mut event_data);

            assert_eq!(event_data, event_data_expected);
        }

        #[test]
        fn smart_truncate_test_end_long_test_names_truncated() {
            let test_names = vec![
                "0123456789".repeat(20 * 1024),
                "0123456789".repeat(20 * 1024),
                "0123456789".repeat(20 * 1024), // 600k in total; 88k-byte over
            ];
            let test_names_truncated = vec![
                "0123456789".repeat(20 * 1024),
                "0123456789".repeat(20 * 1024),
                "<<Truncated (reported 2 / 3)>>".to_owned(),
            ];

            let test_end = buck2_data::TestRunEnd {
                suite: Some(buck2_data::TestSuite {
                    test_names,
                    ..Default::default()
                }),
                ..Default::default()
            };
            let test_end_truncated = buck2_data::TestRunEnd {
                suite: Some(buck2_data::TestSuite {
                    test_names: test_names_truncated,
                    ..Default::default()
                }),
                ..Default::default()
            };

            let mut event_data = make_test_end(test_end);
            let event_data_expected = make_test_end(test_end_truncated);

            ThriftScribeSink::smart_truncate_event(&mut event_data);

            assert_eq!(event_data, event_data_expected);
        }
    }
}

#[cfg(not(fbcode_build))]
mod fbcode {
    use std::sync::Arc;

    use crate::BuckEvent;
    use crate::Event;
    use crate::EventSink;
    use crate::EventSinkStats;
    use crate::EventSinkWithStats;

    pub struct ThriftScribeSink;

    impl ThriftScribeSink {
        pub async fn send_now(&self, _event: BuckEvent) {}
        pub async fn send_messages_now(&self, _events: Vec<BuckEvent>) {}
    }

    impl EventSink for ThriftScribeSink {
        fn send(&self, _event: Event) {}
    }

    impl EventSinkWithStats for ThriftScribeSink {
        fn to_event_sync(self: Arc<Self>) -> Arc<dyn EventSink> {
            self as _
        }

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
