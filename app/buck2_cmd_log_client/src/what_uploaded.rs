/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fmt::Display;
use std::fmt::Formatter;
use std::io::Write;

use buck2_client_ctx::client_ctx::BuckSubcommand;
use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::common::BuckArgMatches;
use buck2_client_ctx::event_log_options::EventLogOptions;
use buck2_client_ctx::events_ctx::EventsCtx;
use buck2_client_ctx::exit_result::ClientIoError;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_data::ReUploadMetrics;
use buck2_event_log::stream_value::StreamValue;
use buck2_event_observer::display;
use buck2_event_observer::display::TargetDisplayOptions;
use buck2_event_observer::humanized::HumanizedBytes;
use buck2_hash::BuckMutMap;
use tokio_stream::StreamExt;

use crate::LogCommandOutputFormat;
use crate::LogCommandOutputFormatWithWriter;
use crate::transform_format;

/// Get what uploaded to RE/CAS from a buck invocation
///
/// Outputs a tab-separated table of: kind, action, action_digest, digests_uploaded, bytes_uploaded.
///
///   kind          action                                     act. digest  #digests #bytes
///   re_inputs     root//foo:bar (cfg) (cxx_compile foo.cpp)                  12    4096
///   action_cache  root//foo:bar (cfg) (cxx_compile foo.cpp)  3f8a9cabcd:142   3   20480
///
/// Kinds:
/// - re_inputs - inputs to a remotely executed action, without any digest
/// - action_cache - locally-run action whose outputs were uploaded to action cache, with action
///                  digest.
#[derive(Debug, clap::Parser)]
#[command(verbatim_doc_comment)]
pub struct WhatUploadedCommand {
    #[clap(flatten)]
    event_log: EventLogOptions,
    #[clap(flatten)]
    output: LogCommandOutputFormat,
    #[clap(
        long = "aggregate-by-ext",
        help = "Aggregates input uploads by file extension. Excludes action-cache uploads"
    )]
    aggregate_by_extension: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize)]
#[serde(rename_all = "snake_case")]
enum UploadKind {
    ReInputs,
    ActionCache,
}

impl Display for UploadKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::ReInputs => "re_inputs",
            Self::ActionCache => "action_cache",
        })
    }
}

#[derive(Debug, PartialEq, Eq, serde::Serialize)]
struct ActionRecord {
    kind: UploadKind,
    action: String,
    /// Absent for input uploads because `ReUploadEnd` does not include the action digest.
    action_digest: Option<String>,
    digests_uploaded: u64,
    bytes_uploaded: u64,
}

impl Display for ActionRecord {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}\t{}\t{}\t{}\t{}",
            self.kind,
            self.action,
            self.action_digest.as_deref().unwrap_or_default(),
            self.digests_uploaded,
            self.bytes_uploaded
        )
    }
}

#[derive(serde::Serialize)]
struct ExtensionRecord {
    extension: String,
    digests_uploaded: u64,
    bytes_uploaded: u64,
}

impl Display for ExtensionRecord {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}\t{}\t{}",
            self.extension, self.digests_uploaded, self.bytes_uploaded
        )
    }
}

fn action_identity(
    key: Option<&buck2_data::ActionKey>,
    name: Option<&buck2_data::ActionName>,
) -> String {
    display::display_action_identity(key, name, TargetDisplayOptions::for_log())
        .unwrap_or_else(|_| "unknown action".to_owned())
}

fn re_upload_record(
    actions: &BuckMutMap<u64, buck2_data::ActionExecutionStart>,
    upload: &ReUploadEvent,
) -> ActionRecord {
    let action = match actions.get(&upload.parent_span_id) {
        Some(action) => action_identity(action.key.as_ref(), action.name.as_ref()),
        None => "unknown action".to_owned(),
    };
    ActionRecord {
        kind: UploadKind::ReInputs,
        action,
        action_digest: None,
        digests_uploaded: upload.inner.digests_uploaded.unwrap_or_default(),
        bytes_uploaded: upload.inner.bytes_uploaded.unwrap_or_default(),
    }
}

/// Returns `None` for failed attempts because they did not upload data.
fn cache_upload_record(upload: &buck2_data::CacheUploadEnd) -> Option<ActionRecord> {
    if !upload.success {
        return None;
    }

    Some(ActionRecord {
        kind: UploadKind::ActionCache,
        action: action_identity(upload.key.as_ref(), upload.name.as_ref()),
        action_digest: Some(upload.action_digest.clone()),
        digests_uploaded: (upload.file_digests.len() + upload.tree_digests.len()) as u64,
        bytes_uploaded: upload.output_bytes.unwrap_or_default(),
    })
}

fn write_record<T: Display + serde::Serialize>(
    output: &mut LogCommandOutputFormatWithWriter,
    record: &T,
) -> Result<(), ClientIoError> {
    match output {
        LogCommandOutputFormatWithWriter::Readable(w)
        | LogCommandOutputFormatWithWriter::Tabulated(w) => Ok(writeln!(w, "{record}")?),
        LogCommandOutputFormatWithWriter::Csv(writer) => Ok(writer.serialize(record)?),
        LogCommandOutputFormatWithWriter::Json(w) => {
            serde_json::to_writer(w.by_ref(), record)?;
            w.write_all(b"\n")?;
            Ok(())
        }
    }
}

fn write_extension_stats(
    output: &mut LogCommandOutputFormatWithWriter,
    stats_by_extension: &BuckMutMap<String, ReUploadMetrics>,
) -> Result<(), ClientIoError> {
    let mut records: Vec<ExtensionRecord> = stats_by_extension
        .iter()
        .map(|(ext, m)| ExtensionRecord {
            extension: ext.to_owned(),
            bytes_uploaded: m.bytes_uploaded,
            digests_uploaded: m.digests_uploaded,
        })
        .collect();
    records.sort_by_key(|a| a.bytes_uploaded);
    for record in records {
        write_record(output, &record)?;
    }
    Ok(())
}

struct ReUploadEvent<'a> {
    pub parent_span_id: u64,
    pub inner: &'a buck2_data::ReUploadEnd,
}

impl BuckSubcommand for WhatUploadedCommand {
    const COMMAND_NAME: &'static str = "log-what-uploaded";

    async fn exec_impl(
        self,
        _matches: BuckArgMatches<'_>,
        ctx: ClientCommandContext<'_>,
        _events_ctx: &mut EventsCtx,
    ) -> ExitResult {
        let Self {
            event_log,
            output,
            aggregate_by_extension,
        } = self;

        buck2_client_ctx::stdio::print_with_writer::<buck2_error::Error, _>(async move |w| {
            let mut output = transform_format(output, w);
            let log_path = event_log.get(&ctx).await?;

            let (invocation, mut events) = log_path.unpack_stream().await?;
            buck2_client_ctx::eprintln!(
                "Showing uploads from: {}",
                invocation.display_command_line()
            )?;

            let mut total_digests_uploaded = 0;
            let mut total_bytes_uploaded = 0;
            let mut failed_cache_uploads = 0;
            let mut cache_uploads_without_extensions = 0;
            let mut actions = BuckMutMap::default();
            let mut stats_by_extension: BuckMutMap<String, ReUploadMetrics> = BuckMutMap::default();
            while let Some(event) = events.try_next().await? {
                match event {
                    StreamValue::Event(event) => {
                        // Keep action spans so child upload events can be attributed.
                        if let Some(buck2_data::buck_event::Data::SpanStart(start)) = &event.data
                            && let Some(buck2_data::span_start_event::Data::ActionExecution(
                                action,
                            )) = &start.data
                        {
                            actions.insert(event.span_id, action.clone());
                        }

                        let Some(buck2_data::buck_event::Data::SpanEnd(end)) = &event.data else {
                            continue;
                        };
                        match end.data.as_ref() {
                            Some(buck2_data::span_end_event::Data::ReUpload(u)) => {
                                let upload = ReUploadEvent {
                                    parent_span_id: event.parent_id,
                                    inner: u,
                                };
                                if aggregate_by_extension {
                                    for (extension, metrics) in &upload.inner.stats_by_extension {
                                        let entry = stats_by_extension
                                            .entry(extension.to_owned())
                                            .or_default();
                                        entry.bytes_uploaded += metrics.bytes_uploaded;
                                        entry.digests_uploaded += metrics.digests_uploaded;
                                    }
                                } else {
                                    let record = re_upload_record(&actions, &upload);
                                    total_digests_uploaded += record.digests_uploaded;
                                    total_bytes_uploaded += record.bytes_uploaded;
                                    write_record(&mut output, &record)?;
                                }
                            }
                            Some(buck2_data::span_end_event::Data::CacheUpload(u)) => {
                                if aggregate_by_extension {
                                    // Cache-upload events contain digests but no paths from which
                                    // to derive extensions.
                                    if u.success {
                                        cache_uploads_without_extensions += 1;
                                    }
                                } else if let Some(record) = cache_upload_record(u) {
                                    total_digests_uploaded += record.digests_uploaded;
                                    total_bytes_uploaded += record.bytes_uploaded;
                                    write_record(&mut output, &record)?;
                                } else {
                                    failed_cache_uploads += 1;
                                }
                            }
                            _ => {}
                        }
                    }
                    StreamValue::Result(..) | StreamValue::PartialResult(..) => {}
                }
            }
            if aggregate_by_extension {
                write_extension_stats(&mut output, &stats_by_extension)?;
                if cache_uploads_without_extensions > 0 {
                    buck2_client_ctx::eprintln!(
                        "note: omitted {} action-cache uploads (file extensions unavailable)",
                        cache_uploads_without_extensions
                    )?;
                }
            } else {
                // Only the summary is humanized. The per-row byte counts stay
                // raw so that `--format csv`/`json` remain machine-readable and
                // `sort -k5 -rn` keeps working on the readable format.
                buck2_client_ctx::eprintln!(
                    "total: digests: {}, bytes: {} ({})",
                    total_digests_uploaded,
                    HumanizedBytes::new(total_bytes_uploaded),
                    total_bytes_uploaded
                )?;
                if failed_cache_uploads > 0 {
                    buck2_client_ctx::eprintln!(
                        "note: omitted {} failed action-cache uploads",
                        failed_cache_uploads
                    )?;
                }
            }

            Ok(())
        })
        .await?;
        ExitResult::success()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn action_key() -> buck2_data::ActionKey {
        buck2_data::ActionKey {
            owner: Some(buck2_data::action_key::Owner::TargetLabel(
                buck2_data::ConfiguredTargetLabel {
                    label: Some(buck2_data::TargetLabel {
                        package: "root//some/package".to_owned(),
                        name: "target".to_owned(),
                    }),
                    configuration: Some(buck2_data::Configuration {
                        full_name: "conf".to_owned(),
                    }),
                    ..Default::default()
                },
            )),
            ..Default::default()
        }
    }

    fn cache_upload() -> buck2_data::CacheUploadEnd {
        buck2_data::CacheUploadEnd {
            key: Some(action_key()),
            name: Some(buck2_data::ActionName {
                category: "cxx_compile".to_owned(),
                identifier: "foo.cpp".to_owned(),
            }),
            success: true,
            action_digest: "aa53d2bb64e3cc75f4:142".to_owned(),
            file_digests: vec!["aa:1".to_owned(), "bb:2".to_owned(), "cc:3".to_owned()],
            tree_digests: vec!["dd:4".to_owned(), "ee:5".to_owned()],
            output_bytes: Some(4096),
            ..Default::default()
        }
    }

    #[test]
    fn cache_upload_record_includes_action_and_metrics() {
        let record = cache_upload_record(&cache_upload()).unwrap();

        assert_eq!(record.kind, UploadKind::ActionCache);
        assert_eq!(
            record.action_digest.as_deref(),
            Some("aa53d2bb64e3cc75f4:142")
        );
        assert_eq!(record.digests_uploaded, 5);
        assert_eq!(record.bytes_uploaded, 4096);
        assert!(
            record.action.contains("root//some/package:target")
                && record.action.contains("cxx_compile"),
            "unexpected action identity: {}",
            record.action
        );
    }

    #[test]
    fn cache_upload_record_ignores_failures() {
        let rejected = buck2_data::CacheUploadEnd {
            success: false,
            error: "permission denied".to_owned(),
            re_error_code: Some("PERMISSION_DENIED".to_owned()),
            ..cache_upload()
        };

        assert!(cache_upload_record(&rejected).is_none());
    }

    #[test]
    fn cache_upload_record_defaults_missing_metrics() {
        let record = cache_upload_record(&buck2_data::CacheUploadEnd {
            file_digests: vec![],
            tree_digests: vec![],
            output_bytes: None,
            ..cache_upload()
        })
        .unwrap();

        assert_eq!(record.digests_uploaded, 0);
        assert_eq!(record.bytes_uploaded, 0);
    }

    #[test]
    fn re_upload_record_has_no_action_digest() {
        let record = re_upload_record(
            &BuckMutMap::default(),
            &ReUploadEvent {
                parent_span_id: 1,
                inner: &buck2_data::ReUploadEnd {
                    digests_uploaded: Some(7),
                    bytes_uploaded: Some(512),
                    ..Default::default()
                },
            },
        );

        assert_eq!(record.kind, UploadKind::ReInputs);
        assert_eq!(record.action_digest, None);
        assert_eq!(record.digests_uploaded, 7);
        assert_eq!(record.bytes_uploaded, 512);
        assert_eq!(record.to_string(), "re_inputs\tunknown action\t\t7\t512");
    }
}
