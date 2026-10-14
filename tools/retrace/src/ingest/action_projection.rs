/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Test-only reference projection of a paired action start/end.
//!
//! Preserve the original borrowed field selection independently of the
//! production normalizer, so naming, missingness, hashes and command details
//! can be compared without merely testing the same implementation twice.
//!
//! Field selection, missingness, naming, hashing and cloning intentionally
//! match scan::project_action. This module is not linked into production.

use super::scan::ActionRec;
use super::scan::CmdRec;
use super::scan::LastCmd;
use super::scan::dur_us;
use super::scan::key_hash;
use super::scan::remote_cache_origin;
use super::scan::ts_us;

pub(super) struct ProjectedAction {
    /// `cfg` and `category` are placeholders, NOT valid interner assignments.
    /// The ordered reducer must replace both before publishing this record.
    pub record: ActionRec,
    pub cfg: String,
    pub category: String,
}

/// Project only a pair already accepted by the ordered reducer. Orphan counts,
/// capture detection and first-local-host selection remain reducer operations
/// and must happen even for ends which have no paired start.
pub(super) fn project_action(
    sid: u64,
    ts: i64,
    dur: i64,
    start: buck2_data::ActionExecutionStart,
    end: Box<buck2_data::ActionExecutionEnd>,
) -> ProjectedAction {
    let (full_target, short_at, cfg) = names_of(&start);
    let category = start
        .name
        .as_ref()
        .map(|n| n.category.clone())
        .unwrap_or_default();

    let cmds: Vec<CmdRec> = end
        .commands
        .iter()
        .map(|c| {
            let md = c.details.as_ref().and_then(|d| d.metadata.as_ref());
            CmdRec {
                status: status_name(c),
                wall_time_us: md.and_then(|m| m.wall_time.as_ref()).map(dur_us),
                start_us: md.and_then(|m| m.start_time.as_ref()).map(ts_us),
                client_start_us: md.and_then(|m| m.client_start_time.as_ref()).map(ts_us),
                elastic: c
                    .details
                    .as_ref()
                    .and_then(super::scan::re_worker)
                    .is_some_and(|w| w.elastic_capacity),
            }
        })
        .collect();

    let mut last = LastCmd::default();
    if let Some(d) = end.commands.last().and_then(|c| c.details.as_ref()) {
        let md = d.metadata.as_ref();
        let stats = md.and_then(|m| m.execution_stats.as_ref());
        last.signed_exit_code = d.signed_exit_code;
        last.exec_us = md.and_then(|m| m.execution_time.as_ref()).map(dur_us);
        last.queue_us = md.and_then(|m| m.queue_duration.as_ref()).map(dur_us);
        last.input_fetch_us = md
            .and_then(|m| m.input_materialization_duration.as_ref())
            .map(dur_us);
        let worker = super::scan::re_worker(d);
        last.upload_us = worker
            .and_then(|w| w.output_upload_us)
            .and_then(super::scan::us_i64);
        last.worker_wall_us = worker
            .and_then(|w| w.worker_wall_time_us)
            .and_then(super::scan::us_i64);
        last.worker_bytes_in = worker.and_then(|w| w.input_download_bytes);
        last.worker_bytes_out = worker.and_then(|w| w.output_upload_bytes);
        last.attempts = worker.and_then(|w| w.execution_attempts).filter(|n| *n > 1);
        last.final_queue_us = worker.and_then(|w| w.final_queue_us);
        last.hashing_us = md.and_then(|m| m.hashing_duration.as_ref()).map(dur_us);
        last.mem_peak = stats.and_then(|s| s.memory_peak);
        last.cpu_us = worker.and_then(super::scan::cpu_total_us);
        last.cpu_user_us = worker.and_then(|w| w.cpu_time_user_us);
        last.cpu_sys_us = worker.and_then(|w| w.cpu_time_system_us);
        last.elastic = worker.is_some_and(|w| w.elastic_capacity);
        if let Some(kind) = d.command_kind.as_ref() {
            use buck2_data::command_execution_kind::Command;
            last.hashing_local_outputs = matches!(
                &kind.command,
                Some(
                    Command::LocalCommand(_)
                        | Command::OmittedLocalCommand(_)
                        | Command::WorkerCommand(_)
                )
            );
            match &kind.command {
                Some(Command::LocalCommand(c)) => {
                    last.local_digest =
                        (!c.action_digest.is_empty()).then(|| c.action_digest.clone());
                    if !c.argv.is_empty() {
                        last.argv = Some(c.argv.clone());
                    }
                    last.cwd = c.working_directory.clone();
                    last.env_rule = c.environment_rule;
                }
                Some(Command::WorkerCommand(c)) if !c.argv.is_empty() => {
                    last.argv = Some(c.argv.clone());
                }
                Some(Command::WorkerInitCommand(c)) if !c.argv.is_empty() => {
                    last.argv = Some(c.argv.clone());
                }
                Some(Command::RemoteCommand(rc)) => {
                    last.remote = true;
                    // Enum zero alone does not establish reuse in old logs.
                    last.reused = rc.cache_hit || remote_cache_origin(rc.cache_hit_type);
                    last.remote_queue_us = rc.queue_time.as_ref().map(dur_us);
                    last.argv = Some(vec!["<remote>".to_owned(), rc.action_digest.clone()]);
                }
                Some(Command::OmittedLocalCommand(c)) => {
                    last.local_digest =
                        (!c.action_digest.is_empty()).then(|| c.action_digest.clone());
                }
                _ => {}
            }
        }
    }

    let digest = end.commands.iter().find_map(|c| {
        use buck2_data::command_execution_kind::Command;
        let kind = c.details.as_ref().and_then(|d| d.command_kind.as_ref())?;
        let d = match &kind.command {
            Some(Command::LocalCommand(x)) => &x.action_digest,
            Some(Command::RemoteCommand(x)) => &x.action_digest,
            Some(Command::WorkerCommand(x)) => &x.action_digest,
            Some(Command::OmittedLocalCommand(x)) => &x.action_digest,
            _ => return None,
        };
        (!d.is_empty()).then(|| d.clone())
    });

    let no_producers = buck2_data::ActionGraphProducers::default();
    let producers = end.graph_producers.as_ref().unwrap_or(&no_producers);
    ProjectedAction {
        record: ActionRec {
            sid,
            ts,
            dur,
            full_target,
            short_at,
            cfg: 0,
            category: 0,
            key_hash: start.key.as_ref().map(key_hash),
            kind: start.kind,
            failed: end.failed,
            error_kind: super::scan::action_error_kind(&end),
            execution_kind: end.execution_kind,
            cache_upload_result: end.cache_upload_result,
            dep_file_cache_upload_result: end.dep_file_cache_upload_result,
            wall_time_us: end.wall_time.as_ref().map(dur_us),
            prepare_us: end.prepare_command_us,
            finish_us: end.finish_action_us,
            output_size: end.output_size,
            input_files_bytes: end.input_files_bytes,
            invalidated: end.invalidation_info.is_some(),
            cmds,
            last,
            digest,
            execution: None,
            dep_span_ids: producers.span_ids.clone(),
            dep_action_keys: producers
                .reused_ids
                .iter()
                .map(|&id| u128::from(id))
                .collect(),
            dep_tset_ids: producers.tset_ids.clone(),
        },
        cfg,
        category,
    }
}

fn names_of(start: &buck2_data::ActionExecutionStart) -> (String, u32, String) {
    let what = match &start.name {
        Some(n) => format!("{} {}", n.category, n.identifier).trim().to_owned(),
        None => String::new(),
    };
    let (pkg, tname, cfg) = target_of(start);
    let short_at = u32::try_from(pkg.len() + 1).expect("package paths are far below 4 GiB");
    (format!("{pkg}:{tname} ({what})"), short_at, cfg)
}

fn target_of(start: &buck2_data::ActionExecutionStart) -> (String, String, String) {
    use buck2_data::action_key::Owner;
    let owner = start.key.as_ref().and_then(|k| k.owner.as_ref());
    match owner {
        Some(Owner::TargetLabel(c)) => {
            let (pkg, name) = match &c.label {
                Some(l) => (l.package.clone(), l.name.clone()),
                None => ("?".to_owned(), "?".to_owned()),
            };
            let cfg = c
                .configuration
                .as_ref()
                .map(|x| x.full_name.clone())
                .unwrap_or_default();
            (pkg, name, cfg)
        }
        Some(Owner::BxlKey(..)) => ("?".to_owned(), "BxlKey".to_owned(), String::new()),
        Some(Owner::TestTargetLabel(..)) => {
            ("?".to_owned(), "TestTargetLabel".to_owned(), String::new())
        }
        Some(Owner::AnonTarget(..)) => ("?".to_owned(), "AnonTarget".to_owned(), String::new()),
        Some(Owner::LocalResourceSetup(..)) => (
            "?".to_owned(),
            "LocalResourceSetup".to_owned(),
            String::new(),
        ),
        None => ("?".to_owned(), "?".to_owned(), String::new()),
    }
}

fn status_name(c: &buck2_data::CommandExecution) -> &'static str {
    use buck2_data::command_execution::Status;
    match &c.status {
        Some(Status::Success(..)) => "Success",
        Some(Status::Failure(..)) => "Failure",
        Some(Status::Timeout(..)) => "Timeout",
        Some(Status::Error(..)) => "Error",
        Some(Status::Cancelled(..)) => "Cancelled",
        Some(Status::WorkerFailure(..)) => "WorkerFailure",
        None => "Unknown",
    }
}
