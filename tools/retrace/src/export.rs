/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! `--json` export and the `--stats` text: every analysis as
//! machine-readable JSON (the same functions the GUI panels call, so
//! the surfaces cannot disagree).
//!
//! Every topic travels in the light honesty envelope
//! (docs/contract.md): `status` available/partial/refused, an optional
//! `bound` direction, counted `coverage` (numerator/denominator, no
//! stored percentages), and machine-carried `limitations`. A refused
//! topic is an explicit envelope with a reason — never a silent
//! omission.

use std::cell::OnceCell;
use std::io::Read as _;
use std::io::Seek as _;

use serde_json::Map;
use serde_json::Value;
use serde_json::json;

use crate::analyses;
use crate::analyses::Facts;
use crate::dag;
use crate::detail::MAX_DETAIL_RECORD_BYTES;
use crate::graph::Graph;
use crate::quality;
use crate::quality::Claim;
use crate::quality::Dep;
pub(crate) use crate::quality::envelope;
use crate::types::F_CACHE_HIT;
use crate::types::F_FAILED;
use crate::types::Trace;

pub const TOPICS: &[&str] = &[
    "overview",
    "command-profile",
    "action-detail",
    "breakdown",
    "rule-sites",
    "peers",
    "stall",
    "critical-path",
    "longest",
    "top-memory",
    "re-queue",
    "admission",
    "retries",
    "health",
];

pub const DEFAULT_TOPK: usize = 20;

/// Inputs some topics need beyond the trace itself.
#[derive(Default)]
pub struct TopicArgs<'a> {
    /// Exact action display name or `action:N` (index in this loaded trace).
    pub action: Option<&'a str>,
}

/// The critical path buck2 reported during the build, for a capture without a dependency graph.
/// Action steps name the trace action they recorded, when the trace holds it.
fn reported_critical_path(tr: &Trace, path: &crate::command_profile::BuckCriticalPath) -> Value {
    let mut by_seq = vec![None; tr.acts.len()];
    for act in &tr.acts {
        if let Some(slot) = by_seq.get_mut(act.seq as usize) {
            *slot = Some(act.name);
        }
    }
    let steps: Vec<Value> = path
        .entries
        .iter()
        .map(|e| {
            let action = e
                .acts
                .first()
                .and_then(|&seq| by_seq.get(seq as usize).copied().flatten())
                .and_then(|name| tr.names.get(name as usize).cloned());
            json!({
                "kind": e.kind,
                "label": e.label,
                "action": action,
                "start_offset_us": e.start_offset_us,
                "duration_us": e.duration_us,
                "user_us": e.user_us,
                "potential_improvement_us": e.potential_improvement_us,
                "reused": e.reused,
            })
        })
        .collect();
    envelope(
        "available",
        None,
        Vec::new(),
        vec![
            "buck2's own critical-path model, reported during the build: the build was recorded \
             without --log-action-graph, so Retrace has no dependency graph to recompute it, \
             replay what-ifs or run the cascade"
                .into(),
        ],
        json!({
            "source": "buck2_reported",
            "backend": path.backend,
            "cp_us": path.model_duration_us(),
            "steps": steps,
        }),
    )
}

fn refused(limitations: Vec<String>) -> Value {
    envelope("refused", None, Vec::new(), limitations, Value::Null)
}

/// Selection stays graph-independent and never guesses between same-named
/// actions. Indices are local to this loaded trace, not stable build IDs.
fn detail_action(f: &Facts, query: &str) -> Result<u32, String> {
    if query.trim().is_empty() {
        return Err("--action requires an exact action name or action:N".into());
    }
    if let Some(index) = query.strip_prefix("action:") {
        return index
            .parse::<u32>()
            .ok()
            .and_then(|index| f.sel.iter().copied().find(|&e| f.ev[e as usize] == index))
            .ok_or_else(|| {
                "--action=action:N must name a recipe action in the selected run".into()
            });
    }
    let mut count = 0;
    let matches: Vec<_> = f
        .sel
        .iter()
        .copied()
        .filter(|&e| f.name(e) == query)
        .filter(|_| {
            count += 1;
            count <= 8
        })
        .collect();
    match matches.as_slice() {
        [e] => Ok(*e),
        [] => Err(format!(
            "No action has the exact name `{query}` in the selected run"
        )),
        _ => Err(format!(
            "Action name `{query}` is ambiguous ({count} actions); choose {}{} from this loaded trace",
            matches
                .iter()
                .take(8)
                .map(|&e| format!("action:{}", f.ev[e as usize]))
                .collect::<Vec<_>>()
                .join(", "),
            if count > matches.len() {
                format!(" (+{} more)", count - matches.len())
            } else {
                String::new()
            }
        )),
    }
}

fn detail_record(f: &Facts, e: u32) -> anyhow::Result<Value> {
    let a = f.act(e);
    anyhow::ensure!(
        a.len as usize <= MAX_DETAIL_RECORD_BYTES,
        "action record exceeds the {MAX_DETAIL_RECORD_BYTES}-byte detail limit"
    );
    let mut reader = f.tr.source_reader()?;
    reader.seek(std::io::SeekFrom::Start(a.off))?;
    let mut bytes = vec![0; a.len as usize];
    reader.read_exact(&mut bytes)?;
    let text = std::str::from_utf8(&bytes)?
        .trim_end()
        .trim_end_matches(',');
    let record: Value = serde_json::from_str(text)?;
    f.tr.validate_source()?;
    Ok(record)
}

fn action_detail(f: &Facts, query: Option<&str>, q: &quality::Quality) -> Value {
    let Some(query) = query else {
        return refused(vec!["action-detail requires --action=<exact name> or --action=action:N; no remote inputs or outputs are downloaded".into()]);
    };
    let e = match detail_action(f, query) {
        Ok(e) => e,
        Err(error) => return refused(vec![error]),
    };
    let record = match detail_record(f, e) {
        Ok(record) => record,
        Err(error) => return refused(vec![format!("Cannot read action detail: {error:#}")]),
    };
    let activity = crate::activity::Activity::from_record(&record);
    let context = crate::detail::PopupCtx {
        t0: f.tr.run_origin_us(f.run.as_deref()),
        ..Default::default()
    };
    let rows = crate::detail::popup_rows(&record, &context);
    let stage_lines = crate::detail::stage_lines(&record["args"], f.dur(e), None, None);
    let intervals = |intervals: &[crate::activity::Interval]| {
        intervals
            .iter()
            .map(|i| {
                json!({
                    "start_offset_us": i.start_us, "end_offset_us": i.end_us,
                })
            })
            .collect::<Vec<_>>()
    };
    let mut limits = activity.explanations.clone();
    limits.push("One source record, no remote downloads. Activity positions are offsets on the client clock; their union is observed coverage, not worker occupancy. Uncovered time has no measured cause. Stage counters can overlap and are not added to interval coverage. Build-wide comparison medians and modeled upload occupancy are not recomputed for this one-record report.".into());
    let complete = activity.has_complete_evidence();
    if !complete {
        limits.push(activity.evidence.label().to_owned());
    }
    q.seal(
        Dep::Trace,
        Claim::Measurement,
        if complete { "available" } else { "partial" },
        None,
        activity.covered_us.zip(activity.duration_us).map(|(n, d)| vec![("action-microseconds-covered-by-observations", n as u64, d as u64)]).unwrap_or_default(),
        limits,
        json!({
            "action_index": f.ev[e as usize],
            "action_sequence": f.act(e).seq,
            "target": f.name(e),
            "run": f.run,
            "start_us": f.tr.t0_abs.saturating_add(f.ts(e)),
            "duration_us": f.dur(e),
            "span_id": record["args"]["span_id"],
            "rows": rows.into_iter().map(|r| json!({"key": r.key, "value": r.value, "failed": r.failed})).collect::<Vec<_>>(),
            "stage_lines": stage_lines.map(|lines| lines.into_iter().map(|line| {
                let stage = line.stage.and_then(|i| crate::detail::STAGES.get(i));
                let provenance = line.provenance.or_else(|| stage.map(|s| s.prov));
                json!({
                    "stage": stage.map(|s| s.key),
                    "parent_stage": line.parent.and_then(|i| crate::detail::STAGES.get(i)).map(|s| s.key),
                    "text": line.text,
                    "provenance": provenance.map(|p| p.text()),
                    "explanation": line.hover,
                })
            }).collect::<Vec<_>>()),
            "activity": {
                "evidence": activity.evidence.label(),
                "complete_observation_list": complete,
                "clock": "client clock, offsets relative to action start",
                "covered_us": activity.covered_us,
                "uncovered_us": activity.uncovered_us,
                "rejected_spans": activity.rejected_spans,
                "clipped_spans": activity.clipped_spans,
                "diagnosis": activity.diagnosis,
                "highlights": activity.highlights,
                "spans": activity.spans.iter().map(|s| json!({
                    "name": s.name, "label": s.label, "lane": s.lane.label(),
                    "start_offset_us": s.start_us, "end_offset_us": s.end_us,
                    "cancelled": s.cancelled,
                })).collect::<Vec<_>>(),
                "covered": intervals(&activity.covered),
                "gaps": intervals(&activity.gaps),
            },
        }),
    )
}

fn ev_json(f: &Facts, e: u32) -> Value {
    let a = f.act(e);
    json!({
        "target": f.name(e),
        "start_us": f.tr.t0_abs.saturating_add(a.ts),
        "duration_us": a.dur,
        "lane": a.lane,
        "failed": a.flags & F_FAILED != 0,
        "cache_hit": a.flags & F_CACHE_HIT != 0,
        "category": f.src(e),
        "type": f.ty(e),
    })
}

/// [`ev_json`] plus the data the action moved: logical input and output sizes,
/// and the transfers RE measured. `null` is "not recorded".
fn data_moved_json(f: &Facts, e: u32) -> Value {
    let a = f.args(e);
    let mut v = ev_json(f, e);
    v["bytes_in"] = json!(a.bytes_in_recorded.then_some(a.bytes_in));
    v["bytes_out"] = json!(a.bytes_out_recorded.then_some(a.bytes_out));
    v["uploaded_bytes"] = json!(a.iu_bytes_uploaded);
    v["worker_bytes_in"] = json!(a.worker_bytes_in);
    v["worker_bytes_out"] = json!(a.worker_bytes_out);
    v
}

/// Build the requested topics. `want` empty or containing "all" =
/// every topic. The caller validated topic names against [`TOPICS`].
pub fn export(
    tr: &Trace,
    f: &Facts,
    g: Option<&Graph>,
    run: Option<&str>,
    want: &[String],
    topk: usize,
    ta: &TopicArgs,
) -> Value {
    let all = want.is_empty() || want.iter().any(|w| w == "all");
    let nsel = f.sel.len() as u64;
    let requested = |t: &str| all || want.iter().any(|w| w == t);
    let applicable = |t: &str| nsel != 0 || matches!(t, "overview" | "command-profile" | "health");
    let asked = |t: &str| requested(t) && applicable(t);

    // the selected run's record, or the sole run's
    let sel_run = f
        .run
        .as_deref()
        .and_then(|id| tr.runs.iter().find(|r| r.run == id))
        .or_else(|| (f.run.is_none() && tr.runs.len() == 1).then(|| &tr.runs[0]));
    let qq = quality::Quality::assess(tr, run, g);
    // the DAG is built once, and only when a topic asks for it
    let stitched: OnceCell<Option<(dag::DagBundle, dag::DagStats)>> = OnceCell::new();
    let stitch = || {
        stitched
            .get_or_init(|| g.and_then(|g| dag::build_full(tr, g, run)))
            .as_ref()
    };
    let mut out = Map::new();
    for &topic in TOPICS.iter().filter(|&&t| requested(t) && !applicable(t)) {
        out.insert(topic.replace('-', "_"), refused(vec![
            "Not applicable: this command recorded no completed actions. Use command-profile for recorded lifecycle, loading, analysis and resource evidence.".to_owned(),
        ]));
    }
    let failed = f
        .sel
        .iter()
        .filter(|&&e| f.act(e).flags & F_FAILED != 0)
        .count();
    let hits = f
        .sel
        .iter()
        .filter(|&&e| f.act(e).flags & F_CACHE_HIT != 0)
        .count();
    let start = f.sel.iter().map(|&e| f.act(e).ts).min();
    let end = f
        .sel
        .iter()
        .map(|&e| f.act(e).ts.saturating_add(f.dur(e)))
        .max();
    let span = start.zip(end).map_or(0, |(a, b)| b.saturating_sub(a));
    out.insert(
        "trace".into(),
        json!({
            "path": tr.path,
            "actions": nsel,
            "file_layout_lanes": tr.lanes.len(),
            "span_us": span,
            "t0_abs_us": tr.run_origin_us(run),
            "timestamp_clock": "client epoch microseconds; executor timestamps are separately labelled",
            "failed": failed,
            "cache_hits": hits,
            "run": f.run,
            "outcome": sel_run.map(|r| r.outcome.as_str()),
            "dep_capture": sel_run.and_then(|r| r.dep_capture.as_ref()).map(|c| c.to_json()),
            "graph": g.map(|g| json!({
                "path": g.path,
                "nodes": g.n_rows,
                "joins": g.is_join.iter().filter(|&&b| b).count(),
                "edges": g.n_edges,
            })),
        }),
    );

    if let Some(e) = &f.x.source_error {
        for &topic in TOPICS.iter().filter(|&&topic| requested(topic)) {
            out.insert(topic.replace('-', "_"), refused(vec![e.clone()]));
        }
        return Value::Object(out);
    }

    // second-pass coverage feeds several envelopes
    let matched = f
        .sel
        .iter()
        .filter(|&&e| {
            f.x.args
                .get(f.ev[e as usize] as usize)
                .is_some_and(|a| a.decoded)
        })
        .count() as u64;
    let unmatched = nsel - matched;
    let args_cov = ("actions-with-args", matched, nsel);
    let args_lim = || {
        if unmatched > 0 {
            vec![format!(
                "{} action(s) did not re-decode on the second pass",
                unmatched
            )]
        } else {
            Vec::new()
        }
    };
    let part = if unmatched > 0 {
        "partial"
    } else {
        "available"
    };

    if asked("command-profile") {
        let value = match sel_run.and_then(|r| r.command_profile.as_ref()) {
            None => refused(vec![
                "no validated command profile was recorded for the selected run; \
                 action timestamps cannot reconstruct Buck's command-wide profile"
                    .to_owned(),
            ]),
            Some(profile) => match serde_json::to_value(profile) {
                Err(error) => refused(vec![format!(
                    "command profile could not be serialized: {error}"
                )]),
                Ok(data) => {
                    let incomplete = profile.has_incomplete_evidence();
                    let mut limitations = profile.warnings.clone();
                    limitations.push(
                        "Recorded command intervals, Buck critical-path model weights, and \
                         daemon-wide counters have different scopes; do not add them as \
                         exclusive wall-time phases or attribute daemon counters to actions."
                            .to_owned(),
                    );
                    if incomplete {
                        limitations.push(
                            "The profile reports rejected, dropped, or otherwise qualified evidence; \
                             consult its counters and warnings before interpreting missing data."
                                .to_owned(),
                        );
                    }
                    qq.seal(
                        Dep::Trace,
                        Claim::Measurement,
                        if incomplete { "partial" } else { "available" },
                        None,
                        Vec::new(),
                        limitations,
                        data,
                    )
                }
            },
        };
        out.insert("command_profile".into(), value);
    }

    if asked("action-detail") {
        out.insert("action_detail".into(), action_detail(f, ta.action, &qq));
    }

    if asked("overview") {
        let o = analyses::overview_facts(f);
        let chain = stitch().map(|(b, st)| dag::chain_from_bundle(b, st));
        let overview_quality = stitch().map_or_else(|| qq.clone(), |(_, st)| qq.stitched(st));
        let rows = if nsel == 0 {
            command_rows(f)
        } else {
            analyses::overview_rows(f, &o, chain.as_ref(), &analyses::fmt_when_utc)
        };
        let lims = args_lim();
        out.insert(
            "overview".into(),
            overview_quality.seal(
                if g.is_some() { Dep::Graph } else { Dep::Trace },
                Claim::Measurement,
                part,
                None,
                if nsel == 0 {
                    Vec::new()
                } else {
                    vec![args_cov]
                },
                lims,
                Value::Array(
                    rows.into_iter()
                        .map(|r| json!({"key": r.key, "value": r.value, "link": r.link}))
                        .collect(),
                ),
            ),
        );
    }

    if asked("breakdown") {
        let rows = analyses::breakdown(f);
        out.insert(
            "breakdown".into(),
            qq.seal(
                Dep::Trace,
                Claim::Measurement,
                part,
                None,
                vec![args_cov],
                args_lim(),
                Value::Array(
                    rows.iter()
                        .map(|r| {
                            json!({
                                "type": r.ty, "actions": r.count, "total_us": r.total_us,
                                "share_pct": r.share_pct, "avg_us": r.avg_us,
                            })
                        })
                        .collect(),
                ),
            ),
        );
    }

    if asked("rule-sites") {
        let all = analyses::rule_sites(f, usize::MAX);
        let shown = all.len().min(analyses::RULE_SITES_N);
        let rows = &all[..shown];
        let with_cat = f.sel.iter().filter(|&&e| f.src(e).is_some()).count() as u64;
        let mut lims = args_lim();
        if with_cat < nsel {
            lims.push(format!("{} action(s) carry no category", nsel - with_cat));
        }
        // a machine-readable surface must not truncate invisibly
        if shown < all.len() {
            lims.push(format!("showing {shown} of {} categories", all.len()));
        }
        out.insert(
            "rule_sites".into(),
            qq.seal(
                Dep::Trace,
                Claim::Measurement,
                if with_cat < nsel { "partial" } else { part },
                None,
                vec![("actions-with-category", with_cat, nsel.max(1))],
                lims,
                Value::Array(
                    rows.iter()
                        .map(|r| {
                            json!({
                                "category": r.rule, "actions": r.count,
                                "total_us": r.total_us, "share_pct": r.share_pct,
                            })
                        })
                        .collect(),
                ),
            ),
        );
    }

    if asked("peers") {
        let ps = analyses::peer_stats(f);
        let rows = ps.table(f);
        out.insert(
            "peers".into(),
            qq.seal(
                Dep::Trace,
                Claim::Measurement,
                part,
                None,
                vec![args_cov],
                args_lim(),
                Value::Array(
                    rows.iter()
                        .map(|r| {
                            json!({
                                "category": r.cat, "actions": r.n, "p50_us": r.p50_us,
                                "p90_us": r.p90_us, "max_us": r.max_us, "total_us": r.total_us,
                            })
                        })
                        .collect(),
                ),
            ),
        );
    }

    if asked("stall") {
        let r = analyses::stall_audit(f);
        let mut lims = Vec::new();
        if r.with_stages < r.nacts {
            lims.push(format!(
                "{} action(s) carry no stage data; their time cannot be attributed",
                r.nacts - r.with_stages
            ));
        }
        out.insert(
            "stall".into(),
            qq.seal(
                Dep::Trace,
                Claim::Measurement,
                if r.with_stages < r.nacts {
                    "partial"
                } else {
                    "available"
                },
                None,
                vec![(
                    "actions-with-stages",
                    r.with_stages as u64,
                    r.nacts.max(1) as u64,
                )],
                lims,
                json!({
                    "total_dur_us": r.total_dur,
                    "staged_us": r.staged,
                    "cohorts": r.cohorts.iter().map(|c| json!({
                        "n": c.n,
                        "typical_dur_us": c.dur_typ,
                        "t0_us": tr.t0_abs.saturating_add(c.t0),
                        "t1_us": tr.t0_abs.saturating_add(c.t1),
                        "staged_frac": c.staged_frac,
                        "lost_us": c.lost_us,
                        "sample": f.name(c.sample),
                    })).collect::<Vec<_>>(),
                }),
            ),
        );
    }

    if asked("critical-path") {
        match g {
            None => out.insert(
                "critical_path".into(),
                match sel_run
                    .and_then(|r| r.command_profile.as_ref())
                    .and_then(|p| p.critical_path.as_ref())
                    .filter(|path| !path.entries.is_empty())
                {
                    Some(path) => reported_critical_path(tr, path),
                    None => refused(vec![
                        "needs the dependency graph (a sibling *-graph.json, or --graph=); \
                         an auto-discovered sidecar that failed to load was ignored with a \
                         warning on stderr"
                            .into(),
                    ]),
                },
            ),
            Some(_) => match stitch() {
                Some((b, st)) => {
                    let c = dag::chain_from_bundle(b, st);
                    out.insert(
                        "critical_path".into(),
                        qq.stitched(st).seal(
                            Dep::Graph,
                            Claim::LongestPath,
                            "available",
                            None,
                            Vec::new(),
                            Vec::new(),
                            json!({
                                "summed_node_duration_us": b.dag.t1,
                                "cp_us": c.cp_us,
                                "steps": c.entries.len(),
                                // build-level execution-admission band:
                                // client-side queueing, parallel-lane
                                // time — do not read as wall
                                "exec_slot": analyses::exec_slot_overview(f).map(|o| json!({
                                    "actions": o.n,
                                    "submit_us": o.submit_us,
                                    "exec_slot_wait_us": o.sem_us,
                                    "actions_over_1s": o.over_1s,
                                    "actions_over_10s": o.over_10s,
                                    "max_us": o.max_us,
                                })),
                                "path": c.entries.iter().map(|en| {
                                    let e = &b.events[en.ev];
                                    json!({
                                        "target": e.name,
                                        "start_us": e.ts,
                                        "duration_us": en.us,
                                        "cache_hit": e.has_cache,
                                    })
                                }).collect::<Vec<_>>(),
                            }),
                        ),
                    )
                }
                _ => out.insert(
                    "critical_path".into(),
                    refused(vec!["nothing to stitch for the selected run".into()]),
                ),
            },
        };
    }

    if asked("longest") {
        let mut idx: Vec<u32> = f.sel.clone();
        idx.sort_by_key(|&e| std::cmp::Reverse(f.dur(e)));
        idx.truncate(topk);
        out.insert(
            "longest".into(),
            qq.seal(
                Dep::Trace,
                Claim::Measurement,
                part,
                None,
                vec![args_cov],
                args_lim(),
                Value::Array(idx.iter().map(|&e| data_moved_json(f, e)).collect()),
            ),
        );
    }

    if asked("top-memory") {
        let rows = analyses::top_memory(f, analyses::TOP_MEMORY_N);
        if rows.is_empty() {
            out.insert(
                "top_memory".into(),
                refused({
                    let mut lims = args_lim();
                    lims.push("No decoded action in the selected run carries a positive peak-RSS measurement; absent data is not zero memory use.".to_owned());
                    lims
                }),
            );
        } else {
            let with = f
                .sel
                .iter()
                .filter(|&&e| f.args(e).maxrss.unwrap_or(0) != 0)
                .count();
            out.insert(
                "top_memory".into(),
                qq.seal(
                    Dep::Trace,
                    Claim::Measurement,
                    if with < nsel as usize { "partial" } else { part },
                    None,
                    vec![("actions-with-maxrss", with as u64, nsel.max(1))],
                    {
                        let mut lims = args_lim();
                        if with < nsel as usize {
                            lims.push(format!("{} action(s) carry no positive peak-RSS measurement; ranking covers measured actions only", nsel - with as u64));
                        }
                        lims
                    },
                    Value::Array(
                        rows.iter()
                            .map(|r| {
                                json!({
                                    "target": r.target, "maxrss_bytes": r.maxrss,
                                    "type": r.ty, "duration_us": r.dur_us,
                                    "exec_us": r.exec_us, "category": r.rule,
                                })
                            })
                            .collect(),
                    ),
                ),
            );
        }
    }

    if asked("re-queue") {
        match analyses::queue_overview(f) {
            None => out.insert(
                "re_queue".into(),
                refused(vec![
                    "no action carries stages.queue_us (not an RE build, or a producer \
                     without stage data)"
                        .into(),
                ]),
            ),
            Some(q) => out.insert(
                "re_queue".into(),
                qq.seal(
                    Dep::Trace,
                    Claim::Measurement,
                    if q.covered < q.total {
                        "partial"
                    } else {
                        "available"
                    },
                    None,
                    vec![(
                        "actions-with-queue-stage",
                        q.covered as u64,
                        q.total.max(1) as u64,
                    )],
                    if q.covered < q.total {
                        vec![format!(
                            "{} action(s) recorded no queue stage (absent is not zero)",
                            q.total - q.covered
                        )]
                    } else {
                        Vec::new()
                    },
                    json!({
                        "total_us": q.total_us,
                        "measured_actions": q.covered,
                        "actions_with_positive_wait": q.waited,
                        "mean_us": q.mean_us,
                        "p90_us": q.p90_us,
                        "max_us": q.max_us,
                        "max_target": f.name(q.max_ev),
                    }),
                ),
            ),
        };
    }

    if asked("admission") {
        let value = match analyses::exec_slot_overview(f) {
            None => refused(vec!["No selected action carries execution-admission measurements; absence is not zero wait and does not establish the producer version.".into()]),
            Some(o) => qq.seal(
                Dep::Trace,
                Claim::Measurement,
                if o.n < nsel as usize { "partial" } else { part },
                None,
                vec![("actions-with-admission-measurements", o.n as u64, nsel)],
                vec!["Client-side execution admission, not the RE server queue. Sums overlap across actions and are not build wall time; measured zero waits remain included.".into()],
                json!({
                    "actions": o.n,
                    "submit_us": o.submit_us,
                    "exec_slot_wait_us": o.sem_us,
                    "actions_over_1s": o.over_1s,
                    "actions_over_10s": o.over_10s,
                    "max_us": o.max_us,
                }),
            ),
        };
        out.insert("admission".into(), value);
    }

    if asked("retries") {
        let v = match analyses::retry_census(f, topk) {
            None => refused(vec!["no action in this run ran more than once".into()]),
            Some(c) => {
                let classified = (c.concurrent + c.rerun) as u64;
                let count_only = c
                    .extra_attempts
                    .saturating_sub(c.concurrent + c.rerun + c.unknown);
                let mut lims = vec![
                    "attempt statuses come from the log's per-attempt command \
                     records; captures older than that carry counts only"
                        .to_owned(),
                    "`lost_us` is the discarded attempts' command elapsed time as buck2 \
                     measured it (client queueing and download included), not worker \
                     occupancy"
                        .to_owned(),
                    "fates describe timing, not causality: `concurrent` overlaps the winner; \
                     `rerun` ended before it started; `unknown` lacks a comparable interval. \
                     `delay_us` sums classified pre-winner command elapsed budgets, not a \
                     bound on total build delay: concurrent, unknown and count-only attempts \
                     may also affect it. `client-interval` requires both client starts and \
                     their own valid command durations inside the action; `duration-overflow` \
                     only proves overlap when two valid durations cannot fit sequentially"
                        .to_owned(),
                ];
                if c.unknown > 0 {
                    lims.push(format!(
                        "{} discarded attempt(s) have no comparable interval; their \
                         fate is unknown",
                        c.unknown
                    ));
                }
                if count_only > 0 {
                    lims.push(format!(
                        "{count_only} discarded attempt(s) carry only a retry count with \
                         no attempt record"
                    ));
                }
                qq.seal(Dep::Trace, Claim::Measurement,
                    if classified < c.extra_attempts as u64 {
                        "partial"
                    } else {
                        "available"
                    },
                    None,
                    vec![
                        ("actions-that-retried", c.actions as u64, nsel.max(1)),
                        (
                            "discarded-attempts-with-known-fate",
                            classified,
                            c.extra_attempts.max(1) as u64,
                        ),
                        (
                            "discarded-attempts-with-exact-interval",
                            c.exact_fates as u64,
                            c.extra_attempts.max(1) as u64,
                        ),
                    ],
                    lims,
                    json!({
                        "actions": c.actions,
                        "extra_attempts": c.extra_attempts,
                        "retried_duration_us": c.retried_dur_us,
                        "concurrent": c.concurrent,
                        "rerun": c.rerun,
                        "unknown": c.unknown,
                        "exact_fates": c.exact_fates,
                        "delay_us": c.delay_us,
                        "by_status": c.by_status.iter().map(|(k, n, ex)| json!({
                            "status": k, "actions": n, "extra_attempts": ex,
                        })).collect::<Vec<_>>(),
                        "by_category": c.by_category.iter().map(|(k, n, ex)| json!({
                            "category": k, "actions": n, "extra_attempts": ex,
                        })).collect::<Vec<_>>(),
                        "categories_shown": c.by_category.len(),
                        "categories_truncated": c.by_category.len() < c.n_categories,
                        "lost_us": c.lost_us,
                        "rows_shown": c.rows.len().min(topk),
                        "rows_truncated": c.rows.len() > topk,
                        "rows": c.rows.iter().take(topk).map(|r| json!({
                            "target": f.name(r.ev),
                            "attempts": r.tries,
                            "lost_us": r.lost_us,
                            "delay_us": r.delay_us,
                            "duration_us": f.act(r.ev).dur,
                            "discarded": r.attempts.iter().zip(&r.fates).map(|(at, (ft, basis))| json!({
                                "status": at.status,
                                "duration_us": at.dur_recorded.then_some(at.dur_us),
                                "executor_start_us": at.start_recorded.then_some(at.start_us),
                                "executor_start_clock": "executor epoch microseconds; may differ from the client clock",
                                "client_start_us": at.client_start_us,
                                "elastic_capacity": at.elastic.then_some(true),
                                "fate": ft.as_str(),
                                "fate_basis": basis.as_str(),
                            })).collect::<Vec<_>>(),
                        })).collect::<Vec<_>>(),
                    }),
                )
            }
        };
        out.insert("retries".into(), v);
    }

    if asked("health") {
        // the trace's own warnings already carry the capture strings;
        // this topic adds only the graph, stitch and pairing ones, so no
        // string appears twice
        let hq = if g.is_some() {
            stitch().map(|(_, st)| qq.stitched(st))
        } else {
            None
        };
        let hq = hq.as_ref().unwrap_or(&qq);
        out.insert(
            "health".into(),
            envelope(
                if hq.capture_complete() && hq.graph_complete() {
                    "available"
                } else {
                    "partial"
                },
                None,
                hq.coverage(Dep::Graph),
                hq.graph_limitations(),
                json!({
                    "warnings": tr.health.warnings,
                    "parse_ms": tr.parse_ms,
                    "index_ms": tr.index_ms,
                    "extras_unmatched": unmatched,
                    "file_extras_unmatched": f.x.unmatched,
                    "outcome": sel_run.map(|r| r.outcome.as_str()),
                    "dep_capture": sel_run.and_then(|r| r.dep_capture.as_ref()).map(|c| {
                        let mut v = c.to_json();
                        v["complete"] = json!(c.is_complete());
                        v
                    }),
                    "graph": g.map(|g| {
                        let cov = g.meta.coverage.clone().unwrap_or_default();
                        json!({
                            "declared": {
                                "nodes": cov.nodes, "edges": cov.edges,
                                "action_rows": cov.action_rows, "join_rows": cov.join_rows,
                                "dangling_deps": cov.dangling_deps,
                                "dangling_tsets": cov.dangling_tsets,
                            },
                            "loaded": {
                                "rows": g.n_rows, "edges": g.n_edges,
                                "dropped_lines": g.dropped_lines,
                                "dropped_interior": g.dropped_interior,
                                "bad_rows": g.bad_rows,
                                "undefined_joins": g.undefined_joins,
                                "undefined_deps": g.undefined_deps,
                                "targets_in_cycles": g.targets_in_cycles,
                            },
                            "dep_capture": cov.dep_capture.as_ref().map(|c| c.to_json()),
                        })
                    }),
                }),
            ),
        );
    }

    Value::Object(out)
}

fn command_rows(f: &Facts) -> Vec<analyses::OverviewRow> {
    let mut rows = Vec::new();
    let mut add = |key, value| {
        rows.push(analyses::OverviewRow {
            key,
            value,
            link: None,
        })
    };
    if let Some(run) = f
        .run
        .as_deref()
        .and_then(|id| f.tr.runs.iter().find(|r| r.run == id))
    {
        add("Command ID", run.run.clone());
        if let Some(argv) = &run.argv {
            add("Invocation", argv.clone());
        }
        add("Outcome", run.outcome.as_str().to_owned());
        if let Some(profile) = &run.command_profile {
            if let Some(kind) = &profile.command_kind {
                add("Command", kind.clone());
            }
            if let Some(error) = &profile.result_error {
                add("Diagnostic", error.clone());
            }
            match (
                profile.command_start_offset_us,
                profile.command_end_offset_us,
            ) {
                (Some(start), Some(end)) => match end.checked_sub(start).filter(|&d| d >= 0) {
                    Some(span) => add("Command span", analyses::fmt_dur(span)),
                    None => add(
                        "Command span",
                        "inconsistent: the recorded end precedes the start".into(),
                    ),
                },
                _ => add("Command span", "not fully recorded".into()),
            }
            for phase in &profile.phases {
                add("Recorded phase", phase_line(phase));
            }
        }
    }
    add("Action analyses", "not applicable: no completed actions recorded; use --json=command-profile for retained command evidence".into());
    rows
}

/// One recorded command phase for `--stats`. Spans of one kind overlap and stay open while
/// waiting on other work, so their sum is neither work nor wall time; the union of the spans
/// comes first when it is exact.
fn phase_line(phase: &crate::command_profile::PhaseSummary) -> String {
    let active = phase
        .active_us()
        .map(|us| format!("{} with observed activity; ", analyses::fmt_dur(us)))
        .unwrap_or_default();
    format!(
        "{}: {} completed spans; {active}{} total span time (spans overlap and include waiting)",
        phase.kind,
        phase.completed,
        analyses::fmt_dur(phase.total_duration_us)
    )
}

/// The `--stats` text: the overview rows plus the top of each table —
/// the same analyses as `--json`, rendered for a terminal.
pub fn stats_text(tr: &Trace, f: &Facts, g: Option<&Graph>, run: Option<&str>) -> String {
    if let Some(e) = &f.x.source_error {
        return format!("refused: {e}\n");
    }
    if f.sel.is_empty() {
        let mut text: String = command_rows(f)
            .into_iter()
            .map(|r| format!("{:18} {}\n", r.key, r.value))
            .collect();
        append_health_text(&mut text, tr, run, g, None);
        return text;
    }
    let mut s = String::new();
    let o = analyses::overview_facts(f);
    let built = g.and_then(|g| dag::build_full(tr, g, run));
    let chain = built.as_ref().map(|(b, st)| dag::chain_from_bundle(b, st));
    for r in analyses::overview_rows(f, &o, chain.as_ref(), &analyses::fmt_when_utc) {
        s.push_str(&format!("{:18} {}\n", r.key, r.value));
    }
    s.push('\n');
    let b = analyses::breakdown(f);
    if !b.is_empty() {
        s.push_str("work by type:\n");
        for r in b.iter().take(7) {
            s.push_str(&format!(
                "  {:10} {:>8} actions  {:>12}  {:>3}%\n",
                r.ty,
                analyses::fmt_n(r.count as f64),
                analyses::fmt_dur(r.total_us),
                r.share_pct
            ));
        }
    }
    let rs = analyses::rule_sites(f, 5);
    if !rs.is_empty() {
        s.push_str("top categories:\n");
        for r in &rs {
            s.push_str(&format!(
                "  {:28} {:>8} actions  {:>12}  {:>3}%\n",
                r.rule,
                analyses::fmt_n(r.count as f64),
                analyses::fmt_dur(r.total_us),
                r.share_pct
            ));
        }
    }
    if let Some(q) = analyses::queue_overview(f) {
        s.push_str(&format!(
            "RE queue: {} of {} actions have measurements (zeros included); mean {}, p90 {}, max {} ({})\n",
            analyses::fmt_n(q.covered as f64),
            analyses::fmt_n(q.total as f64),
            analyses::fmt_dur(q.mean_us),
            analyses::fmt_dur(q.p90_us),
            analyses::fmt_dur(q.max_us),
            f.name(q.max_ev)
        ));
    }
    if let Some(o) = analyses::exec_slot_overview(f) {
        s.push_str(&format!(
            "execution admission: {} summed submit time, {} recorded permit wait across {} measured actions; {} waited >1s, {} >10s, max {} (parallel action time, not build wall)\n",
            analyses::fmt_dur(o.submit_us),
            analyses::fmt_dur(o.sem_us),
            analyses::fmt_n(o.n as f64),
            analyses::fmt_n(o.over_1s as f64),
            analyses::fmt_n(o.over_10s as f64),
            analyses::fmt_dur(o.max_us),
        ));
    }
    if let Some(c) = analyses::retry_census(f, 3) {
        let count_only = c
            .extra_attempts
            .saturating_sub(c.concurrent + c.rerun + c.unknown);
        let count_note = if count_only > 0 {
            format!(
                ", {} with only a retry count",
                analyses::fmt_n(count_only as f64)
            )
        } else {
            String::new()
        };
        s.push_str(&format!(
            "retries: {} actions re-ran ({} extra attempts: {} ran alongside the \
             winner, {} died before the re-run, {} with no comparable interval{}), {} of \
             command time on discarded attempts, classified pre-winner elapsed budget {} \
             (not a build-delay bound); worst {} \
             attempts ({})\n",
            analyses::fmt_n(c.actions as f64),
            analyses::fmt_n(c.extra_attempts as f64),
            analyses::fmt_n(c.concurrent as f64),
            analyses::fmt_n(c.rerun as f64),
            analyses::fmt_n(c.unknown as f64),
            count_note,
            analyses::fmt_dur(c.lost_us),
            analyses::fmt_dur(c.delay_us),
            c.rows.first().map(|r| r.tries).unwrap_or(0),
            c.rows.first().map(|r| f.name(r.ev)).unwrap_or("")
        ));
    }
    let sa = analyses::stall_audit(f);
    s.push_str(&format!(
        "stage coverage: {} of {} actions carry stage data ({:.0}% of work attributed)\n",
        analyses::fmt_n(sa.with_stages as f64),
        analyses::fmt_n(sa.nacts as f64),
        sa.staged as f64 / sa.total_dur.max(1) as f64 * 100.0
    ));
    for c in sa.cohorts.iter().take(3) {
        s.push_str(&format!(
            "  DURATION COHORT: {} actions of ~{} with {:.0}% stage coverage — {} unclassified (sum of actions), {} first-to-last window (e.g. {})\n",
            c.n,
            analyses::fmt_dur(c.dur_typ),
            c.staged_frac * 100.0,
            analyses::fmt_dur(c.lost_us),
            analyses::fmt_dur(c.t1.saturating_sub(c.t0).max(0)),
            f.name(c.sample)
        ));
    }
    if !sa.cohorts.is_empty() {
        s.push_str(
            "  (Grouping by similar durations does not establish overlap or a shared cause. Unclassified time is not measured idle, CPU, or wait time.)\n",
        );
    }
    append_health_text(&mut s, tr, run, g, built.as_ref().map(|(_, st)| st));
    s
}

fn append_health_text(
    text: &mut String,
    tr: &Trace,
    run: Option<&str>,
    g: Option<&Graph>,
    st: Option<&dag::DagStats>,
) {
    let q = quality::Quality::assess(tr, run, g);
    let q = st.map_or_else(|| q.clone(), |st| q.stitched(st));
    let mut warnings = tr.health.warnings.clone();
    warnings.extend(q.capture_limitations());
    warnings.extend(q.graph_limitations());
    let mut seen = std::collections::HashSet::new();
    for warning in warnings {
        if seen.insert(warning.clone()) {
            text.push_str(&format!("warning: {warning}\n"));
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testdata as fixture;

    /// The headless pipeline over a fixture pair, the way main.rs runs it.
    fn run_export(trace: &str, graph: Option<&str>, want: &[&str]) -> Value {
        let tr = crate::parse::parse(trace).unwrap();
        let x = analyses::load_extras(&tr);
        let run = tr.runs.first().map(|r| r.run.clone());
        let f = Facts::for_run(&tr, &x, run.as_deref()).unwrap();
        let g = graph.map(|p| crate::graph::load_graph(p).unwrap());
        let want: Vec<String> = want.iter().map(|w| (*w).to_owned()).collect();
        export(
            &tr,
            &f,
            g.as_ref(),
            run.as_deref(),
            &want,
            10,
            &TopicArgs::default(),
        )
    }

    fn has_lim(v: &Value, needle: &str) -> bool {
        v["limitations"]
            .as_array()
            .is_some_and(|a| a.iter().any(|l| l.as_str().unwrap().contains(needle)))
    }

    fn cov(v: &Value, scope: &str) -> Option<(u64, u64)> {
        v["coverage"]
            .as_array()?
            .iter()
            .find(|c| c["scope"] == scope)
            .map(|c| {
                (
                    c["numerator"].as_u64().unwrap(),
                    c["denominator"].as_u64().unwrap(),
                )
            })
    }

    fn trace_rows(rows: &[Value]) -> (Trace, analyses::Extras) {
        let path = crate::temp_path("export-review-trace.json");
        let body = rows
            .iter()
            .map(Value::to_string)
            .collect::<Vec<_>>()
            .join("\n");
        std::fs::write(&path, format!("[\n{body}\n]\n")).unwrap();
        let tr = crate::parse::parse(path.to_str().unwrap()).unwrap();
        let x = analyses::load_extras(&tr);
        std::fs::remove_file(path).unwrap();
        (tr, x)
    }

    #[test]
    fn a_phase_sum_is_not_reported_as_work() {
        let phase = crate::command_profile::PhaseSummary {
            kind: "Materialization".into(),
            completed: 3,
            total_duration_us: 9_000_000,
            union_intervals: vec![[0, 2_000_000]],
            union_complete: true,
            ..Default::default()
        };
        let line = phase_line(&phase);
        assert!(
            line.contains("2.00s with observed activity") && line.contains("9.00s total span time"),
            "the union leads and the overlapping sum is named for what it is: {line}"
        );
        assert!(!line.contains("work"), "{line}");
    }

    #[test]
    fn ambiguous_detail_names_disclose_candidates_not_shown() {
        for count in [3, 10] {
            let rows: Vec<_> = (0..count)
                .map(|i| {
                    json!({
                        "ph":"X", "cat":"recipe", "name":"duplicate", "ts":i, "dur":1,
                    })
                })
                .collect();
            let (tr, x) = trace_rows(&rows);
            let f = Facts::for_run(&tr, &x, None).unwrap();
            let error = detail_action(&f, "duplicate").unwrap_err();
            assert!(error.contains(&format!("({count} actions)")), "{error}");
            assert_eq!(error.contains("(+2 more)"), count == 10, "{error}");
        }
    }

    #[test]
    fn retries_report_display_limits_and_count_only_attempts() {
        let rows: Vec<_> = ["b", "a"]
            .into_iter()
            .map(|name| {
                json!({
                    "ph":"X", "cat":"recipe", "name":name, "ts":0, "dur":10,
                    "args":{"src":name, "tries":2},
                })
            })
            .collect();
        let (tr, x) = trace_rows(&rows);
        let f = Facts::for_run(&tr, &x, None).unwrap();
        for topk in [1, 20] {
            let value = export(
                &tr,
                &f,
                None,
                None,
                &["retries".into()],
                topk,
                &TopicArgs::default(),
            );
            let d = &value["retries"]["data"];
            assert_eq!(d["rows_shown"], topk.min(2));
            assert_eq!(d["categories_shown"], topk.min(2));
            assert_eq!(d["rows_truncated"], topk == 1);
            assert_eq!(d["categories_truncated"], topk == 1);
        }
        assert!(stats_text(&tr, &f, None, None).contains("2 with only a retry count"));
    }

    const GRAPH_TOPICS: &[&str] = &["critical-path"];
    const TRACE_TOPICS: &[&str] = &["overview", "breakdown", "peers", "longest"];

    #[test]
    fn action_detail_reproduces_positioned_evidence_without_extras_or_graph() {
        let t = fixture("activity-trace.json");
        let tr = crate::parse::parse(&t).unwrap();
        let x = analyses::Extras::default();
        let f = Facts::for_run(&tr, &x, None).unwrap();
        let get = |action| {
            export(
                &tr,
                &f,
                None,
                None,
                &["action-detail".into()],
                20,
                &TopicArgs { action },
            )
        };
        let value = get(Some("fallback (rustc)"));
        let topic = &value["action_detail"];
        assert_eq!(topic["status"], "available", "{topic}");
        let data = &topic["data"];
        assert_eq!(data["span_id"], 10);
        assert_eq!(data["start_us"], 1_000_000);
        assert_eq!(data["activity"]["covered_us"], 599_999_000);
        assert_eq!(data["activity"]["uncovered_us"], 1_000);
        assert_eq!(data["activity"]["complete_observation_list"], true);
        assert!(
            data["activity"]["spans"]
                .as_array()
                .unwrap()
                .iter()
                .any(|s| s["name"] == "Re.WorkerDownload" && s["cancelled"] == true)
        );
        assert!(
            data["rows"]
                .as_array()
                .unwrap()
                .iter()
                .any(|r| r["key"] == crate::detail::RE_OBSERVATIONS_KEY)
        );
        assert!(
            data["stage_lines"]
                .as_array()
                .unwrap()
                .iter()
                .any(|r| r["stage"] == "materialize_us"
                    && r["provenance"].as_str().unwrap().contains("this machine"))
        );
        let index = format!("action:{}", data["action_index"].as_u64().unwrap());
        assert_eq!(get(Some(&index))["action_detail"]["data"], *data);
        assert_eq!(
            get(Some("fallback"))["action_detail"]["status"],
            "refused",
            "no substring guessing"
        );
        assert_eq!(
            get(Some("action:999999"))["action_detail"]["status"],
            "refused"
        );
        assert_eq!(get(None)["action_detail"]["status"], "refused");
    }

    #[test]
    fn action_detail_qualifies_legacy_data_and_refuses_ambiguity_large_or_changed_records() {
        let t = fixture("smoke-trace.json");
        let path = crate::temp_path("action-detail-generation.json");
        std::fs::write(&path, std::fs::read(&t).unwrap()).unwrap();
        let mut tr = crate::parse::parse(path.to_str().unwrap()).unwrap();
        let x = analyses::Extras::default();
        let i = tr
            .acts
            .iter()
            .position(|a| tr.names[a.name as usize] == "y (rustc emit)")
            .unwrap();
        let query = format!("action:{i}");
        let get = |tr: &Trace, action| {
            let f = Facts::for_run(tr, &x, None).unwrap();
            export(
                tr,
                &f,
                None,
                None,
                &["action-detail".into()],
                20,
                &TopicArgs { action },
            )
        };
        let legacy = get(&tr, Some(query.as_str()));
        assert_eq!(legacy["action_detail"]["status"], "partial");
        assert!(legacy["action_detail"]["data"]["activity"]["uncovered_us"].is_null());
        assert!(legacy["action_detail"]["data"]["stage_lines"].is_array());
        let another = tr
            .acts
            .iter()
            .position(|a| tr.names[a.name as usize] == "z (cxx_link)")
            .unwrap();
        let saved_name = tr.acts[another].name;
        tr.acts[another].name = tr.acts[i].name;
        assert!(has_lim(
            &get(&tr, Some("y (rustc emit)"))["action_detail"],
            "ambiguous"
        ));
        tr.acts[another].name = saved_name;
        let saved_len = tr.acts[i].len;
        tr.acts[i].len = MAX_DETAIL_RECORD_BYTES as u32 + 1;
        assert!(has_lim(
            &get(&tr, Some(&query))["action_detail"],
            "detail limit"
        ));
        tr.acts[i].len = saved_len;
        std::fs::write(&path, "changed generation").unwrap();
        assert_eq!(get(&tr, Some(&query))["action_detail"]["status"], "refused");
        std::fs::remove_file(path).unwrap();
    }

    #[test]
    fn graph_exports_keep_action_identity_when_file_time_and_lane_order_differ() {
        let (t, g) = (fixture("chain-trace.json"), fixture("chain-graph.jsonl"));
        let tr = crate::parse::parse(&t).unwrap();
        let mut file_order: Vec<_> = tr.acts.iter().collect();
        file_order.sort_by_key(|a| a.seq);
        assert!(file_order.windows(2).any(|w| w[0].ts > w[1].ts));
        let value = run_export(&t, Some(&g), &["critical-path", "longest"]);
        let path = &value["critical_path"]["data"];
        assert_eq!(path["cp_us"], 5_850);
        let short = |v: &Value| {
            v["target"]
                .as_str()
                .unwrap()
                .split(" (")
                .next()
                .unwrap()
                .to_owned()
        };
        assert_eq!(
            path["path"]
                .as_array()
                .unwrap()
                .iter()
                .map(short)
                .collect::<Vec<_>>(),
            [
                "fbcode//buck2/app/buck2_core:buck2_core",
                "fbcode//buck2/app/buck2_build_api:buck2_build_api",
                "fbcode//buck2/app/buck2_server_commands:buck2_server_commands",
                "fbcode//buck2/app/buck2_execute_impl:buck2_execute_impl",
                "fbcode//buck2/app/buck2_action_impl:buck2_action_impl",
                "fbcode//buck2/app/buck2_client:buck2_client",
                "fbcode//buck2/app/buck2_daemon:buck2_daemon",
                "fbcode//buck2/cli:buck2",
                "fbcode//buck2/cli:buck2",
            ]
        );
    }

    #[test]
    fn all_topics_have_envelopes_and_action_timestamps_share_a_clock() {
        let (t, g) = (fixture("smoke-trace.json"), fixture("smoke-graph.jsonl"));
        let value = run_export(&t, Some(&g), &["all"]);
        for topic in TOPICS {
            let emitted = &value[topic.replace('-', "_")];
            assert!(emitted["status"].is_string(), "missing envelope: {topic}");
        }
        let longest = value["longest"]["data"].as_array().unwrap();
        let emit = longest
            .iter()
            .find(|a| a["target"] == "y (rustc emit)")
            .unwrap();
        assert_eq!(
            (&emit["bytes_in"], &emit["bytes_out"]),
            (&json!(5_000_000), &json!(1000))
        );
        assert!(
            emit["worker_bytes_in"].is_null(),
            "a local action has no RE worker transfer: {emit}"
        );
        let link = longest
            .iter()
            .find(|a| a["target"] == "z (cxx_link)")
            .unwrap();
        assert!(
            link["bytes_in"].is_null() && link["bytes_out"].is_null(),
            "sizes the log never recorded are null, not 0: {link}"
        );
        for row in value["critical_path"]["data"]["path"].as_array().unwrap() {
            let same = longest
                .iter()
                .find(|a| a["target"] == row["target"])
                .unwrap();
            assert_eq!(same["start_us"], row["start_us"]);
        }
        assert!(value["trace"]["t0_abs_us"].is_i64());
        assert_eq!(cov(&value["longest"], "actions-with-args"), Some((3, 3)));
        assert_eq!(value["top_memory"]["status"], "partial");
        assert!(has_lim(
            &value["top_memory"],
            "ranking covers measured actions only"
        ));
        assert!(value["critical_path"]["data"].get("t1_us").is_none());
        assert!(value["critical_path"]["data"]["summed_node_duration_us"].is_i64());
    }

    #[test]
    fn zero_admission_and_upload_phases_are_measured_not_missing() {
        let (t, g) = (fixture("smoke-trace.json"), fixture("smoke-graph.jsonl"));
        let tr = crate::parse::parse(&t).unwrap();
        let mut x = analyses::load_extras(&tr);
        let i = tr
            .acts
            .iter()
            .position(|a| tr.names[a.name as usize] == "y (rustc emit)")
            .unwrap();
        x.args[i].admission_recorded = true;
        x.args[i].iu = Some(analyses::UploadSub::default());
        x.args[i].stages.as_mut().unwrap().input_upload_us = 10;
        let f = Facts::for_run(&tr, &x, None).unwrap();
        let g = crate::graph::load_graph(&g).unwrap();
        let value = export(
            &tr,
            &f,
            Some(&g),
            None,
            &["admission".into()],
            10,
            &TopicArgs::default(),
        );
        assert_eq!(
            cov(&value["admission"], "actions-with-admission-measurements"),
            Some((1, 3))
        );
        assert_eq!(value["admission"]["data"]["exec_slot_wait_us"], 0);
    }

    #[test]
    fn selected_scope_and_args_coverage_exclude_other_runs_and_bookkeeping() {
        let t = fixture("smoke-trace.json");
        let mut tr = crate::parse::parse(&t).unwrap();
        let mut x = analyses::load_extras(&tr);
        tr.run_ids.push("other".to_owned());
        let other = (tr.run_ids.len() - 1) as u32;
        let mut retained = None;
        for (i, act) in tr.acts.iter_mut().enumerate() {
            if tr.names[act.name as usize] == "y (rustc emit)" {
                retained = Some(i);
            } else {
                act.run = other;
            }
        }
        let i = retained.unwrap();
        x.args[i].decoded = false;
        let f = Facts::for_run(&tr, &x, Some("aaaa-bbbb")).unwrap();
        let value = export(
            &tr,
            &f,
            None,
            Some("aaaa-bbbb"),
            &["longest".into()],
            10,
            &TopicArgs::default(),
        );
        assert_eq!(value["trace"]["actions"], 1);
        assert_eq!(value["trace"]["failed"], 0);
        assert_eq!(value["trace"]["cache_hits"], 0);
        assert_eq!(value["trace"]["span_us"], 500);
        assert_eq!(cov(&value["longest"], "actions-with-args"), Some((0, 1)));
        assert_eq!(value["longest"]["status"], "partial");
        assert!(has_lim(&value["longest"], "did not re-decode"));
    }

    #[test]
    fn command_only_stats_retains_capture_warnings_and_profile_needs_no_extras() {
        let t = fixture("smoke-trace.json");
        let mut tr = crate::parse::parse(&t).unwrap();
        tr.health
            .warnings
            .push("test: command capture ended early".into());
        tr.runs[0].command_profile = Some(crate::command_profile::CommandProfile::default());
        let x = analyses::Extras::default();
        let mut f = Facts::for_run(&tr, &x, None).unwrap();
        let value = export(
            &tr,
            &f,
            None,
            None,
            &["command-profile".into()],
            10,
            &TopicArgs::default(),
        );
        assert!(value["command_profile"]["data"].is_object());
        f.sel.clear();
        let stats = stats_text(&tr, &f, None, None);
        assert!(stats.contains("no completed actions"));
        assert!(stats.contains("command capture ended early"));
    }

    #[test]
    fn command_profile_export_is_explicitly_unavailable_for_legacy_captures() {
        let t = fixture("smoke-trace.json");
        let value = run_export(&t, None, &["command-profile"]);
        let topic = &value["command_profile"];
        assert_eq!(topic["status"], "refused");
        assert!(topic["data"].is_null());
        assert!(has_lim(topic, "no validated command profile"));
        for limitation in topic["limitations"].as_array().unwrap() {
            let text = limitation.as_str().unwrap();
            assert!(!text.contains('\\'), "{text:?}");
            assert!(!text.contains('\n'), "{text:?}");
        }
        assert!(!TOPICS.contains(&"serial-chain"));
        assert!(!TOPICS.contains(&"levers"));
    }

    #[test]
    fn command_profile_export_preserves_the_selected_typed_evidence_and_zeros() {
        let t = fixture("smoke-trace.json");
        let mut tr = crate::parse::parse(&t).unwrap();
        let x = analyses::load_extras(&tr);
        let selected = tr.runs[0].run.clone();
        let profile = crate::command_profile::CommandProfile {
            run: Some(selected.clone()),
            origin_us: tr.t0_abs,
            command_kind: Some("install".to_owned()),
            command_start_offset_us: Some(0),
            command_end_offset_us: Some(0),
            client_wall_us: Some(0),
            lifecycle: vec![crate::command_profile::ObservedInterval {
                kind: "command".to_owned(),
                label: None,
                start_offset_us: 0,
                duration_us: 0,
                cancelled: false,
            }],
            ..Default::default()
        };
        tr.runs[0].command_profile = Some(profile.clone());
        tr.runs.push(crate::types::RunInfo {
            run: "not-selected".to_owned(),
            command_profile: Some(crate::command_profile::CommandProfile {
                run: Some("not-selected".to_owned()),
                client_wall_us: Some(987654),
                ..Default::default()
            }),
            ..Default::default()
        });
        let f = Facts::for_run(&tr, &x, Some(&selected)).unwrap();
        let value = export(
            &tr,
            &f,
            None,
            Some(&selected),
            &["command-profile".to_owned()],
            10,
            &TopicArgs::default(),
        );
        let topic = &value["command_profile"];
        assert_eq!(topic["status"], "available");
        assert_eq!(topic["data"], serde_json::to_value(&profile).unwrap());
        assert_eq!(topic["data"]["client_wall_us"], 0);
        assert!(topic.get("bound").is_none());
    }

    #[test]
    fn command_profile_export_preserves_reported_gaps_as_partial() {
        let t = fixture("smoke-trace.json");
        let mut tr = crate::parse::parse(&t).unwrap();
        let x = analyses::load_extras(&tr);
        tr.runs[0].command_profile = Some(crate::command_profile::CommandProfile {
            run: Some(tr.runs[0].run.clone()),
            rejected_records: 2,
            warnings: vec!["one retained observation could not be positioned".to_owned()],
            ..Default::default()
        });
        let f = Facts::for_run(&tr, &x, None).unwrap();
        let value = export(
            &tr,
            &f,
            None,
            None,
            &["command-profile".to_owned()],
            10,
            &TopicArgs::default(),
        );
        let topic = &value["command_profile"];
        assert_eq!(topic["status"], "partial");
        assert_eq!(topic["data"]["rejected_records"], 2);
        assert!(has_lim(topic, "could not be positioned"));
        assert!(topic.get("bound").is_none());
        for limitation in topic["limitations"].as_array().unwrap() {
            let text = limitation.as_str().unwrap();
            assert!(!text.contains('\\'), "{text:?}");
            assert!(!text.contains('\n'), "{text:?}");
        }
    }

    #[test]
    fn command_profile_numeric_gaps_are_partial_without_warning_strings() {
        let mut tr = crate::parse::parse(&fixture("smoke-trace.json")).unwrap();
        let x = analyses::Extras::default();
        for profile in [
            crate::command_profile::CommandProfile {
                lifecycle_open_at_eof: 1,
                ..Default::default()
            },
            crate::command_profile::CommandProfile {
                phases: vec![crate::command_profile::PhaseSummary {
                    union_complete: true,
                    unmatched_ends: 1,
                    ..Default::default()
                }],
                ..Default::default()
            },
        ] {
            assert!(profile.warnings.is_empty());
            tr.runs[0].command_profile = Some(profile);
            let f = Facts::for_run(&tr, &x, None).unwrap();
            let value = export(
                &tr,
                &f,
                None,
                None,
                &["command-profile".into()],
                10,
                &TopicArgs::default(),
            );
            assert_eq!(value["command_profile"]["status"], "partial");
        }
    }

    #[test]
    fn action_only_run_does_not_inherit_another_runs_metadata() {
        let trace_path = crate::temp_path("mixed-run-export-trace.json");
        let graph_path = crate::temp_path("mixed-run-export-graph.json");
        std::fs::write(&trace_path, concat!(
            "[\n",
            r#"{"ph":"M","cat":"run","name":"run_start","ts":100,"args":{"run":"A"}},"#, "\n",
            r#"{"ph":"X","name":"b","ts":110,"dur":20,"args":{"run":"B"}},"#, "\n",
            r#"{"ph":"M","cat":"run","name":"run_end","ts":150,"args":{"run":"A","outcome":"success","exit":0}}"#, "\n",
            "]\n",
        )).unwrap();
        std::fs::write(
            &graph_path,
            "{\"graph\":{\"enc\":2,\"run\":\"B\"}}\n{\"t\":\"b\",\"d\":[]}\n",
        )
        .unwrap();
        let tr = crate::parse::parse(trace_path.to_str().unwrap()).unwrap();
        let x = analyses::load_extras(&tr);
        let f = Facts::for_run(&tr, &x, Some("B")).unwrap();
        let g = crate::graph::load_graph(graph_path.to_str().unwrap()).unwrap();
        let q = quality::Quality::assess(&tr, Some("B"), Some(&g));
        assert_eq!(q.outcome, crate::types::Outcome::Unknown);
        assert!(matches!(q.pairing, Some(quality::Pairing::Verified)));
        let value = export(
            &tr,
            &f,
            Some(&g),
            Some("B"),
            &["critical-path".into(), "command-profile".into()],
            10,
            &TopicArgs::default(),
        );
        assert_eq!(value["trace"]["actions"], 1);
        assert!(value["trace"]["outcome"].is_null());
        assert_eq!(value["command_profile"]["status"], "refused");
        assert_eq!(value["critical_path"]["data"]["cp_us"], 20);
        std::fs::remove_file(trace_path).unwrap();
        std::fs::remove_file(graph_path).unwrap();
    }
    #[test]
    fn declared_header_counts_gate_every_graph_topic_to_partial() {
        let (t, g) = (
            fixture("review-trace.json"),
            fixture("review-declared-missing-graph.jsonl"),
        );
        let want: Vec<&str> = GRAPH_TOPICS
            .iter()
            .chain(TRACE_TOPICS)
            .chain(&["health"])
            .copied()
            .collect();
        let v = run_export(&t, Some(&g), &want);
        let cp = &v["critical_path"];
        assert_eq!(cp["status"], "partial", "{cp}");
        assert_eq!(cp["bound"], "lower", "{cp}");
        assert_eq!(cov(cp, "graph-rows-declared"), Some((2, 200)), "{cp}");
        assert_eq!(cov(cp, "graph-edges-declared"), Some((0, 100)), "{cp}");
        assert_eq!(cov(cp, "actions-with-graph-nodes"), Some((2, 2)), "{cp}");
        assert!(
            has_lim(cp, "declares 200 node row(s), 2 were loaded"),
            "{cp}"
        );
        let h = &v["health"];
        assert_eq!(h["status"], "partial", "{h}");
        assert!(has_lim(h, "200 node row(s)"), "{h}");
        assert!(has_lim(h, "10 dangling reference(s)"), "{h}");
        // Overview includes the dependency CP; genuinely trace-only
        // topics remain independent of graph quality.
        assert_eq!(v["overview"]["status"], "partial");
        assert!(has_lim(&v["overview"], "declares 200 node row(s)"));
        for topic in ["breakdown", "peers", "longest"] {
            assert_eq!(v[topic]["status"], "available", "{topic}: {}", v[topic]);
        }
    }

    #[test]
    fn a_torn_defining_row_reaches_the_json_envelopes() {
        let (t, g) = (
            fixture("review-trace.json"),
            fixture("review-torn-ref-graph.jsonl"),
        );
        let v = run_export(&t, Some(&g), &["critical-path", "health"]);
        let cp = &v["critical_path"];
        assert_eq!(cp["status"], "partial", "{cp}");
        assert_eq!(cp["bound"], "lower", "{cp}");
        assert!(has_lim(cp, "torn tail"), "{cp}");
        assert!(has_lim(cp, "tset:missing"), "{cp}");
        assert!(has_lim(cp, "declares 3 node row(s), 2 were loaded"), "{cp}");
        assert!(
            !v["health"]["limitations"].as_array().unwrap().is_empty(),
            "{}",
            v["health"]
        );
    }

    #[test]
    fn a_known_incomplete_capture_makes_trace_only_topics_partial() {
        let p = crate::temp_path("export-incomplete.json");
        std::fs::write(
            &p,
            concat!(
                "[\n",
                r#"{"ph":"M","cat":"run","name":"run_start","ts":0,"pid":1,"args":{"v":1,"run":"a"}},"#,
                "\n",
                r#"{"ph":"X","cat":"recipe","name":"x","ts":0,"dur":5,"args":{"run":"a"}},"#,
                "\n",
                r#"{"ph":"X","cat":"recipe","name":"y","ts":5,"dur":5,"args":{"run":"a"}},"#,
                "\n",
                r#"{"ph":"M","cat":"run","name":"run_end","ts":10,"pid":1,"args":{"v":1,"run":"a","exit":0,"outcome":"success","dep_capture":{"state":"started","footer_present":false,"finality":"unknown","declared":{},"observed":{"actions_recorded":2},"command_result_seen":true,"command_end_seen":true}}}"#,
                "\n]\n"
            ),
        )
        .unwrap();
        let v = run_export(p.to_str().unwrap(), None, TRACE_TOPICS);
        for topic in TRACE_TOPICS {
            let tv = &v[*topic];
            assert_eq!(tv["status"], "partial", "{topic}: {tv}");
            assert!(has_lim(tv, "no dependency-capture footer"), "{topic}: {tv}");
            assert!(
                cov(tv, "actions-recorded-vs-declared").is_none(),
                "nothing declared"
            );
        }
        std::fs::remove_file(&p).ok();

        // a trace from before the record existed keeps every status
        {
            let t = fixture("smoke-trace.json");
            let v = run_export(&t, None, TRACE_TOPICS);
            for topic in TRACE_TOPICS {
                assert_eq!(v[*topic]["status"], "available", "{topic}");
            }
        }
    }

    #[test]
    fn every_emitted_topic_is_reachable_via_topics() {
        // an asked-block whose name is missing from TOPICS is dead
        // surface area: `--json=<name>` exits 2 as an unknown topic
        let src = include_str!("export.rs");
        for (i, _) in src.match_indices("asked(\"") {
            let rest = &src[i + 7..];
            let t = &rest[..rest.find('"').expect("the literal closes on this line")];
            assert!(
                TOPICS.contains(&t),
                "topic {t:?} is emitted but not in TOPICS — `--json={t}` would be refused"
            );
        }
    }
}
