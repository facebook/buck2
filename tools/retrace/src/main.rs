/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! CLI entry. Exit codes (docs/contract.md): 0 = a result was emitted;
//! 2 = refusal or usage error, with the reason on stderr. A failed
//! BUILD in the trace is data, not a tool failure.

use retrace::analyses;
use retrace::export;
use retrace::graph;
use retrace::parse;
use retrace::types::F_NOT_RECIPE;
use retrace::types::Trace;

fn stdout_write(args: std::fmt::Arguments<'_>, newline: bool) {
    use std::io::Write;
    let mut out = std::io::stdout().lock();
    let result = out.write_fmt(args).and_then(|()| {
        if newline {
            out.write_all(b"\n")
        } else {
            Ok(())
        }
    });
    if let Err(error) = result {
        if error.kind() == std::io::ErrorKind::BrokenPipe {
            std::process::exit(0);
        }
        refuse(&format!("write stdout: {error}"));
    }
}

macro_rules! println { ($($arg:tt)*) => { stdout_write(format_args!($($arg)*), true) }; }
macro_rules! print { ($($arg:tt)*) => { stdout_write(format_args!($($arg)*), false) }; }

/// Prints the help: the `--help` form when `long`, the `-h` summary otherwise. It goes to stdout
/// when asked for, and to stderr alongside a usage error.
fn print_help(long: bool, to_stdout: bool) {
    use std::io::IsTerminal;
    let terminal = if to_stdout {
        std::io::stdout().is_terminal()
    } else {
        std::io::stderr().is_terminal()
    };
    let text = retrace::help::render(long, terminal && std::env::var_os("NO_COLOR").is_none());
    if to_stdout {
        print!("{text}");
    } else {
        eprint!("{text}");
    }
}

/// A usage error: the `-h` summary on stderr, exit 2.
fn usage_error() -> ! {
    print_help(false, false);
    std::process::exit(2);
}

fn refuse(msg: &str) -> ! {
    eprintln!("retrace: {msg}");
    std::process::exit(2);
}

fn bench(tr: &Trace) {
    println!(
        "parse {} ms | index {} ms | {} acts | {} lanes | span {:.1}s",
        tr.parse_ms,
        tr.index_ms,
        tr.acts
            .iter()
            .filter(|a| a.flags & F_NOT_RECIPE == 0)
            .count(),
        tr.lanes.len(),
        tr.span as f64 / 1e6
    );
    let width = 1600.0f32;
    for (label, upp, rows) in [
        (
            "fit-all-lanes",
            tr.span as f64 / width as f64,
            tr.lanes.len(),
        ),
        ("zoom-100x", tr.span as f64 / width as f64 / 100.0, 90),
        ("zoom-10000x", tr.span as f64 / width as f64 / 10000.0, 90),
    ] {
        let t = std::time::Instant::now();
        let mut n = (0, 0);
        let reps = 20;
        for _ in 0..reps {
            n = parse::cull(
                tr,
                tr.span as f64 * 0.3,
                upp,
                0,
                rows,
                width,
                |_, _, _, _| {},
            );
        }
        println!(
            "{label:14} rows={rows:6} painted={:7} merged={:8} cull_ms={:.2}",
            n.0,
            n.1,
            t.elapsed().as_secs_f64() * 1000.0 / reps as f64
        );
    }
}

/// Refuses the first argument that looks like an option and is not in
/// `known`.  Every entry path validates through here, so a verb that
/// owns its own flag world cannot forget to.
fn refuse_unknown_flags(args: &[String], known: &[impl AsRef<str>]) {
    let Some(bad) = args.iter().find(|a| {
        a.starts_with('-')
            && !known
                .iter()
                .map(AsRef::as_ref)
                .any(|k| match k.strip_suffix('=') {
                    Some(_) => a.starts_with(k),
                    None => a.as_str() == k,
                })
    }) else {
        return;
    };
    refuse(&format!("unknown option {bad} (see 'retrace --help')"));
}

/// Resolve syntax before opening a source or creating any artifacts.
fn validate_args<'a>(args: &'a [String], known: &[impl AsRef<str>]) -> Option<&'a String> {
    refuse_unknown_flags(args, known);
    let mut keys = std::collections::HashSet::new();
    let mut positional = None;
    for arg in args {
        if arg.starts_with('-') {
            let key = arg.split('=').next().unwrap_or(arg);
            if !keys.insert(key) {
                refuse(&format!("duplicate option {key}"));
            }
            if arg.ends_with('=') {
                refuse(&format!("{key} requires a non-empty value"));
            }
        } else if positional.replace(arg).is_some() {
            refuse("expected one input; name a graph with --graph=PATH");
        }
    }
    if positional.is_some_and(|p| p.is_empty()) {
        refuse("input must not be empty; use --last to open the latest command");
    }
    if positional.is_some() && keys.contains("--last") {
        refuse("choose either --last or an input path / Command ID, not both");
    }
    positional
}

fn main() {
    let args: Vec<String> = std::env::args_os()
        .skip(1)
        .map(|arg| {
            arg.into_string()
                .unwrap_or_else(|_| refuse("arguments must be valid UTF-8"))
        })
        .collect();
    if args.iter().any(|a| a == "--help") {
        print_help(true, true);
        std::process::exit(0);
    }
    if args.iter().any(|a| a == "-h") {
        print_help(false, true);
        std::process::exit(0);
    }
    // options may appear in any position
    let path = match validate_args(&args, &retrace::help::known_flags()) {
        Some(p) => p.clone(),
        None => usage_error(),
    };
    let mut opts = parse::LoadOpts::default();
    if let Some(v) = args
        .iter()
        .find_map(|a| a.strip_prefix("--max-input-bytes="))
    {
        opts.max_input_bytes = v
            .parse()
            .unwrap_or_else(|_| refuse("--max-input-bytes wants an integer byte count"));
    }
    if let Some(v) = args.iter().find_map(|a| a.strip_prefix("--threads=")) {
        opts.threads = v
            .parse()
            .unwrap_or_else(|_| refuse("--threads wants an integer"));
    }
    let topk: usize = args
        .iter()
        .find_map(|a| a.strip_prefix("--topk="))
        .map(|v| {
            let n: usize = v
                .parse()
                .unwrap_or_else(|_| refuse("--topk wants a positive integer"));
            if n == 0 {
                // an empty "available" table would look like data
                refuse("--topk wants a positive integer");
            }
            n
        })
        .unwrap_or(export::DEFAULT_TOPK);

    // validate --json topics BEFORE the (possibly long) parse
    let json_spec = args
        .iter()
        .find(|a| *a == "--json" || a.starts_with("--json="));
    let json_want: Option<Vec<String>> = json_spec.map(|spec| {
        spec.strip_prefix("--json=")
            .map(|v| v.split(',').map(|s| s.trim().to_owned()).collect())
            .unwrap_or_default()
    });
    if let Some(want) = &json_want {
        let unknown: Vec<&String> = want
            .iter()
            .filter(|w| *w != "all" && !export::TOPICS.contains(&w.as_str()))
            .collect();
        if !unknown.is_empty() {
            refuse(&format!(
                "unknown --json topic(s): {}\nknown: all, {}",
                unknown
                    .iter()
                    .map(|s| s.as_str())
                    .collect::<Vec<_>>()
                    .join(", "),
                export::TOPICS.join(", ")
            ));
        }
    }

    // no headless verb: open the window (the GUI parses on its own
    // worker thread so it opens instantly)
    let headless = json_want.is_some()
        || args
            .iter()
            .any(|a| a == "--stats" || a == "--bench" || a == "--summary");
    let has = |key: &str| {
        args.iter()
            .any(|a| a == key || a.starts_with(&format!("{key}=")))
    };
    let output_modes = usize::from(json_want.is_some())
        + usize::from(has("--stats"))
        + usize::from(has("--summary"));
    if output_modes > 1 {
        refuse("choose one output mode: --json, --stats or --summary");
    }
    if has("--bench") && (json_want.is_some() || has("--stats")) {
        refuse("--bench adds timing to the summary; it cannot accompany --json or --stats");
    }
    if !headless
        && args.iter().any(|a| {
            ["--repair-cycles", "--topk", "--compact", "--action"]
                .iter()
                .any(|k| a == k || a.starts_with(&format!("{k}=")))
        })
    {
        refuse(
            "analysis options (including --repair-cycles) require --json, --stats or --summary; they do not alter the window",
        );
    }
    let topic_requested = |topic: &str| {
        json_want
            .as_ref()
            .is_some_and(|w| w.is_empty() || w.iter().any(|s| s == "all" || s == topic))
    };
    if has("--compact") && json_want.is_none() {
        refuse("--compact requires --json");
    }
    if has("--action") && !topic_requested("action-detail") {
        refuse("--action requires --json=action-detail (or all)");
    }
    let no_extras = json_want.as_ref().is_some_and(|w| {
        !w.is_empty()
            && w.iter()
                .all(|t| matches!(t.as_str(), "command-profile" | "action-detail"))
    });

    let mut tr = match parse::parse_opts(&path, &opts) {
        Ok(tr) => tr,
        // {:#} renders the whole context chain on one line
        Err(e) => refuse(&format!("{e:#}")),
    };
    let run_sel = args.iter().find_map(|a| a.strip_prefix("--run="));
    let run_id = match parse::select_trace_run(&tr, run_sel) {
        Ok(run) => run,
        Err(e) => refuse(&format!("{e:#}")),
    };

    let explicit = args
        .iter()
        .find_map(|a| a.strip_prefix("--graph=").map(|s| s.to_owned()));
    let discovered = explicit.is_none();
    let gp = explicit.or_else(|| {
        (!no_extras)
            .then(|| graph::sibling_graph_path(&path))
            .flatten()
    });
    let g = match &gp {
        // an AUTO-DISCOVERED sidecar must not take down analyses that
        // never asked for a graph; an explicitly named one is the
        // user's input and refuses loudly (docs/contract.md). The pair
        // check follows the same policy.
        Some(p) => match graph::load_graph_opts(
            p,
            &graph::GraphLoadOpts {
                max_input_bytes: opts.max_input_bytes,
                repair_cycles: args.iter().any(|a| a == "--repair-cycles"),
                ..Default::default()
            },
        ) {
            Ok(g) => {
                let pairing = graph::check_trace_pair(&tr, run_id.as_deref(), &g.meta);
                match pairing {
                    Ok(()) => Some(g),
                    Err(e) if discovered => {
                        eprintln!("warning: ignoring the graph beside this trace: {e:#}");
                        tr.health.graph_warning =
                            Some(format!("auto-discovered graph rejected: {e:#}"));
                        None
                    }
                    Err(e) => refuse(&format!("{e:#}")),
                }
            }
            Err(e) if discovered => {
                eprintln!("warning: ignoring auto-discovered graph sidecar: {e:#}");
                tr.health.graph_warning = Some(format!("auto-discovered graph rejected: {e:#}"));
                None
            }
            Err(e) => refuse(&format!("{e:#}")),
        },
        None => None,
    };

    if let Some(want) = json_want {
        let x = if no_extras {
            analyses::Extras::default()
        } else {
            analyses::load_extras(&tr)
        };
        if let Some(e) = &x.source_error {
            refuse(e);
        }
        let f = match analyses::Facts::for_run(&tr, &x, run_id.as_deref()) {
            Ok(f) => f,
            Err(e) => refuse(&format!("{e:#}")),
        };
        let ta = export::TopicArgs {
            action: args.iter().find_map(|a| a.strip_prefix("--action=")),
        };
        let v = export::export(&tr, &f, g.as_ref(), run_id.as_deref(), &want, topk, &ta);
        let pretty = !args.iter().any(|a| a == "--compact");
        println!(
            "{}",
            if pretty {
                serde_json::to_string_pretty(&v).expect("a serde_json::Value always serializes")
            } else {
                serde_json::to_string(&v).expect("a serde_json::Value always serializes")
            }
        );
        return;
    }

    if args.iter().any(|a| a == "--stats") {
        let x = analyses::load_extras(&tr);
        if let Some(e) = &x.source_error {
            refuse(e);
        }
        let f = match analyses::Facts::for_run(&tr, &x, run_id.as_deref()) {
            Ok(f) => f,
            Err(e) => refuse(&format!("{e:#}")),
        };
        print!(
            "{}",
            export::stats_text(&tr, &f, g.as_ref(), run_id.as_deref())
        );
        return;
    }

    // With no output option, a one-line load summary.
    let selected = tr.selected_summary(run_id.as_deref());
    println!(
        "{}: {} actions ({} cache hits, {} failed) | span {:.1}s | {} lanes{}",
        tr.path,
        retrace::humanize::count(selected.actions as u64),
        retrace::humanize::count(selected.cache_hits as u64),
        retrace::humanize::count(selected.failed as u64),
        selected.span_us as f64 / 1e6,
        selected.lanes,
        match &run_id {
            Some(r) => format!(" | run {r}"),
            None => String::new(),
        }
    );
    for w in &tr.health.warnings {
        println!("warning: {w}");
    }
    if let Some(g) = &g {
        println!(
            "{}: {} nodes ({} joins) | {} edges | load {:.2}s{}{}{}{}{}",
            g.path,
            retrace::humanize::count(g.n_rows as u64),
            retrace::humanize::count(g.is_join.iter().filter(|&&b| b).count() as u64),
            retrace::humanize::count(g.n_edges as u64),
            g.load_ms as f64 / 1e3,
            // the header's own accounting, reconciled against what loaded
            match g.meta.coverage.as_ref().and_then(|c| c.nodes.zip(c.edges)) {
                Some((n, e)) => format!(
                    " | declared {} nodes / {} edges{}",
                    retrace::humanize::count(n.max(0) as u64),
                    retrace::humanize::count(e.max(0) as u64),
                    if n != g.n_rows as i64 || e != g.n_edges as i64 {
                        " — PARTIAL"
                    } else {
                        ""
                    }
                ),
                None => String::new(),
            },
            if g.dropped_lines > g.dropped_interior {
                format!(" | {} torn line(s)", g.dropped_lines - g.dropped_interior)
            } else {
                String::new()
            },
            if g.dropped_interior > 0 {
                format!(
                    " | {} malformed line(s) inside the file",
                    g.dropped_interior
                )
            } else {
                String::new()
            },
            if g.undefined_joins + g.undefined_deps > 0 {
                format!(
                    " | {} undefined ref(s) — PARTIAL",
                    g.undefined_joins + g.undefined_deps
                )
            } else {
                String::new()
            },
            if g.targets_in_cycles > 0 {
                format!(
                    " | {} target(s) on a cycle — REPAIRED, not a bound",
                    g.targets_in_cycles
                )
            } else {
                String::new()
            },
        );
        let t = std::time::Instant::now();
        match retrace::dag::build_full(&tr, g, run_id.as_deref()) {
            Some((b, st)) => {
                println!(
                    "stitch {:.2}s | {} nodes | {} edges | T1 {:.1}s | CP {:.6}s | \
                     matched (defined rows) {}/{}{}{}{}",
                    t.elapsed().as_secs_f64(),
                    retrace::humanize::count(b.dag.weight_us.len() as u64),
                    retrace::humanize::count(b.dag.preds.len() as u64),
                    b.dag.t1 as f64 / 1e6,
                    b.dag.cp as f64 / 1e6,
                    retrace::humanize::count(st.n_matched as u64),
                    retrace::humanize::count(st.n_events as u64),
                    if st.n_unattributed > 0 {
                        format!(
                            " | {} event(s) dropped: no run id among several runs",
                            st.n_unattributed
                        )
                    } else {
                        String::new()
                    },
                    if st.n_cycle_edges > 0 {
                        format!(
                            " | {} cycle edge(s) dropped — REPAIRED, not a bound",
                            st.n_cycle_edges
                        )
                    } else {
                        String::new()
                    },
                    if st.n_named_only > 0 {
                        format!(" | {} named only (defining row missing)", st.n_named_only)
                    } else {
                        String::new()
                    },
                );
                let t2 = std::time::Instant::now();
                {
                    let c = retrace::dag::chain_from_bundle(&b, &st);
                    println!(
                        "chain  {:.2}s | CP {:.6}s over {} steps",
                        t2.elapsed().as_secs_f64(),
                        c.cp_us as f64 / 1e6,
                        c.entries.len()
                    );
                }
            }
            None => println!("stitch: nothing to stitch (no events for this run)"),
        }
    }
    if args.iter().any(|a| a == "--bench") {
        bench(&tr);
    }
}
