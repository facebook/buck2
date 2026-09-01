/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! A/Bs two prebuilt [`compute_bench`] binaries.
//!
//! The two binaries are built from different revisions, so the comparison shows what a
//! change did to DICE's own per-`compute` cost. Both are run from one process, interleaved,
//! which is what makes the numbers comparable: a machine that gets busier or hotter part way
//! through a run affects both arms rather than whichever happened to run second.
//!
//!   buck2 run @fbcode//mode/opt fbcode//buck2/dice/dice_examples:ab_compute_bench -- \
//!       --a /tmp/compute_bench.before --b /tmp/compute_bench.after --reps 10 \
//!       -- --num-keys 10000 --warm-rounds 200
//!
//! Build the two arms with the same mode (`@fbcode//mode/opt`), or the comparison measures
//! the optimisation level rather than the change.
//!
//! Reports the median of each arm because these distributions have a hard floor and a long
//! tail: the fastest run is the one that was least interrupted, and outliers are always
//! slow. The min–max range is printed alongside so a delta smaller than the spread is
//! visible as such.

use std::collections::BTreeMap;
use std::process::Command;

use anyhow::Context;
use clap::Parser;
use table::ArmStats;
use table::Comparison;

#[derive(Parser)]
struct Cli {
    /// Baseline `compute_bench` binary.
    #[arg(long)]
    a: String,
    /// Candidate `compute_bench` binary.
    #[arg(long)]
    b: String,
    /// ABBA rounds. Each runs both binaries twice.
    #[arg(long, default_value_t = 5)]
    reps: u32,
    /// Arguments forwarded to both `compute_bench` binaries.
    #[arg(trailing_var_arg = true, allow_hyphen_values = true)]
    bench_args: Vec<String>,
}

/// One run of the bench binary.
struct Run {
    /// `ns_per_compute` per stage, plus peak RSS.
    metrics: BTreeMap<String, f64>,
    /// The `compute` count each stage's rate was divided by, kept so the two arms can be
    /// checked for agreement before their rates are compared.
    computes: BTreeMap<String, u64>,
}

fn run(bin: &str, bench_args: &[String]) -> anyhow::Result<Run> {
    let out = Command::new(bin)
        .args(bench_args)
        .output()
        .with_context(|| format!("failed to run {bin}"))?;
    anyhow::ensure!(
        out.status.success(),
        "{bin} failed:\n{}",
        String::from_utf8_lossy(&out.stderr)
    );

    let mut metrics = BTreeMap::new();
    let mut computes = BTreeMap::new();
    for line in String::from_utf8(out.stdout)?.lines() {
        let row: serde_json::Value = serde_json::from_str(line)
            .with_context(|| format!("{bin} emitted a non-JSON line: {line}"))?;
        match row.get("type").and_then(|t| t.as_str()) {
            Some("rate") => {
                let stage = row["stage"].as_str().context("rate row without a stage")?;
                metrics.insert(
                    format!("{stage}_ns"),
                    row["ns_per_compute"]
                        .as_f64()
                        .context("rate row without ns_per_compute")?,
                );
                if let Some(n) = row.get("computes").and_then(|v| v.as_u64()) {
                    computes.insert(stage.to_owned(), n);
                }
            }
            // Stage rows carry no `type`. `emit_stage` resets the peak after emitting a row,
            // so each row covers only its own stage and the run's peak is the max of them.
            None if row.get("stage").is_some() => {
                if let Some(peak) = row.get("peak_rss_bytes").and_then(|v| v.as_f64()) {
                    metrics
                        .entry("peak_rss_bytes".to_owned())
                        .and_modify(|e| *e = f64::max(*e, peak))
                        .or_insert(peak);
                }
            }
            _ => {}
        }
    }
    anyhow::ensure!(!metrics.is_empty(), "{bin} emitted no metrics");
    Ok(Run { metrics, computes })
}

fn stats(runs: &[Run], metric: &str, arm: &str, bin: &str) -> anyhow::Result<ArmStats> {
    let mut values: Vec<f64> = runs
        .iter()
        .filter_map(|r| r.metrics.get(metric).copied())
        .collect();
    // The metric list comes from arm A, so a B binary built from a revision with a different
    // set of stages lands here rather than indexing off the end of an empty vec.
    anyhow::ensure!(
        !values.is_empty(),
        "arm {arm} ({bin}) never reported `{metric}`, so the two binaries do not run the same \
         stages and this metric cannot be compared"
    );
    values.sort_by(f64::total_cmp);
    Ok(ArmStats {
        median: values[values.len() / 2],
        min: values[0],
        max: values[values.len() - 1],
    })
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();
    // Strip the `--` that separates our arguments from the bench binary's.
    let bench_args: Vec<String> = cli
        .bench_args
        .iter()
        .skip_while(|a| *a == "--")
        .cloned()
        .collect();

    // Discard the first run of each arm: it pays for page faults and for warming the CPU's
    // caches and frequency, and would otherwise land in the min.
    run(&cli.a, &bench_args)?;
    run(&cli.b, &bench_args)?;

    let mut a_runs = Vec::new();
    let mut b_runs = Vec::new();
    for rep in 0..cli.reps {
        eprintln!("rep {}/{}", rep + 1, cli.reps);
        a_runs.push(run(&cli.a, &bench_args)?);
        b_runs.push(run(&cli.b, &bench_args)?);
        b_runs.push(run(&cli.b, &bench_args)?);
        a_runs.push(run(&cli.a, &bench_args)?);
    }

    // A rate is only comparable if both arms divided by the same number of computes. Two
    // binaries whose benchmark sources disagree about that denominator would otherwise report
    // a delta that silently means nothing.
    for (stage, a_computes) in &a_runs[0].computes {
        let b_computes = b_runs[0].computes.get(stage);
        anyhow::ensure!(
            b_computes == Some(a_computes),
            "stage `{stage}` divides by {a_computes} computes in A but {} in B; the arms' \
             denominators differ, so their rates are not comparable",
            b_computes.map_or_else(|| "nothing".to_owned(), u64::to_string),
        );
    }

    // `a_runs[0].metrics` is a `BTreeMap`, so its keys already come out ordered.
    let metrics: Vec<String> = a_runs[0].metrics.keys().cloned().collect();
    let comparisons: Vec<Comparison> = metrics
        .iter()
        .map(|metric| {
            Ok(Comparison {
                metric: metric.clone(),
                a: stats(&a_runs, metric, "A", &cli.a)?,
                b: stats(&b_runs, metric, "B", &cli.b)?,
            })
        })
        .collect::<anyhow::Result<_>>()?;

    println!("A = {}", cli.a);
    println!("B = {}", cli.b);
    println!("args = {}", bench_args.join(" "));
    println!("{} runs per arm\n", a_runs.len());
    println!("{}", table::comparison_table(&comparisons));
    Ok(())
}
