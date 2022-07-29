/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![feature(exclusive_range_pattern)]
#![feature(async_closure)]

use std::fs;
use std::path::Path;
use std::path::PathBuf;

use anyhow::Context;
use clap::Args;
use clap::FromArgMatches;
use clap::Parser;
use quickcheck::Arbitrary;
use quickcheck::Gen;
use quickcheck::QuickCheck;
use quickcheck::TestResult;
use quickcheck::Testable;
use thiserror::Error;
use tracing_subscriber::fmt;
use tracing_subscriber::prelude::*;
use tracing_subscriber::EnvFilter;

mod computation;
mod execution;

use crate::execution::DiceExecutionOrder;
use crate::execution::DiceExecutionOrderOptions;

#[derive(Error, Debug)]
pub enum DiceFuzzError {
    #[error("couldn't parse provided replay file")]
    UnparsableReplay,
}

fn magical_cleanup(stderr: &str) -> anyhow::Result<&str> {
    // The last line of our output looks like (e.g.)
    // thread 'main' panicked at '[quickcheck] TEST FAILED. Arguments: ({"init_vars":[{"SetValue":{"new_ctx_id":1,"var":1,"expr":{"Unit":{"Literal":true}}}}],"timeline":[{"SetValue":{"new_ctx_id":3,"var":1,"expr":{"Unit":{"Literal":true}}}},{"Query":{"ctx_id":3,"var":1}},{"EnqueueStep":[1,["ReturnTransient"]]},{"EnqueueStep":[1,["ReturnTransient"]]},{"EnqueueStep":[1,["ReturnTransient"]]},{"Query":{"ctx_id":1,"var":1}},{"SetValue":{"new_ctx_id":9,"var":1,"expr":{"Unit":{"Literal":false}}}},{"SetValue":{"new_ctx_id":12,"var":1,"expr":{"Unit":{"Literal":true}}}},{"Query":{"ctx_id":9,"var":1}},{"Query":{"ctx_id":12,"var":1}},{"SetValue":{"new_ctx_id":14,"var":1,"expr":{"Unit":{"Literal":true}}}},{"SetValue":{"new_ctx_id":15,"var":1,"expr":{"Unit":{"Literal":true}}}},{"Query":{"ctx_id":15,"var":1}},{"Query":{"ctx_id":14,"var":1}},{"SetValue":{"new_ctx_id":17,"var":1,"expr":{"Unit":{"Literal":true}}}},{"Query":{"ctx_id":17,"var":1}}],"is_shrinking":true})', third-party/rust/vendor/quickcheck-1.0.3/src/tester.rs:165:28
    stderr
        .rsplit_once("TEST FAILED. Arguments: (")
        .and_then(|(_, suffix)| suffix.rsplit_once(')'))
        .map(|(prefix, _)| prefix)
        .ok_or_else(|| DiceFuzzError::UnparsableReplay.into())
}

fn execution_order_from_path(filepath: &Path) -> anyhow::Result<DiceExecutionOrder> {
    let buf = std::fs::read_to_string(filepath)?;
    if let Ok(ex) = serde_json::from_reader::<_, DiceExecutionOrder>(&mut buf.as_bytes()) {
        return Ok(ex);
    }
    // Well, maybe it's our own stderr and we can parse it?
    let cleaned_up = magical_cleanup(&buf)?;
    serde_json::from_reader::<_, DiceExecutionOrder>(&mut cleaned_up.as_bytes())
        .context(format!("While parsing {}", &cleaned_up))
}

#[derive(clap::Parser)]
#[clap(
    name = "fuzzy-dice",
    about = "a tool for finding bugs in DICE by simulating many different computations"
)]
struct Opts {
    #[clap(subcommand)]
    command: Commands,
}

#[derive(clap::Subcommand)]
enum Commands {
    #[clap(about = "Replays an existing failure.")]
    Replay(SubCommandCommon<Replay>),
    #[clap(about = "Searches for new failures.")]
    Fuzz(SubCommandCommon<Fuzz>),
}

#[derive(Parser)]
struct SubCommandCommon<T: FromArgMatches + Args> {
    #[clap(flatten)]
    cmd: T,
    #[clap(
        long,
        value_parser,
        help = "If set, prints a DICE-dump as JSON to given location after each operation."
    )]
    print_dumps: Option<PathBuf>,
}

#[derive(Parser)]
struct Replay {
    #[clap(
        value_parser,
        help = "the path to the file containing the execution to replay"
    )]
    path: PathBuf,
}

#[derive(Parser)]
struct Fuzz {
    #[clap(
        default_value_t = 2_000_000,
        help = "The maximum number of tests for fuzzing. The actual number may be lower due to\
            discarded test cases"
    )]
    max_tests: u64,
    #[clap(
        default_value_t = 2_000_000,
        help = "The number of passes to hit before stopping and considering it as a pass"
    )]
    num_tests: u64,
}

fn main() -> anyhow::Result<()> {
    #[tokio::main]
    async fn replay(
        options: &DiceExecutionOrderOptions,
        execution: DiceExecutionOrder,
    ) -> anyhow::Result<()> {
        let res = execution.execute(options).await;

        if res.is_error() || res.is_failure() {
            Err(anyhow::anyhow!(format!("{:?}", res)))
        } else {
            Ok(())
        }
    }

    struct Fuzzer {
        options: DiceExecutionOrderOptions,
    }

    impl Fuzzer {
        async fn shrink_failure(&self, execution: DiceExecutionOrder) -> Option<TestResult> {
            let mut res = None;
            let mut executions = execution.shrink();

            while let Some(exec) = executions.next() {
                let r_new = qc_fuzz(&exec, &self.options).await;
                if r_new.is_failure() {
                    eprintln!("Found failure at {}... continuing to shrink", exec.uuid);

                    res = Some(r_new);

                    executions = exec.shrink();
                }
            }

            res
        }
    }

    impl Testable for Fuzzer {
        #[tokio::main]
        async fn result(&self, g: &mut Gen) -> TestResult {
            let execution = DiceExecutionOrder::arbitrary(g);
            let r = qc_fuzz(&execution, &self.options).await;

            if r.is_failure() {
                eprintln!(
                    "Found failure at execution {}. Beginning shrinking...",
                    execution.uuid
                );
                self.shrink_failure(execution).await.unwrap_or(r)
            } else {
                r
            }
        }
    }

    async fn qc_fuzz(
        execution: &DiceExecutionOrder,
        options: &DiceExecutionOrderOptions,
    ) -> TestResult {
        let res = execution.execute(options).await;

        if !(res.is_error() || res.is_failure()) {
            if let Some(dump_loc) = execution.get_dump_dir(options) {
                // we don't keep dump of anything that are not a failure
                if dump_loc.exists() {
                    fs::remove_dir_all(dump_loc).expect("failed to remove dump");
                }
            }
        }

        res
    }

    let cmd = Opts::parse();

    match cmd.command {
        Commands::Fuzz(fuzz) => {
            let fuzzer = Fuzzer {
                options: DiceExecutionOrderOptions {
                    print_dumps: fuzz.print_dumps,
                },
            };

            QuickCheck::new()
                .max_tests(fuzz.cmd.max_tests)
                .tests(fuzz.cmd.num_tests)
                .gen(Gen::new(10))
                .quickcheck(fuzzer);
        }
        Commands::Replay(replay_cmd) => {
            tracing_subscriber::registry()
                .with(fmt::layer())
                .with(EnvFilter::from_default_env())
                .init();
            let execution = execution_order_from_path(&replay_cmd.cmd.path)?;
            let options = DiceExecutionOrderOptions {
                print_dumps: replay_cmd.print_dumps,
            };
            replay(&options, execution)?;
        }
    }

    println!("Fuzzing complete.");
    Ok(())
}
