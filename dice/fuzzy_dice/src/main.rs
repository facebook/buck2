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

use anyhow::Context;
use clap::Arg;
use clap::Command;
use futures::FutureExt;
use quickcheck::Gen;
use quickcheck::QuickCheck;
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

fn execution_order_from_path(filepath: &str) -> anyhow::Result<DiceExecutionOrder> {
    let buf = std::fs::read_to_string(filepath)?;
    if let Ok(ex) = serde_json::from_reader::<_, DiceExecutionOrder>(&mut buf.as_bytes()) {
        return Ok(ex);
    }
    // Well, maybe it's our own stderr and we can parse it?
    let cleaned_up = magical_cleanup(&buf)?;
    serde_json::from_reader::<_, DiceExecutionOrder>(&mut cleaned_up.as_bytes())
        .context(format!("While parsing {}", &cleaned_up))
}

#[allow(deprecated)] // TODO(nga): use non-deprecated API.
fn main() -> anyhow::Result<()> {
    #[tokio::main]
    async fn replay(options: &DiceExecutionOrderOptions, execution: DiceExecutionOrder) {
        execution.execute(options).await.unwrap();
    }
    #[tokio::main]
    async fn qc_fuzz(execution: DiceExecutionOrder) -> bool {
        std::panic::AssertUnwindSafe(async {
            execution
                .execute(&DiceExecutionOrderOptions { print_dumps: false })
                .await
                .unwrap();
        })
        .catch_unwind()
        .await
        .is_ok()
    }

    let cmd = Command::new("fuzzy-dice")
        .version("0.1")
        .about("a tool for finding bugs in DICE by simulating many different computations")
        .subcommand(Command::new("fuzz").about("Searches for new failures."))
        .subcommand(
            Command::new("replay")
                .about("Replays an existing failure.")
                .arg(Arg::new("path").required(true).takes_value(true))
                .arg(
                    Arg::new("print-dumps")
                        .long("print-dumps")
                        .takes_value(false)
                        .help("If set, prints a DICE-dump as JSON to stderr after each operation."),
                ),
        )
        .arg_required_else_help(true);

    let matches = cmd.get_matches();

    match matches.subcommand() {
        Some(("fuzz", _submatches)) => {
            QuickCheck::new()
                .max_tests(2_000_000)
                .tests(2_000_000)
                .gen(Gen::new(10))
                .quickcheck(qc_fuzz as fn(DiceExecutionOrder) -> bool);
        }
        Some(("replay", submatches)) => {
            tracing_subscriber::registry()
                .with(fmt::layer())
                .with(EnvFilter::from_default_env())
                .init();
            let execution = execution_order_from_path(submatches.value_of("path").unwrap())?;
            let options = DiceExecutionOrderOptions {
                print_dumps: submatches.is_present("print-dumps"),
            };
            replay(&options, execution);
        }
        _ => unreachable!("clap should ensure we don't get here"),
    }
    Ok(())
}
