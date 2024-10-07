/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::path::PathBuf;
use std::process::Stdio;

use anyhow::Context as _;
use buck2_cli_proto::protobuf_util::ProtobufSplitter;
use buck2_subscription_proto::subscription_response::Response;
use buck2_subscription_proto::Materialized;
use buck2_subscription_proto::SubscribeToPaths;
use buck2_subscription_proto::SubscriptionRequest;
use buck2_subscription_proto::SubscriptionResponse;
use clap::Parser;
use futures::stream::TryStreamExt;
use prost::Message;
use tokio::io::AsyncWriteExt;
use tokio::process::Command;
use tokio_util::codec::FramedRead;

#[derive(Parser)]
struct Opt {
    /// Path to the Buck2 binary
    #[clap(long, default_value = "buck2")]
    buck2: PathBuf,

    /// Optional isolation dir
    #[clap(long)]
    isolation_dir: Option<String>,

    /// Path to expect
    expect: String,
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let Opt {
        buck2,
        isolation_dir,
        expect,
    } = Parser::parse();

    let mut command = Command::new(buck2);
    command.stdin(Stdio::piped());
    command.stdout(Stdio::piped());
    command.stderr(Stdio::inherit());

    if let Some(isolation_dir) = isolation_dir {
        command.arg("--isolation-dir");
        command.arg(isolation_dir);
    }

    command.arg("subscribe");
    let mut command = command.spawn().context("Error spawning")?;

    let mut stdin = command.stdin.take().unwrap();
    let stdout = command.stdout.take().unwrap();

    let req = SubscriptionRequest {
        request: Some(
            SubscribeToPaths {
                paths: vec![expect.clone()],
            }
            .into(),
        ),
    }
    .encode_length_delimited_to_vec();

    stdin.write_all(&req).await?;
    stdin.flush().await?;

    let mut stream = FramedRead::new(stdout, ProtobufSplitter);
    let mut msg = stream.try_next().await?.context("was disconnected")?;
    let res = SubscriptionResponse::decode_length_delimited(&mut msg).context("Error decoding")?;

    match res.response.as_ref().context("Empty response")? {
        Response::Materialized(Materialized { path }) if *path == expect => {
            println!("{}", path);
            Ok(())
        }
        _ => Err(anyhow::anyhow!("Unexpected response: {:?}", res)),
    }
}
