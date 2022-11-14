/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::ffi::OsStr;
use std::os::unix::ffi::OsStrExt;
use std::pin::Pin;

use anyhow::Context as _;
use buck2_common::convert::ProstDurationExt;
use buck2_core::process::background_command;
use buck2_forkserver_proto::forkserver_server::Forkserver;
use buck2_forkserver_proto::CommandRequest;
use buck2_forkserver_proto::RequestEvent;
use buck2_grpc::to_tonic;
use futures::future::select;
use futures::future::FutureExt;
use futures::stream::Stream;
use tonic::Request;
use tonic::Response;
use tonic::Status;
use tonic::Streaming;

use crate::convert::encode_event_stream;
use crate::run::prepare_command;
use crate::run::stream_command_events;
use crate::run::timeout_into_cancellation;
use crate::run::GatherOutputStatus;

// Not quite BoxStream: it has to be Sync (...)
type RunStream =
    Pin<Box<dyn Stream<Item = Result<buck2_forkserver_proto::CommandEvent, Status>> + Send>>;

pub struct UnixForkserverService;

#[async_trait::async_trait]
impl Forkserver for UnixForkserverService {
    type RunStream = RunStream;

    async fn run(
        &self,
        req: Request<Streaming<RequestEvent>>,
    ) -> Result<Response<Self::RunStream>, Status> {
        to_tonic(async move {
            let mut stream = req.into_inner();

            let msg = stream
                .message()
                .await?
                .and_then(|m| m.data)
                .and_then(|m| m.into_command_request())
                .context("RequestEvent was not a CommandRequest!")?;

            let cancel = async move {
                stream
                    .message()
                    .await?
                    .and_then(|m| m.data)
                    .and_then(|m| m.into_cancel_request())
                    .context("RequestEvent was not a CancelRequest!")?;

                anyhow::Ok(GatherOutputStatus::Cancelled)
            };

            let CommandRequest {
                exe,
                argv,
                env,
                cwd,
                timeout,
            } = msg;

            let exe = OsStr::from_bytes(&exe);
            let cwd = cwd.as_ref().map(|c| OsStr::from_bytes(&c.path));
            let argv = argv.iter().map(|a| OsStr::from_bytes(a));
            let timeout = timeout
                .map(|t| t.try_into_duration())
                .transpose()
                .context("Invalid timeout")?;

            let mut cmd = background_command(exe);
            if let Some(cwd) = cwd {
                cmd.current_dir(cwd);
            }
            cmd.args(argv);

            {
                use buck2_forkserver_proto::env_directive::Data;

                for directive in env {
                    match directive.data.context("EnvDirective is missing data")? {
                        Data::Clear(..) => {
                            cmd.env_clear();
                        }
                        Data::Set(var) => {
                            cmd.env(OsStr::from_bytes(&var.key), OsStr::from_bytes(&var.value));
                        }
                        Data::Remove(var) => {
                            cmd.env_remove(OsStr::from_bytes(&var.key));
                        }
                    }
                }
            }

            let mut cmd = prepare_command(cmd);

            let child = cmd.spawn().context("Spawn failed")?;

            let timeout = timeout_into_cancellation(timeout);

            let cancellation = select(timeout.boxed(), cancel.boxed()).map(|r| r.factor_first().0);

            let stream = stream_command_events(child, cancellation)?;
            let stream = encode_event_stream(stream);
            Ok(Box::pin(stream) as _)
        })
        .await
    }
}
