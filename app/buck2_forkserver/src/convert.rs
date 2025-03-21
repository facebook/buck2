/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_common::convert::ProstDurationExt;
use buck2_error::BuckErrorContext;
use futures::stream::Stream;
use futures::stream::StreamExt;

use crate::run::CommandEvent;
use crate::run::GatherOutputStatus;

#[allow(dead_code)]
pub(crate) fn encode_event_stream<S>(
    s: S,
) -> impl Stream<Item = Result<buck2_forkserver_proto::CommandEvent, tonic::Status>>
where
    S: Stream<Item = buck2_error::Result<CommandEvent>>,
{
    fn convert_event(e: CommandEvent) -> buck2_forkserver_proto::CommandEvent {
        use buck2_forkserver_proto::command_event::Data;

        let data = match e {
            CommandEvent::Stdout(bytes) => Data::Stdout(buck2_forkserver_proto::StreamEvent {
                data: bytes.to_vec(),
            }),
            CommandEvent::Stderr(bytes) => Data::Stderr(buck2_forkserver_proto::StreamEvent {
                data: bytes.to_vec(),
            }),
            CommandEvent::Exit(GatherOutputStatus::Finished {
                exit_code,
                execution_stats,
            }) => Data::Exit(buck2_forkserver_proto::ExitEvent {
                exit_code,
                execution_stats,
            }),
            CommandEvent::Exit(GatherOutputStatus::TimedOut(duration)) => {
                Data::Timeout(buck2_forkserver_proto::TimeoutEvent {
                    duration: duration.try_into().ok(),
                })
            }
            CommandEvent::Exit(GatherOutputStatus::Cancelled) => {
                Data::Cancel(buck2_forkserver_proto::CancelEvent {})
            }
            CommandEvent::Exit(GatherOutputStatus::SpawnFailed(reason)) => {
                Data::SpawnFailed(buck2_forkserver_proto::SpawnFailedEvent { reason })
            }
        };

        buck2_forkserver_proto::CommandEvent { data: Some(data) }
    }

    fn convert_err(e: buck2_error::Error) -> tonic::Status {
        tonic::Status::unknown(format!("{:#}", e))
    }

    s.map(|r| r.map(convert_event).map_err(convert_err))
}

pub(crate) fn decode_event_stream<S>(s: S) -> impl Stream<Item = buck2_error::Result<CommandEvent>>
where
    S: Stream<Item = Result<buck2_forkserver_proto::CommandEvent, tonic::Status>>,
{
    fn convert_event(e: buck2_forkserver_proto::CommandEvent) -> buck2_error::Result<CommandEvent> {
        use buck2_forkserver_proto::command_event::Data;

        let event = match e.data.buck_error_context("Missing `data`")? {
            Data::Stdout(buck2_forkserver_proto::StreamEvent { data }) => {
                CommandEvent::Stdout(data.into())
            }
            Data::Stderr(buck2_forkserver_proto::StreamEvent { data }) => {
                CommandEvent::Stderr(data.into())
            }
            Data::Exit(buck2_forkserver_proto::ExitEvent {
                exit_code,
                execution_stats,
            }) => CommandEvent::Exit(GatherOutputStatus::Finished {
                exit_code,
                execution_stats,
            }),
            Data::Timeout(buck2_forkserver_proto::TimeoutEvent { duration }) => {
                CommandEvent::Exit(GatherOutputStatus::TimedOut(
                    duration
                        .buck_error_context("Missing `duration`")?
                        .try_into_duration()
                        .buck_error_context("Invalid `duration`")?,
                ))
            }
            Data::Cancel(buck2_forkserver_proto::CancelEvent {}) => {
                CommandEvent::Exit(GatherOutputStatus::Cancelled)
            }
            Data::SpawnFailed(buck2_forkserver_proto::SpawnFailedEvent { reason }) => {
                CommandEvent::Exit(GatherOutputStatus::SpawnFailed(reason))
            }
        };

        Ok(event)
    }

    fn convert_err(e: tonic::Status) -> buck2_error::Error {
        buck2_error::buck2_error!(
            buck2_error::ErrorTag::Tier0,
            "forkserver error: {}",
            e.message()
        )
    }

    s.map(|r| r.map_err(convert_err).and_then(convert_event))
}
