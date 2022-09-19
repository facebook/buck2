use std::process::ExitStatus;

use anyhow::Context as _;
use buck2_common::convert::ProstDurationExt;
use futures::stream::Stream;
use futures::stream::StreamExt;

use crate::run::CommandEvent;
use crate::run::GatherOutputStatus;

pub fn encode_event_stream<S>(
    s: S,
) -> impl Stream<Item = Result<forkserver_proto::CommandEvent, tonic::Status>>
where
    S: Stream<Item = anyhow::Result<CommandEvent>>,
{
    fn convert_event(e: CommandEvent) -> forkserver_proto::CommandEvent {
        use forkserver_proto::command_event::Data;

        let data = match e {
            CommandEvent::Stdout(bytes) => Data::Stdout(forkserver_proto::StreamEvent {
                data: bytes.to_vec(),
            }),
            CommandEvent::Stderr(bytes) => Data::Stderr(forkserver_proto::StreamEvent {
                data: bytes.to_vec(),
            }),
            CommandEvent::Exit(GatherOutputStatus::Finished(status)) => {
                let exit_code;

                #[cfg(unix)]
                {
                    use std::os::unix::process::ExitStatusExt;
                    exit_code = status.into_raw();
                }

                #[cfg(not(unix))]
                {
                    // Windows will always set an exit code.
                    exit_code = status.code().unwrap_or(1);
                }

                Data::Exit(forkserver_proto::ExitEvent { exit_code })
            }
            CommandEvent::Exit(GatherOutputStatus::TimedOut(duration)) => {
                Data::Timeout(forkserver_proto::TimeoutEvent {
                    duration: Some(duration.into()),
                })
            }
            CommandEvent::Exit(GatherOutputStatus::Cancelled) => {
                Data::Cancel(forkserver_proto::CancelEvent {})
            }
        };

        forkserver_proto::CommandEvent { data: Some(data) }
    }

    fn convert_err(e: anyhow::Error) -> tonic::Status {
        tonic::Status::unknown(format!("{:#}", e))
    }

    s.map(|r| r.map(convert_event).map_err(convert_err))
}

pub fn decode_event_stream<S>(s: S) -> impl Stream<Item = anyhow::Result<CommandEvent>>
where
    S: Stream<Item = Result<forkserver_proto::CommandEvent, tonic::Status>>,
{
    fn convert_event(e: forkserver_proto::CommandEvent) -> anyhow::Result<CommandEvent> {
        use forkserver_proto::command_event::Data;

        let event = match e.data.context("Missing `data`")? {
            Data::Stdout(forkserver_proto::StreamEvent { data }) => {
                CommandEvent::Stdout(data.into())
            }
            Data::Stderr(forkserver_proto::StreamEvent { data }) => {
                CommandEvent::Stderr(data.into())
            }
            Data::Exit(forkserver_proto::ExitEvent { exit_code }) => {
                let exit_status;

                #[cfg(unix)]
                {
                    use std::os::unix::process::ExitStatusExt;
                    exit_status = ExitStatus::from_raw(exit_code)
                }

                #[cfg(not(unix))]
                {
                    use std::os::windows::process::ExitStatusExt;
                    exit_status = ExitStatus::from_raw(exit_code as _)
                }

                CommandEvent::Exit(GatherOutputStatus::Finished(exit_status))
            }
            Data::Timeout(forkserver_proto::TimeoutEvent { duration }) => {
                CommandEvent::Exit(GatherOutputStatus::TimedOut(
                    duration
                        .context("Missing `duration`")?
                        .try_into_duration()
                        .context("Invalid `duration`")?,
                ))
            }
            Data::Cancel(forkserver_proto::CancelEvent {}) => {
                CommandEvent::Exit(GatherOutputStatus::Cancelled)
            }
        };

        Ok(event)
    }

    fn convert_err(e: tonic::Status) -> anyhow::Error {
        anyhow::anyhow!("forkserver error: {}", e.message())
    }

    s.map(|r| r.map_err(convert_err).and_then(convert_event))
}
