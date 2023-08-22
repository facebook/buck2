/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::time::Duration;
use std::time::SystemTime;

use async_trait::async_trait;
use buck2_cli_proto::command_result;
use buck2_events::sink::scribe::new_thrift_scribe_sink_if_enabled;
use buck2_wrapper_common::invocation_id::TraceId;
use dupe::Dupe;
use fbinit::FacebookInit;

use crate::subscribers::subscriber::EventSubscriber;

pub struct BuildGraphStats {
    fb: FacebookInit,
    trace_id: TraceId,
}

impl BuildGraphStats {
    pub fn new(fb: FacebookInit, trace_id: TraceId) -> Self {
        Self { fb, trace_id }
    }

    async fn handle_build_response(
        &self,
        res: &buck2_cli_proto::BuildResponse,
    ) -> anyhow::Result<()> {
        let event = self.build_graph_stats_from_build_response(res);
        self.send_event(event).await;

        Ok(())
    }

    fn build_graph_stats_from_build_response(
        &self,
        res: &buck2_cli_proto::BuildResponse,
    ) -> buck2_events::BuckEvent {
        let build_targets = res
            .build_targets
            .iter()
            .map(|t| buck2_data::BuildTarget {
                target: t.target.clone(),
                configuration: t.configuration.clone(),
                configured_graph_size: t.configured_graph_size,
            })
            .collect();
        let stats = buck2_data::BuildGraphStats { build_targets };
        buck2_events::BuckEvent::new(
            SystemTime::now(),
            self.trace_id.dupe(),
            None,
            None,
            buck2_data::RecordEvent {
                data: Some(stats.into()),
            }
            .into(),
        )
    }

    async fn send_event(&self, event: buck2_events::BuckEvent) {
        if let Ok(Some(sink)) =
            new_thrift_scribe_sink_if_enabled(self.fb, 1, Duration::from_millis(100), 2, None)
        {
            tracing::info!("Sending an event to Scribe: {:?}", &event);
            sink.send_now(event).await;
        } else {
            tracing::info!("An event was not sent to Scribe: {:?}", &event);
        }
    }
}

#[async_trait]
impl EventSubscriber for BuildGraphStats {
    async fn handle_command_result(
        &mut self,
        result: &buck2_cli_proto::CommandResult,
    ) -> anyhow::Result<()> {
        match &result.result {
            Some(command_result::Result::BuildResponse(res)) => {
                self.handle_build_response(res).await
            }
            _ => Ok(()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[fbinit::test]
    fn build_graph_stats_normal(fb: FacebookInit) {
        let res = buck2_cli_proto::BuildResponse {
            build_targets: vec![
                buck2_cli_proto::BuildTarget {
                    target: "//some/target:A".to_owned(),
                    configuration: "//some/conf:A".to_owned(),
                    configured_graph_size: Some(123),
                    ..Default::default()
                },
                buck2_cli_proto::BuildTarget {
                    target: "//some/target:B".to_owned(),
                    configuration: "//some/conf:B".to_owned(),
                    configured_graph_size: None,
                    ..Default::default()
                },
            ],
            ..Default::default()
        };

        let uuid = TraceId::new();
        let handler = BuildGraphStats::new(fb, uuid.dupe());
        let event = handler.build_graph_stats_from_build_response(&res);

        let event_expected = buck2_data::BuckEvent {
            data: Some(buck2_data::buck_event::Data::Record(
                buck2_data::RecordEvent {
                    data: Some(buck2_data::record_event::Data::BuildGraphStats(
                        buck2_data::BuildGraphStats {
                            build_targets: vec![
                                buck2_data::BuildTarget {
                                    target: "//some/target:A".to_owned(),
                                    configuration: "//some/conf:A".to_owned(),
                                    configured_graph_size: Some(123),
                                },
                                buck2_data::BuildTarget {
                                    target: "//some/target:B".to_owned(),
                                    configuration: "//some/conf:B".to_owned(),
                                    configured_graph_size: None,
                                },
                            ],
                        },
                    )),
                },
            )),
            ..Default::default()
        };

        assert_eq!(event.trace_id().unwrap(), uuid);
        assert_eq!(event.data(), &event_expected.data.unwrap());
    }

    #[fbinit::test]
    fn build_graph_stats_empty_target(fb: FacebookInit) {
        let res = buck2_cli_proto::BuildResponse {
            build_targets: vec![],
            ..Default::default()
        };

        let uuid = TraceId::new();
        let handler = BuildGraphStats::new(fb, uuid.dupe());
        let event = handler.build_graph_stats_from_build_response(&res);

        let event_expected = buck2_data::BuckEvent {
            data: Some(buck2_data::buck_event::Data::Record(
                buck2_data::RecordEvent {
                    data: Some(buck2_data::record_event::Data::BuildGraphStats(
                        buck2_data::BuildGraphStats {
                            build_targets: vec![],
                        },
                    )),
                },
            )),
            ..Default::default()
        };

        assert_eq!(event.trace_id().unwrap(), uuid);
        assert_eq!(event.data(), &event_expected.data.unwrap());
    }
}
