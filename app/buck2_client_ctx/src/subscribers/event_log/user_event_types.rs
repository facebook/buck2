/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use anyhow::Context;
use buck2_common::convert::ProstDurationExt;
use buck2_data::ActionKind;
use buck2_data::ActionName;
use buck2_data::BuckEvent;
use buck2_data::StarlarkUserEvent;
use serde::Deserialize;
use serde::Serialize;

use crate::stream_value::StreamValue;

#[derive(Debug, thiserror::Error)]
pub(crate) enum SerializeUserEventError {
    #[error("Internal error: Missing `data` in `{0}`")]
    MissingData(String),
    #[error("Internal error: Missing `timestamp` in `BuckEvent`")]
    MissingTimestamp,
    #[error(
        "Internal error: Missing `input_materialization_duration` in `CommandExecutionDetails`"
    )]
    MissingInputMaterializationDuration,
    #[error("Internal error: Missing `name` in `ActionExecutionEnd`")]
    MissingName,
}

/// Wrapper around StarlarkUserEvent so we discard the rest of BuckEvent fields.
#[derive(Serialize, Deserialize, Allocative, Clone)]
pub struct UserEvent {
    #[serde(flatten)]
    data: UserEventData,
    epoch_millis: u64,
}

/// BuckEvent types that are allowed in the user event log.
#[derive(Serialize, Deserialize, Allocative, Clone)]
pub enum UserEventData {
    StarlarkUserEvent(StarlarkUserEvent),
    ActionExecutionEvent(ActionExecutionEndSimple),
    BxlEnsureArtifactsEvent(BxlEnsureArtifactsEvent),
}

/// Simplified version of ActionExecutionEnd.
#[derive(Serialize, Deserialize, Allocative, Clone)]
pub struct ActionExecutionEndSimple {
    kind: ActionKind,
    name: ActionName,
    duration_millis: u64,
    output_size: u64,
    input_materialization_duration_millis: u64,
}

#[derive(Serialize, Deserialize, Allocative, Clone)]
pub struct BxlEnsureArtifactsEvent {
    duration_millis: u64,
}

pub fn try_get_user_event_for_read(
    stream_value: &StreamValue,
) -> anyhow::Result<Option<UserEvent>> {
    match stream_value {
        StreamValue::Event(buck_event) => try_get_user_event(buck_event.as_ref()),
        _ => Ok(None),
    }
}

pub(crate) fn try_get_user_event(buck_event: &BuckEvent) -> anyhow::Result<Option<UserEvent>> {
    let timestamp = buck_event
        .timestamp
        .as_ref()
        .context(SerializeUserEventError::MissingTimestamp)?;
    let epoch_millis = timestamp.seconds as u64 * 1000 + timestamp.nanos as u64 / 1_000_000;

    match buck_event
        .data
        .as_ref()
        .context(SerializeUserEventError::MissingData("BuckEvent".to_owned()))?
    {
        buck2_data::buck_event::Data::Instant(ref instant) => {
            match instant
                .data
                .as_ref()
                .context(SerializeUserEventError::MissingData(
                    "InstantEvent".to_owned(),
                ))? {
                buck2_data::instant_event::Data::StarlarkUserEvent(event) => Ok(Some(UserEvent {
                    data: UserEventData::StarlarkUserEvent(event.clone()),
                    epoch_millis,
                })),
                _ => Ok(None),
            }
        }
        buck2_data::buck_event::Data::SpanEnd(ref span_end_event) => {
            let duration_millis = span_end_event
                .duration
                .as_ref()
                .context(SerializeUserEventError::MissingTimestamp)?
                .try_into_duration()?
                .as_millis() as u64;

            match span_end_event
                .data
                .as_ref()
                .context(SerializeUserEventError::MissingData(
                    "SpanEndEvent".to_owned(),
                ))? {
                buck2_data::span_end_event::Data::ActionExecution(action_execution) => {
                    let mut input_materialization_duration_millis = 0;

                    // Take the last command report's input materialization duration
                    if let Some(command) = action_execution.commands.last() {
                        if let Some(details) = &command.details {
                            input_materialization_duration_millis = details
                                .input_materialization_duration
                                .as_ref()
                                .context(
                                    SerializeUserEventError::MissingInputMaterializationDuration,
                                )?
                                .try_into_duration()?
                                .as_millis()
                                as u64;
                        }
                    }

                    let action_event = ActionExecutionEndSimple {
                        kind: action_execution.kind(),
                        name: action_execution
                            .name
                            .as_ref()
                            .context(SerializeUserEventError::MissingName)?
                            .clone(),
                        duration_millis,
                        output_size: action_execution.output_size,
                        input_materialization_duration_millis,
                    };

                    Ok(Some(UserEvent {
                        data: UserEventData::ActionExecutionEvent(action_event),
                        epoch_millis,
                    }))
                }
                buck2_data::span_end_event::Data::BxlEnsureArtifacts(_) => {
                    let bxl_ensure_artifacts = BxlEnsureArtifactsEvent { duration_millis };

                    Ok(Some(UserEvent {
                        data: UserEventData::BxlEnsureArtifactsEvent(bxl_ensure_artifacts),
                        epoch_millis,
                    }))
                }
                _ => Ok(None),
            }
        }
        _ => Ok(None),
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use buck2_data::starlark_user_metadata_value::Value;
    use buck2_data::StarlarkUserEvent;
    use buck2_data::StarlarkUserMetadataValue;

    #[test]
    fn test_serialize_starlark_user_event() {
        let val1 = StarlarkUserMetadataValue {
            value: Some(Value::BoolValue(true)),
        };

        let mut metadata = HashMap::new();
        metadata.insert("bool_value".to_owned(), val1);

        let starlark_user_event = StarlarkUserEvent {
            id: "foo".to_owned(),
            metadata,
        };

        let serialized = serde_json::to_string_pretty(&starlark_user_event).unwrap();
        let expected = r#"{
  "id": "foo",
  "metadata": {
    "bool_value": true
  }
}"#;
        assert_eq!(expected, serialized);
    }
}
