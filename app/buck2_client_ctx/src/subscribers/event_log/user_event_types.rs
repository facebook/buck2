/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use anyhow::Context;
use buck2_data::ActionExecutionEnd;
use buck2_data::ActionExecutionStart;
use buck2_data::BuckEvent;
use buck2_data::StarlarkUserEvent;
use thiserror::Error;

use super::write::StreamValueForWrite;

#[derive(Debug, thiserror::Error)]
pub(crate) enum SerializeUserEventError {
    #[error("Internal error: Missing `data` in `{0}`")]
    MissingData(String),
}

pub(crate) fn is_user_event(buck_event: &BuckEvent) -> anyhow::Result<bool> {
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
                buck2_data::instant_event::Data::StarlarkUserEvent(_) => Ok(true),
                _ => Ok(false),
            }
        }
        buck2_data::buck_event::Data::SpanStart(ref span_start_event) => {
            match span_start_event
                .data
                .as_ref()
                .context(SerializeUserEventError::MissingData(
                    "SpanStartEvent".to_owned(),
                ))? {
                buck2_data::span_start_event::Data::ActionExecution(_) => Ok(true),
                _ => Ok(false),
            }
        }
        buck2_data::buck_event::Data::SpanEnd(ref span_end_event) => {
            match span_end_event
                .data
                .as_ref()
                .context(SerializeUserEventError::MissingData(
                    "SpanEndEvent".to_owned(),
                ))? {
                buck2_data::span_end_event::Data::ActionExecution(_) => Ok(true),
                _ => Ok(false),
            }
        }
        _ => Ok(false),
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
