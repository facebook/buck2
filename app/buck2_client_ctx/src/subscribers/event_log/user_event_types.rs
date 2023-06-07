/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_data::StarlarkUserEvent;

use super::write::StreamValueForWrite;

pub(crate) fn is_user_event<'v>(buck_event: &'v buck2_data::BuckEvent) -> bool {
    match buck_event.data.as_ref().unwrap() {
        buck2_data::buck_event::Data::Instant(ref instant) => {
            match instant.data.as_ref().unwrap() {
                buck2_data::instant_event::Data::StarlarkUserEvent(_) => true,
                _ => false,
            }
        }
        // TODO(wendyy) - support actions
        _ => false,
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
