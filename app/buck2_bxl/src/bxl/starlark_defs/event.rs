/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Parse some inputs to a `[`StarlarkUserEvent`].
//!

use std::collections::HashMap;

use buck2_data::starlark_user_metadata_value::Value::BoolValue;
use buck2_data::starlark_user_metadata_value::Value::DictValue;
use buck2_data::starlark_user_metadata_value::Value::IntValue;
use buck2_data::starlark_user_metadata_value::Value::ListValue;
use buck2_data::starlark_user_metadata_value::Value::StringValue;
use buck2_data::StarlarkUserEvent;
use buck2_data::StarlarkUserMetadataDictValue;
use buck2_data::StarlarkUserMetadataListValue;
use buck2_data::StarlarkUserMetadataValue;
use starlark::values::dict::DictRef;
use starlark::values::list::ListRef;
use starlark::values::UnpackValue;
use starlark::values::Value;
use thiserror::Error;

#[derive(Error, Debug)]
enum StarlarkUserEventUnpack {
    #[error(
        "Metadata should be a dict where keys are strings, and values are strings, ints, bools, or dicts/lists of the mentioned types. Got type: `{0}`"
    )]
    InvalidMetadata(String),
    #[error("Metadata keys should be strings. Got type: `{0}`")]
    InvalidKey(String),
    #[error(
        "Metadata values should be strings, ints, bools, or dicts/lists of the mentioned types. Key `{0}` had value type `{1}`"
    )]
    InvalidValue(String, String),
}

pub(crate) fn to_starlark_user_event<'v>(
    id: &str,
    metadata: Value<'v>,
) -> anyhow::Result<StarlarkUserEvent> {
    Ok(StarlarkUserEvent {
        id: id.to_owned(),
        metadata: unpack_metadata_map(metadata)?,
    })
}

fn unpack_metadata_map<'v>(
    metadata: Value<'v>,
) -> anyhow::Result<HashMap<String, StarlarkUserMetadataValue>> {
    let metadata = match DictRef::from_value(metadata) {
        Some(metadata) => metadata,
        None => {
            return Err(
                StarlarkUserEventUnpack::InvalidMetadata(metadata.get_type().to_owned()).into(),
            );
        }
    };

    metadata
        .iter()
        .map(|(k, v)| {
            let k = match k.unpack_str() {
                Some(k) => k.to_owned(),
                None => {
                    return Err(StarlarkUserEventUnpack::InvalidKey(k.get_type().to_owned()).into());
                }
            };

            let v = get_metadata_value(&k, v)?;
            Ok((k, v))
        })
        .collect::<anyhow::Result<HashMap<_, _>>>()
}

fn get_metadata_value<'v>(k: &str, v: Value<'v>) -> anyhow::Result<StarlarkUserMetadataValue> {
    if let Some(v) = v.unpack_str() {
        Ok(StarlarkUserMetadataValue {
            value: Some(StringValue(v.into())),
        })
    } else if let Some(v) = v.unpack_bool() {
        Ok(StarlarkUserMetadataValue {
            value: Some(BoolValue(v)),
        })
    } else if let Some(v) = v.unpack_i32() {
        Ok(StarlarkUserMetadataValue {
            value: Some(IntValue(v)),
        })
    // Let's also accept floats since `instant()` methods return floats, but cast them to ints
    } else if let Some(v) = f64::unpack_value(v) {
        Ok(StarlarkUserMetadataValue {
            value: Some(IntValue(v as i32)),
        })
    } else if DictRef::from_value(v).is_some() {
        let dict = unpack_metadata_map(v)?;
        let dict = StarlarkUserMetadataDictValue { value: dict };
        Ok(StarlarkUserMetadataValue {
            value: Some(DictValue(dict)),
        })
    } else if let Some(v) = ListRef::from_value(v) {
        let list = v
            .iter()
            .map(|e| get_metadata_value(k, e))
            .collect::<anyhow::Result<Vec<_>>>()?;
        let list = StarlarkUserMetadataListValue { value: list };
        Ok(StarlarkUserMetadataValue {
            value: Some(ListValue(list)),
        })
    } else {
        Err(StarlarkUserEventUnpack::InvalidValue(k.to_owned(), v.get_type().to_owned()).into())
    }
}
