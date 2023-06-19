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

use buck2_data::StarlarkUserEvent;
use buck2_data::StarlarkUserMetadataValue;
use starlark::values::dict::DictRef;
use starlark::values::UnpackValue;
use starlark::values::Value;
use thiserror::Error;

#[derive(Error, Debug)]
enum StarlarkUserEventUnpack {
    #[error(
        "Metadata should be a dict where keys are strings, and values are strings, ints, or bools. Got type: `{0}`"
    )]
    InvalidMetadata(String),
    #[error("Metadata keys should be strings. Got type: `{0}`")]
    InvalidKey(String),
    #[error("Metadata values should be strings, ints, or bools. Key `{0}` had value type `{1}`")]
    InvalidValue(String, String),
}

pub(crate) fn to_starlark_user_event<'v>(
    id: &str,
    metadata: Value<'v>,
) -> anyhow::Result<StarlarkUserEvent> {
    let metadata_value = match DictRef::from_value(metadata) {
        Some(metadata_value) => metadata_value,
        None => {
            return Err(
                StarlarkUserEventUnpack::InvalidMetadata(metadata.get_type().to_owned()).into(),
            );
        }
    };

    let metadata_map = metadata_value
        .iter()
        .map(|(k, v)| {
            let k = match k.unpack_str() {
                Some(k) => k.to_owned(),
                None => {
                    return Err(StarlarkUserEventUnpack::InvalidKey(k.get_type().to_owned()).into());
                }
            };

            if let Some(v) = v.unpack_str() {
                Ok((
                    k,
                    StarlarkUserMetadataValue {
                        value: Some(
                            buck2_data::starlark_user_metadata_value::Value::StringValue(v.into()),
                        ),
                    },
                ))
            } else if let Some(v) = v.unpack_bool() {
                Ok((
                    k,
                    StarlarkUserMetadataValue {
                        value: Some(buck2_data::starlark_user_metadata_value::Value::BoolValue(
                            v,
                        )),
                    },
                ))
            } else if let Some(v) = v.unpack_i32() {
                Ok((
                    k,
                    StarlarkUserMetadataValue {
                        value: Some(buck2_data::starlark_user_metadata_value::Value::IntValue(v)),
                    },
                ))
            // Let's also accept floats since `instant()` methods return floats, but cast them to ints
            } else if let Some(v) = f64::unpack_value(v) {
                Ok((
                    k,
                    StarlarkUserMetadataValue {
                        value: Some(buck2_data::starlark_user_metadata_value::Value::IntValue(
                            v as i32,
                        )),
                    },
                ))
            } else {
                return Err(
                    StarlarkUserEventUnpack::InvalidValue(k, v.get_type().to_owned()).into(),
                );
            }
        })
        .collect::<anyhow::Result<HashMap<_, _>>>()?;

    Ok(StarlarkUserEvent {
        id: id.to_owned(),
        metadata: metadata_map,
    })
}
