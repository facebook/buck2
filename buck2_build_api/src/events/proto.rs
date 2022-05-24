/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Utilities and traits for converting certain key Buck2 to objects to protobuf messages, suitable for transmission
//! in events.
use buck2_bxl_core::{BxlFunctionLabel, BxlKey};
use buck2_core::{
    configuration::Configuration,
    target::{ConfiguredTargetLabel, TargetLabel},
};

/// Trait for things that can be converted into protobuf messages, for ease of emitting events. There are many core Buck
/// types that are represented in the Daemon API that use this trait to ease conversion.
pub(crate) trait ToProtoMessage {
    type Message: prost::Message;

    fn as_proto(&self) -> Self::Message;
}

impl ToProtoMessage for TargetLabel {
    type Message = buck2_data::TargetLabel;

    fn as_proto(&self) -> Self::Message {
        buck2_data::TargetLabel {
            package: self.pkg().to_string(),
            name: self.name().to_string(),
        }
    }
}

impl ToProtoMessage for ConfiguredTargetLabel {
    type Message = buck2_data::ConfiguredTargetLabel;

    fn as_proto(&self) -> Self::Message {
        buck2_data::ConfiguredTargetLabel {
            label: Some(self.unconfigured().as_proto()),
            configuration: Some(self.cfg().as_proto()),
        }
    }
}

impl ToProtoMessage for Configuration {
    type Message = buck2_data::Configuration;

    fn as_proto(&self) -> Self::Message {
        buck2_data::Configuration {
            full_name: self.full_name().to_owned(),
        }
    }
}

impl ToProtoMessage for BxlKey {
    type Message = buck2_data::BxlFunctionKey;

    fn as_proto(&self) -> Self::Message {
        buck2_data::BxlFunctionKey {
            label: Some(self.label().as_proto()),
            args: self
                .cli_args()
                .iter()
                .map(|(k, v)| format!("--{} {}", k, v))
                .collect(),
        }
    }
}

impl ToProtoMessage for BxlFunctionLabel {
    type Message = buck2_data::BxlFunctionLabel;

    fn as_proto(&self) -> Self::Message {
        buck2_data::BxlFunctionLabel {
            bxl_path: self.bxl_path.to_string(),
            name: self.name.clone(),
        }
    }
}
