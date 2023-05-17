/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

/// This should have been a message in proto, but that is backwards incompatible.
pub enum BaseDeferredKeyProto {
    TargetLabel(crate::ConfiguredTargetLabel),
    BxlKey(crate::BxlFunctionKey),
    AnonTarget(crate::AnonTarget),
}

impl From<BaseDeferredKeyProto> for crate::critical_path_entry2::action_execution::Owner {
    fn from(value: BaseDeferredKeyProto) -> Self {
        match value {
            BaseDeferredKeyProto::TargetLabel(t) => {
                crate::critical_path_entry2::action_execution::Owner::TargetLabel(t)
            }
            BaseDeferredKeyProto::BxlKey(b) => {
                crate::critical_path_entry2::action_execution::Owner::BxlKey(b)
            }
            BaseDeferredKeyProto::AnonTarget(a) => {
                crate::critical_path_entry2::action_execution::Owner::AnonTarget(a)
            }
        }
    }
}

impl From<BaseDeferredKeyProto> for crate::critical_path_entry2::materialization::Owner {
    fn from(value: BaseDeferredKeyProto) -> Self {
        match value {
            BaseDeferredKeyProto::TargetLabel(t) => {
                crate::critical_path_entry2::materialization::Owner::TargetLabel(t)
            }
            BaseDeferredKeyProto::BxlKey(b) => {
                crate::critical_path_entry2::materialization::Owner::BxlKey(b)
            }
            BaseDeferredKeyProto::AnonTarget(a) => {
                crate::critical_path_entry2::materialization::Owner::AnonTarget(a)
            }
        }
    }
}

impl From<BaseDeferredKeyProto> for crate::dynamic_lambda_start::Owner {
    fn from(value: BaseDeferredKeyProto) -> Self {
        match value {
            BaseDeferredKeyProto::TargetLabel(t) => {
                crate::dynamic_lambda_start::Owner::TargetLabel(t)
            }
            BaseDeferredKeyProto::BxlKey(b) => crate::dynamic_lambda_start::Owner::BxlKey(b),
            BaseDeferredKeyProto::AnonTarget(a) => {
                crate::dynamic_lambda_start::Owner::AnonTarget(a)
            }
        }
    }
}

impl From<BaseDeferredKeyProto> for crate::action_key::Owner {
    fn from(value: BaseDeferredKeyProto) -> Self {
        match value {
            BaseDeferredKeyProto::TargetLabel(t) => crate::action_key::Owner::TargetLabel(t),
            BaseDeferredKeyProto::BxlKey(b) => crate::action_key::Owner::BxlKey(b),
            BaseDeferredKeyProto::AnonTarget(a) => crate::action_key::Owner::AnonTarget(a),
        }
    }
}
