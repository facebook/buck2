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
use buck2_core::configuration::Configuration;
use buck2_core::target::ConfiguredTargetLabel;
use buck2_core::target::TargetLabel;
use buck2_data::ToProtoMessage;
use buck2_execute::bxl::types::BxlFunctionLabel;
use buck2_execute::bxl::types::BxlKey;
