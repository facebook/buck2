/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use buck2_common::dice::cycles::CycleAdapterDescriptor;
use buck2_util::cycle_detector::CycleDescriptor;
use derive_more::Display;
use dice::DynKey;
use dupe::Dupe;
use gazebo::prelude::*;

use crate::execution::ExecutionPlatformResolutionKey;
use crate::execution::ToolchainExecutionPlatformCompatibilityKey;
use crate::nodes::ConfiguredTargetNodeKey;

#[derive(Debug, buck2_error::Error, Clone, Dupe)]
#[buck2(input)]
#[error("{}", display_configured_graph_cycle_error(&.cycle[..]))]
pub struct ConfiguredGraphCycleError {
    cycle: Arc<Vec<ConfiguredGraphCycleKeys>>,
}

fn display_configured_graph_cycle_error(cycle: &[ConfiguredGraphCycleKeys]) -> String {
    use std::fmt::Write;

    let mut s = String::new();
    writeln!(
        s,
        "Configured target cycle detected (`->` means \"depends on\"):"
    )
    .unwrap();
    for p in cycle.iter() {
        writeln!(s, "  {} ->", p).unwrap();
    }
    // point back at the first item in the cycle.
    writeln!(s, "  {}", cycle.first().unwrap()).unwrap();
    s
}

// TODO(cjhopman): There's other keys that could be involved in a cycle in the configured graph and they should probably also be tracked
// here. Would be good to check on things like transitions, toolchains, configuration nodes. Still, this will currently catch most
// configured graph cycles.
#[derive(Debug, Display, Clone, Eq, PartialEq, Hash)]
#[allow(private_interfaces)]
pub enum ConfiguredGraphCycleKeys {
    #[display("{}", _0)]
    ConfiguredTargetNode(ConfiguredTargetNodeKey),
    #[display("{}", _0)]
    ToolchainExecutionPlatformCompatibility(ToolchainExecutionPlatformCompatibilityKey),
    #[display("{}", _0)]
    ExecutionPlatformResolution(ExecutionPlatformResolutionKey),
}

#[derive(Debug)]
pub struct ConfiguredGraphCycleDescriptor;

impl CycleDescriptor for ConfiguredGraphCycleDescriptor {
    type Key = ConfiguredGraphCycleKeys;

    type Error = ConfiguredGraphCycleError;

    fn cycle_error(cycle: Vec<&Self::Key>) -> Self::Error {
        ConfiguredGraphCycleError {
            cycle: Arc::new(cycle.cloned()),
        }
    }
}

impl CycleAdapterDescriptor for ConfiguredGraphCycleDescriptor {
    fn to_key(key: &DynKey) -> Option<Self::Key> {
        if let Some(v) = key.downcast_ref::<ConfiguredTargetNodeKey>() {
            return Some(ConfiguredGraphCycleKeys::ConfiguredTargetNode(v.dupe()));
        }
        if let Some(v) = key.downcast_ref::<ExecutionPlatformResolutionKey>() {
            return Some(ConfiguredGraphCycleKeys::ExecutionPlatformResolution(
                v.dupe(),
            ));
        }
        if let Some(v) = key.downcast_ref::<ToolchainExecutionPlatformCompatibilityKey>() {
            return Some(
                ConfiguredGraphCycleKeys::ToolchainExecutionPlatformCompatibility(v.dupe()),
            );
        }

        None
    }
}
