/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Display;
use std::sync::Arc;

use buck2_common::dice::cycles::CycleAdapterDescriptor;
use buck2_interpreter::path::OwnedStarlarkModulePath;
use buck2_util::cycle_detector::CycleDescriptor;
use derive_more::Display;
use gazebo::prelude::VecExt;
use thiserror::Error;

use crate::interpreter::dice_calculation_delegate::testing::EvalImportKey;

#[derive(Debug)]
pub struct LoadCycleDescriptor;

#[derive(Debug, Clone, Eq, PartialEq, Hash, Display)]
pub enum LoadCycleKey {
    #[display(fmt = "{}", _0)]
    Module(OwnedStarlarkModulePath),
}

#[derive(Debug, Error, Clone)]
pub struct LoadCycleError {
    cycle: Arc<Vec<OwnedStarlarkModulePath>>,
}

impl Display for LoadCycleError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Load cycle detected (`->` means \"loads\"):")?;
        for p in self.cycle.iter() {
            writeln!(f, "  {} ->", p)?;
        }
        // point back at the first item in the cycle.
        writeln!(f, "  {}", self.cycle.first().unwrap())?;
        Ok(())
    }
}

impl CycleDescriptor for LoadCycleDescriptor {
    type Key = LoadCycleKey;

    type Error = LoadCycleError;

    fn cycle_error(cycle: Vec<&Self::Key>) -> Self::Error {
        LoadCycleError {
            cycle: Arc::new(cycle.into_map(|v| match v {
                LoadCycleKey::Module(p) => p.clone(),
            })),
        }
    }
}

impl CycleAdapterDescriptor for LoadCycleDescriptor {
    fn to_key(key: &dyn std::any::Any) -> Option<Self::Key> {
        key.downcast_ref::<EvalImportKey>()
            .map(|v| LoadCycleKey::Module(v.0.clone()))
    }
}
