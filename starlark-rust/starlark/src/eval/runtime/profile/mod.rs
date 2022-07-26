/*
 * Copyright 2019 The Starlark in Rust Authors.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

use std::fmt::Display;
use std::str::FromStr;

use gazebo::dupe::Dupe;

pub(crate) mod bc;
pub(crate) mod csv;
pub(crate) mod flame;
pub(crate) mod flamegraph;
pub(crate) mod heap;
pub(crate) mod stmt;
pub(crate) mod typecheck;

/// How to profile starlark code.
#[derive(Debug, PartialEq, Eq, Hash, Clone, Dupe)]
pub enum ProfileMode {
    /// The heap profile mode provides information about the time spent in each function and allocations
    /// performed by each function. Enabling this mode the side effect of disabling garbage-collection.
    /// This profiling mode is the recommended one.
    HeapSummary,
    /// Like heap summary, but information about retained memory after module is frozen.
    HeapSummaryRetained,
    /// Like heap profile, but writes output comparible with
    /// [flamegraph.pl](https://github.com/brendangregg/FlameGraph/blob/master/flamegraph.pl).
    HeapFlame,
    /// Like heap flame, but information about retained memory after module is frozen.
    HeapFlameRetained,
    /// The statement profile mode provides information about time spent in each statement.
    Statement,
    /// The bytecode profile mode provides information about bytecode instructions.
    Bytecode,
    /// The bytecode profile mode provides information about bytecode instruction pairs.
    BytecodePairs,
    /// Provide output compatible with
    /// [flamegraph.pl](https://github.com/brendangregg/FlameGraph/blob/master/flamegraph.pl).
    TimeFlame,
    /// Profile runtime typechecking.
    Typecheck,
}

impl Display for ProfileMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.name())
    }
}

impl ProfileMode {
    fn name(&self) -> &str {
        match self {
            ProfileMode::HeapSummary => "heap-summary",
            ProfileMode::HeapSummaryRetained => "heap-summary-retained",
            ProfileMode::HeapFlame => "heap-flame",
            ProfileMode::HeapFlameRetained => "heap-flame-retained",
            ProfileMode::Statement => "statement",
            ProfileMode::Bytecode => "bytecode",
            ProfileMode::BytecodePairs => "bytecode-pairs",
            ProfileMode::TimeFlame => "time-flame",
            ProfileMode::Typecheck => "typecheck",
        }
    }
}

impl FromStr for ProfileMode {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        for mode in [
            ProfileMode::HeapSummary,
            ProfileMode::HeapFlame,
            ProfileMode::Statement,
            ProfileMode::Bytecode,
            ProfileMode::BytecodePairs,
            ProfileMode::TimeFlame,
            ProfileMode::Typecheck,
        ] {
            if s == mode.name() {
                return Ok(mode);
            }
        }
        Err(anyhow::anyhow!("Invalid ProfileMode: `{}`", s))
    }
}
