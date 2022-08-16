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

use std::fs;
use std::path::Path;

use anyhow::Context;

use crate::eval::runtime::profile::bc::BcPairsProfileData;
use crate::eval::runtime::profile::bc::BcProfileData;
use crate::eval::ProfileMode;
use crate::values::AggregateHeapProfileInfo;

#[derive(Debug, thiserror::Error)]
enum ProfileDataError {
    #[error("Profile data is not consistent with profile mode (internal error)")]
    ProfileDataNotConsistent,
}

#[derive(Clone, Debug)]
pub(crate) enum ProfileDataImpl {
    Bc(Box<BcProfileData>),
    BcPairs(BcPairsProfileData),
    AggregateHeapProfileInfo(Box<AggregateHeapProfileInfo>),
    Other(String),
}

/// Collected profiling data.
#[derive(Clone, Debug)]
pub struct ProfileData {
    pub(crate) profile_mode: ProfileMode,
    /// Serialized to text (e.g. CSV or flamegraph).
    pub(crate) profile: ProfileDataImpl,
}

impl ProfileData {
    pub(crate) fn new(profile_mode: ProfileMode, profile: String) -> ProfileData {
        ProfileData {
            profile_mode,
            profile: ProfileDataImpl::Other(profile),
        }
    }

    /// Generate a string with profile data (e.g. CSV or flamegraph, depending on profile type).
    pub fn gen(&self) -> anyhow::Result<String> {
        match (&self.profile, &self.profile_mode) {
            (ProfileDataImpl::Other(profile), _) => Ok(profile.clone()),
            (ProfileDataImpl::Bc(bc), _) => Ok(bc.gen_csv()),
            (ProfileDataImpl::BcPairs(bc_pairs), _) => Ok(bc_pairs.gen_csv()),
            (
                ProfileDataImpl::AggregateHeapProfileInfo(profile),
                ProfileMode::HeapFlameRetained | ProfileMode::HeapFlameAllocated,
            ) => Ok(profile.gen_flame_graph()),
            (
                ProfileDataImpl::AggregateHeapProfileInfo(profile),
                ProfileMode::HeapSummaryRetained | ProfileMode::HeapSummaryAllocated,
            ) => Ok(profile.gen_summary_csv()),
            (ProfileDataImpl::AggregateHeapProfileInfo(_), _) => {
                Err(ProfileDataError::ProfileDataNotConsistent.into())
            }
        }
    }

    /// Write to a file.
    pub fn write(&self, path: &Path) -> anyhow::Result<()> {
        fs::write(path, &self.gen()?).with_context(|| {
            format!(
                "write profile `{}` data to `{}`",
                self.profile_mode,
                path.display()
            )
        })?;
        Ok(())
    }
}
