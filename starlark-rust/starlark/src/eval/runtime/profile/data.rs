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
use gazebo::prelude::*;

use crate::eval::runtime::profile::bc::BcPairsProfileData;
use crate::eval::runtime::profile::bc::BcProfileData;
use crate::eval::ProfileMode;
use crate::values::AggregateHeapProfileInfo;

#[derive(Debug, thiserror::Error)]
enum ProfileDataError {
    #[error("Profile data is not consistent with profile mode (internal error)")]
    ProfileDataNotConsistent,
    #[error("Empty profile list cannot be merged")]
    EmptyProfileList,
    #[error("Different profile modes in profile")]
    DifferentProfileModes,
    #[error("Merge of profile data for profile mode `{0}` is not implemented")]
    MergeNotImplemented(ProfileMode),
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

    /// Merge profiles (aggregate).
    pub fn merge<'a>(
        profiles: impl IntoIterator<Item = &'a ProfileData>,
    ) -> anyhow::Result<ProfileData> {
        let profiles = Vec::from_iter(profiles);
        let profile_mode = match profiles.first() {
            None => return Err(ProfileDataError::EmptyProfileList.into()),
            Some(p) => p.profile_mode.dupe(),
        };
        for p in &profiles {
            if p.profile_mode != profile_mode {
                return Err(ProfileDataError::DifferentProfileModes.into());
            }
        }
        let profile = match &profile_mode {
            ProfileMode::Bytecode => {
                let profiles = profiles.try_map(|p| match &p.profile {
                    ProfileDataImpl::Bc(bc) => Ok(&**bc),
                    _ => Err(ProfileDataError::ProfileDataNotConsistent),
                })?;
                let profile = BcProfileData::merge(profiles);
                ProfileDataImpl::Bc(box profile)
            }
            ProfileMode::BytecodePairs => {
                let profiles = profiles.try_map(|p| match &p.profile {
                    ProfileDataImpl::BcPairs(bc_pairs) => Ok(bc_pairs),
                    _ => Err(ProfileDataError::ProfileDataNotConsistent),
                })?;
                let profile = BcPairsProfileData::merge(profiles);
                ProfileDataImpl::BcPairs(profile)
            }
            ProfileMode::HeapSummaryAllocated
            | ProfileMode::HeapSummaryRetained
            | ProfileMode::HeapFlameAllocated
            | ProfileMode::HeapFlameRetained => {
                let profiles = profiles.try_map(|p| match &p.profile {
                    ProfileDataImpl::AggregateHeapProfileInfo(profile) => Ok(&**profile),
                    _ => Err(ProfileDataError::ProfileDataNotConsistent),
                })?;
                let profile = AggregateHeapProfileInfo::merge(profiles);
                ProfileDataImpl::AggregateHeapProfileInfo(box profile)
            }
            profile_mode => {
                return Err(ProfileDataError::MergeNotImplemented(profile_mode.dupe()).into());
            }
        };
        Ok(ProfileData {
            profile_mode,
            profile,
        })
    }
}

#[cfg(test)]
mod tests {
    use gazebo::dupe::Dupe;

    use crate::eval::runtime::profile::bc::BcPairsProfileData;
    use crate::eval::runtime::profile::bc::BcProfileData;
    use crate::eval::runtime::profile::data::ProfileDataImpl;
    use crate::eval::ProfileData;
    use crate::eval::ProfileMode;
    use crate::values::AggregateHeapProfileInfo;

    #[test]
    fn merge_bc() {
        let profile = ProfileData {
            profile_mode: ProfileMode::Bytecode,
            profile: ProfileDataImpl::Bc(box BcProfileData::default()),
        };
        // Smoke.
        ProfileData::merge([&profile, &profile]).unwrap();
    }

    #[test]
    fn merge_bc_pairs() {
        let profile = ProfileData {
            profile_mode: ProfileMode::BytecodePairs,
            profile: ProfileDataImpl::BcPairs(BcPairsProfileData::default()),
        };
        // Smoke.
        ProfileData::merge([&profile, &profile]).unwrap();
    }

    #[test]
    fn merge_aggregated_heap_profile() {
        for profile_mode in [
            ProfileMode::HeapFlameRetained,
            ProfileMode::HeapFlameAllocated,
            ProfileMode::HeapSummaryRetained,
            ProfileMode::HeapSummaryAllocated,
        ] {
            let profile = ProfileData {
                profile_mode: profile_mode.dupe(),
                profile: ProfileDataImpl::AggregateHeapProfileInfo(
                    box AggregateHeapProfileInfo::default(),
                ),
            };
            // Smoke.
            ProfileData::merge([&profile, &profile]).unwrap();
        }
    }
}
