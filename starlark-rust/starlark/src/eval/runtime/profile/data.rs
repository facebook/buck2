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
use dupe::Dupe;
use starlark_syntax::slice_vec_ext::SliceExt;

use crate::eval::runtime::profile::bc::BcPairsProfileData;
use crate::eval::runtime::profile::bc::BcProfileData;
use crate::eval::runtime::profile::flamegraph::FlameGraphData;
use crate::eval::runtime::profile::mode::ProfileMode;
use crate::eval::runtime::profile::stmt::StmtProfileData;
use crate::eval::runtime::profile::typecheck::TypecheckProfileData;
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
    HeapFlameRetained(Box<AggregateHeapProfileInfo>),
    HeapFlameAllocated(Box<AggregateHeapProfileInfo>),
    HeapSummaryRetained(Box<AggregateHeapProfileInfo>),
    HeapSummaryAllocated(Box<AggregateHeapProfileInfo>),
    /// Flame graph data is in milliseconds.
    TimeFlameProfile(FlameGraphData),
    Statement(StmtProfileData),
    Typecheck(TypecheckProfileData),
}

impl ProfileDataImpl {
    pub(crate) fn profile_mode(&self) -> ProfileMode {
        match self {
            ProfileDataImpl::Bc(_) => ProfileMode::Bytecode,
            ProfileDataImpl::BcPairs(_) => ProfileMode::BytecodePairs,
            ProfileDataImpl::HeapFlameRetained(_) => ProfileMode::HeapFlameRetained,
            ProfileDataImpl::HeapFlameAllocated(_) => ProfileMode::HeapFlameAllocated,
            ProfileDataImpl::HeapSummaryRetained(_) => ProfileMode::HeapSummaryRetained,
            ProfileDataImpl::HeapSummaryAllocated(_) => ProfileMode::HeapSummaryAllocated,
            ProfileDataImpl::TimeFlameProfile(_) => ProfileMode::TimeFlame,
            ProfileDataImpl::Statement(_) => ProfileMode::Statement,
            ProfileDataImpl::Typecheck(_) => ProfileMode::Typecheck,
        }
    }
}

/// Collected profiling data.
#[derive(Clone, Debug)]
pub struct ProfileData {
    pub(crate) profile: ProfileDataImpl,
}

fn _assert_profile_data_send_sync() {
    fn _assert_send_sync<T: Send + Sync>() {}
    _assert_send_sync::<ProfileData>();
}

impl ProfileData {
    /// Generate a string with profile data (e.g. CSV or flamegraph, depending on profile type).
    pub fn gen(&self) -> anyhow::Result<String> {
        match &self.profile {
            ProfileDataImpl::Bc(bc) => Ok(bc.gen_csv()),
            ProfileDataImpl::BcPairs(bc_pairs) => Ok(bc_pairs.gen_csv()),
            ProfileDataImpl::HeapFlameRetained(profile)
            | ProfileDataImpl::HeapFlameAllocated(profile) => Ok(profile.gen_flame_graph()),
            ProfileDataImpl::HeapSummaryRetained(profile)
            | ProfileDataImpl::HeapSummaryAllocated(profile) => Ok(profile.gen_summary_csv()),
            ProfileDataImpl::TimeFlameProfile(data) => Ok(data.write()),
            ProfileDataImpl::Statement(data) => Ok(data.write_to_string()),
            ProfileDataImpl::Typecheck(data) => Ok(data.gen_csv()),
        }
    }

    /// Write to a file.
    pub fn write(&self, path: &Path) -> anyhow::Result<()> {
        fs::write(path, self.gen()?).with_context(|| {
            format!(
                "write profile `{}` data to `{}`",
                self.profile.profile_mode(),
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
            Some(p) => p.profile.profile_mode(),
        };
        for p in &profiles {
            if p.profile.profile_mode() != profile_mode {
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
                ProfileDataImpl::Bc(Box::new(profile))
            }
            ProfileMode::BytecodePairs => {
                let profiles = profiles.try_map(|p| match &p.profile {
                    ProfileDataImpl::BcPairs(bc_pairs) => Ok(bc_pairs),
                    _ => Err(ProfileDataError::ProfileDataNotConsistent),
                })?;
                let profile = BcPairsProfileData::merge(profiles);
                ProfileDataImpl::BcPairs(profile)
            }
            ProfileMode::HeapSummaryAllocated => {
                let profiles = profiles.try_map(|p| match &p.profile {
                    ProfileDataImpl::HeapSummaryAllocated(profile) => Ok(&**profile),
                    _ => Err(ProfileDataError::ProfileDataNotConsistent),
                })?;
                let profile = AggregateHeapProfileInfo::merge(profiles);
                ProfileDataImpl::HeapSummaryAllocated(Box::new(profile))
            }
            ProfileMode::HeapSummaryRetained => {
                let profiles = profiles.try_map(|p| match &p.profile {
                    ProfileDataImpl::HeapSummaryRetained(profile) => Ok(&**profile),
                    _ => Err(ProfileDataError::ProfileDataNotConsistent),
                })?;
                let profile = AggregateHeapProfileInfo::merge(profiles);
                ProfileDataImpl::HeapSummaryRetained(Box::new(profile))
            }
            ProfileMode::HeapFlameAllocated => {
                let profiles = profiles.try_map(|p| match &p.profile {
                    ProfileDataImpl::HeapFlameAllocated(profile) => Ok(&**profile),
                    _ => Err(ProfileDataError::ProfileDataNotConsistent),
                })?;
                let profile = AggregateHeapProfileInfo::merge(profiles);
                ProfileDataImpl::HeapFlameAllocated(Box::new(profile))
            }
            ProfileMode::HeapFlameRetained => {
                let profiles = profiles.try_map(|p| match &p.profile {
                    ProfileDataImpl::HeapFlameRetained(profile) => Ok(&**profile),
                    _ => Err(ProfileDataError::ProfileDataNotConsistent),
                })?;
                let profile = AggregateHeapProfileInfo::merge(profiles);
                ProfileDataImpl::HeapFlameRetained(Box::new(profile))
            }
            ProfileMode::TimeFlame => {
                let profiles = profiles.try_map(|p| match &p.profile {
                    ProfileDataImpl::TimeFlameProfile(data) => Ok(data),
                    _ => Err(ProfileDataError::ProfileDataNotConsistent),
                })?;
                let profile = FlameGraphData::merge(profiles);
                ProfileDataImpl::TimeFlameProfile(profile)
            }
            profile_mode => {
                return Err(ProfileDataError::MergeNotImplemented(profile_mode.dupe()).into());
            }
        };
        Ok(ProfileData { profile })
    }
}

#[cfg(test)]
mod tests {
    use crate::eval::runtime::profile::bc::BcPairsProfileData;
    use crate::eval::runtime::profile::data::ProfileDataImpl;
    use crate::eval::runtime::profile::flamegraph::FlameGraphData;
    use crate::eval::runtime::profile::mode::ProfileMode;
    use crate::eval::ProfileData;

    #[test]
    fn merge_bc() {
        let profile = ProfileData {
            profile: ProfileDataImpl::Bc(Box::default()),
        };
        // Smoke.
        ProfileData::merge([&profile, &profile]).unwrap();
    }

    #[test]
    fn merge_bc_pairs() {
        let profile = ProfileData {
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
                profile: match profile_mode {
                    ProfileMode::HeapFlameRetained => {
                        ProfileDataImpl::HeapFlameRetained(Box::default())
                    }
                    ProfileMode::HeapFlameAllocated => {
                        ProfileDataImpl::HeapFlameAllocated(Box::default())
                    }
                    ProfileMode::HeapSummaryRetained => {
                        ProfileDataImpl::HeapSummaryRetained(Box::default())
                    }
                    ProfileMode::HeapSummaryAllocated => {
                        ProfileDataImpl::HeapSummaryAllocated(Box::default())
                    }
                    _ => unreachable!(),
                },
            };
            // Smoke.
            ProfileData::merge([&profile, &profile]).unwrap();
        }
    }

    #[test]
    fn merge_time_flame() {
        let profile = ProfileData {
            profile: ProfileDataImpl::TimeFlameProfile(FlameGraphData::default()),
        };
        // Smoke.
        ProfileData::merge([&profile, &profile]).unwrap();
    }
}
