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

use crate::eval::runtime::profile::bc::BcPairsProfileData;
use crate::eval::runtime::profile::bc::BcPairsProfilerType;
use crate::eval::runtime::profile::bc::BcProfileData;
use crate::eval::runtime::profile::bc::BcProfilerType;
use crate::eval::runtime::profile::flamegraph::FlameGraphData;
use crate::eval::runtime::profile::heap::HeapFlameAllocatedProfilerType;
use crate::eval::runtime::profile::heap::HeapFlameRetainedProfilerType;
use crate::eval::runtime::profile::heap::HeapSummaryAllocatedProfilerType;
use crate::eval::runtime::profile::heap::HeapSummaryRetainedProfilerType;
use crate::eval::runtime::profile::mode::ProfileMode;
use crate::eval::runtime::profile::profiler_type::ProfilerType;
use crate::eval::runtime::profile::stmt::StmtProfileData;
use crate::eval::runtime::profile::time_flame::TimeFlameProfilerType;
use crate::eval::runtime::profile::typecheck::TypecheckProfileData;
use crate::values::AggregateHeapProfileInfo;

#[derive(Debug, thiserror::Error)]
enum ProfileDataError {
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
    Coverage(StmtProfileData),
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
            ProfileDataImpl::Coverage(_) => ProfileMode::Coverage,
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
    pub fn gen(&self) -> crate::Result<String> {
        match &self.profile {
            ProfileDataImpl::Bc(bc) => Ok(bc.gen_csv()),
            ProfileDataImpl::BcPairs(bc_pairs) => Ok(bc_pairs.gen_csv()),
            ProfileDataImpl::HeapFlameRetained(profile)
            | ProfileDataImpl::HeapFlameAllocated(profile) => Ok(profile.gen_flame_graph()),
            ProfileDataImpl::HeapSummaryRetained(profile)
            | ProfileDataImpl::HeapSummaryAllocated(profile) => Ok(profile.gen_summary_csv()),
            ProfileDataImpl::TimeFlameProfile(data) => Ok(data.write()),
            ProfileDataImpl::Statement(data) => Ok(data.write_to_string()),
            ProfileDataImpl::Coverage(data) => Ok(data.write_coverage()),
            ProfileDataImpl::Typecheck(data) => Ok(data.gen_csv()),
        }
    }

    /// Write to a file.
    pub fn write(&self, path: &Path) -> crate::Result<()> {
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
    ) -> crate::Result<ProfileData> {
        let profiles = Vec::from_iter(profiles);
        let profile_mode = match profiles.first() {
            None => return Err(crate::Error::new_other(ProfileDataError::EmptyProfileList)),
            Some(p) => p.profile.profile_mode(),
        };
        for p in &profiles {
            if p.profile.profile_mode() != profile_mode {
                return Err(crate::Error::new_other(
                    ProfileDataError::DifferentProfileModes,
                ));
            }
        }
        let profile = match &profile_mode {
            ProfileMode::Bytecode => BcProfilerType::merge_profiles(&profiles)?.profile,
            ProfileMode::BytecodePairs => BcPairsProfilerType::merge_profiles(&profiles)?.profile,
            ProfileMode::HeapSummaryAllocated => {
                HeapSummaryAllocatedProfilerType::merge_profiles(&profiles)?.profile
            }
            ProfileMode::HeapSummaryRetained => {
                HeapSummaryRetainedProfilerType::merge_profiles(&profiles)?.profile
            }
            ProfileMode::HeapFlameAllocated => {
                HeapFlameAllocatedProfilerType::merge_profiles(&profiles)?.profile
            }
            ProfileMode::HeapFlameRetained => {
                HeapFlameRetainedProfilerType::merge_profiles(&profiles)?.profile
            }
            ProfileMode::TimeFlame => TimeFlameProfilerType::merge_profiles(&profiles)?.profile,
            profile_mode => {
                return Err(crate::Error::new_other(
                    ProfileDataError::MergeNotImplemented(profile_mode.dupe()),
                ));
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
    fn test_smoke_merge() {
        for profile_mode in ProfileMode::ALL {
            let profile = ProfileData {
                profile: match profile_mode {
                    ProfileMode::Bytecode => ProfileDataImpl::Bc(Box::default()),
                    ProfileMode::BytecodePairs => {
                        ProfileDataImpl::BcPairs(BcPairsProfileData::default())
                    }
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
                    ProfileMode::TimeFlame => {
                        ProfileDataImpl::TimeFlameProfile(FlameGraphData::default())
                    }
                    ProfileMode::Statement => ProfileDataImpl::Statement(Default::default()),
                    ProfileMode::Coverage => ProfileDataImpl::Coverage(Default::default()),
                    ProfileMode::Typecheck => ProfileDataImpl::Typecheck(Default::default()),
                },
            };
            let result = ProfileData::merge([&profile, &profile]);
            match profile_mode {
                ProfileMode::Statement | ProfileMode::Coverage | ProfileMode::Typecheck => {
                    assert!(
                        result.is_err(),
                        "Merge for {profile_mode:?} must be not implemented yet"
                    )
                }
                _ => {
                    assert!(
                        result.is_ok(),
                        "Merge for {profile_mode:?} must be implemented"
                    )
                }
            }
        }
    }
}
