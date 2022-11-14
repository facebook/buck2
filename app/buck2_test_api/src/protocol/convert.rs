/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use anyhow::Context as _;
use host_sharing::HostSharingRequirements;
use host_sharing::WeightClass;

pub fn host_sharing_requirements_from_grpc(
    input: buck2_test_proto::HostSharingRequirements,
) -> anyhow::Result<HostSharingRequirements> {
    use buck2_test_proto::host_sharing_requirements::*;

    let requirements = match input.requirements.context("Missing `requirements`")? {
        Requirements::Shared(Shared { weight }) => {
            host_sharing::HostSharingRequirements::Shared(WeightClass::Permits(weight.try_into()?))
        }
        Requirements::ExclusiveAccess(ExclusiveAccess {}) => {
            host_sharing::HostSharingRequirements::ExclusiveAccess
        }
        Requirements::OnePerToken(OnePerToken { identifier, weight }) => {
            host_sharing::HostSharingRequirements::OnePerToken(
                identifier,
                WeightClass::Permits(weight.try_into()?),
            )
        }
    };

    Ok(requirements)
}

pub fn host_sharing_requirements_to_grpc(
    input: HostSharingRequirements,
) -> anyhow::Result<buck2_test_proto::HostSharingRequirements> {
    use buck2_test_proto::host_sharing_requirements::*;

    let requirements = match input {
        host_sharing::HostSharingRequirements::Shared(WeightClass::Permits(weight)) => {
            Requirements::Shared(Shared {
                weight: weight.try_into()?,
            })
        }
        host_sharing::HostSharingRequirements::ExclusiveAccess => {
            Requirements::ExclusiveAccess(ExclusiveAccess {})
        }
        host_sharing::HostSharingRequirements::OnePerToken(
            identifier,
            WeightClass::Permits(weight),
        ) => Requirements::OnePerToken(OnePerToken {
            identifier,
            weight: weight.try_into()?,
        }),
    };

    Ok(buck2_test_proto::HostSharingRequirements {
        requirements: Some(requirements),
    })
}
