use std::convert::TryInto;

use anyhow::Context as _;
use host_sharing::{HostSharingRequirements, WeightClass};

pub fn host_sharing_requirements_from_grpc(
    input: test_proto::HostSharingRequirements,
) -> anyhow::Result<HostSharingRequirements> {
    use test_proto::host_sharing_requirements::*;

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
) -> anyhow::Result<test_proto::HostSharingRequirements> {
    use test_proto::host_sharing_requirements::*;

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

    Ok(test_proto::HostSharingRequirements {
        requirements: Some(requirements),
    })
}
