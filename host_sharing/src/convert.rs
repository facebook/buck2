/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use anyhow::Context as _;

use crate::HostSharingRequirements;
use crate::WeightClass;
use crate::WeightPercentage;

pub fn weight_class_from_grpc(
    input: buck2_host_sharing_proto::WeightClass,
) -> anyhow::Result<WeightClass> {
    use buck2_host_sharing_proto::weight_class::*;

    Ok(match input.value.context("Missing `value`")? {
        Value::Permits(p) => WeightClass::Permits(p.try_into().context("Invalid `permits`")?),
        Value::Percentage(p) => {
            WeightClass::Percentage(WeightPercentage::try_new(p).context("Invalid `percentage`")?)
        }
    })
}

pub fn host_sharing_requirements_from_grpc(
    input: buck2_host_sharing_proto::HostSharingRequirements,
) -> anyhow::Result<HostSharingRequirements> {
    use buck2_host_sharing_proto::host_sharing_requirements::*;

    let requirements = match input.requirements.context("Missing `requirements`")? {
        Requirements::Shared(Shared { weight_class }) => crate::HostSharingRequirements::Shared(
            weight_class_from_grpc(weight_class.context("Missing `weight_class`")?)?,
        ),
        Requirements::ExclusiveAccess(ExclusiveAccess {}) => {
            crate::HostSharingRequirements::ExclusiveAccess
        }
        Requirements::OnePerToken(OnePerToken {
            identifier,
            weight_class,
        }) => crate::HostSharingRequirements::OnePerToken(
            identifier,
            weight_class_from_grpc(weight_class.context("Missing `weight_class`")?)?,
        ),
    };

    Ok(requirements)
}

pub fn weight_class_to_grpc(
    input: WeightClass,
) -> anyhow::Result<buck2_host_sharing_proto::WeightClass> {
    use buck2_host_sharing_proto::weight_class::*;

    let value = match input {
        crate::WeightClass::Permits(p) => {
            Value::Permits(p.try_into().context("Invalid `permits`")?)
        }
        crate::WeightClass::Percentage(p) => Value::Percentage(p.into_value().into()),
    };

    Ok(buck2_host_sharing_proto::WeightClass { value: Some(value) })
}

pub fn host_sharing_requirements_to_grpc(
    input: HostSharingRequirements,
) -> anyhow::Result<buck2_host_sharing_proto::HostSharingRequirements> {
    use buck2_host_sharing_proto::host_sharing_requirements::*;

    let requirements = match input {
        crate::HostSharingRequirements::Shared(weight) => Requirements::Shared(Shared {
            weight_class: Some(weight_class_to_grpc(weight)?),
        }),
        crate::HostSharingRequirements::ExclusiveAccess => {
            Requirements::ExclusiveAccess(ExclusiveAccess {})
        }
        crate::HostSharingRequirements::OnePerToken(identifier, weight) => {
            Requirements::OnePerToken(OnePerToken {
                identifier,
                weight_class: Some(weight_class_to_grpc(weight)?),
            })
        }
    };

    Ok(buck2_host_sharing_proto::HostSharingRequirements {
        requirements: Some(requirements),
    })
}
