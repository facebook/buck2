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
use host_sharing::WeightPercentage;

pub fn weight_class_from_grpc(input: buck2_test_proto::WeightClass) -> anyhow::Result<WeightClass> {
    use buck2_test_proto::weight_class::*;

    Ok(match input.value.context("Missing `value`")? {
        Value::Permits(p) => WeightClass::Permits(p.try_into().context("Invalid `permits`")?),
        Value::Percentage(p) => {
            WeightClass::Percentage(WeightPercentage::try_new(p).context("Invalid `percentage`")?)
        }
    })
}

pub fn host_sharing_requirements_from_grpc(
    input: buck2_test_proto::HostSharingRequirements,
) -> anyhow::Result<HostSharingRequirements> {
    use buck2_test_proto::host_sharing_requirements::*;

    let requirements = match input.requirements.context("Missing `requirements`")? {
        Requirements::Shared(Shared { weight_class }) => {
            host_sharing::HostSharingRequirements::Shared(weight_class_from_grpc(
                weight_class.context("Missing `weight_class`")?,
            )?)
        }
        Requirements::ExclusiveAccess(ExclusiveAccess {}) => {
            host_sharing::HostSharingRequirements::ExclusiveAccess
        }
        Requirements::OnePerToken(OnePerToken {
            identifier,
            weight_class,
        }) => host_sharing::HostSharingRequirements::OnePerToken(
            identifier,
            weight_class_from_grpc(weight_class.context("Missing `weight_class`")?)?,
        ),
    };

    Ok(requirements)
}

pub fn weight_class_to_grpc(input: WeightClass) -> anyhow::Result<buck2_test_proto::WeightClass> {
    use buck2_test_proto::weight_class::*;

    let value = match input {
        host_sharing::WeightClass::Permits(p) => {
            Value::Permits(p.try_into().context("Invalid `permits`")?)
        }
        host_sharing::WeightClass::Percentage(p) => Value::Percentage(p.into_value().into()),
    };

    Ok(buck2_test_proto::WeightClass { value: Some(value) })
}

pub fn host_sharing_requirements_to_grpc(
    input: HostSharingRequirements,
) -> anyhow::Result<buck2_test_proto::HostSharingRequirements> {
    use buck2_test_proto::host_sharing_requirements::*;

    let requirements = match input {
        host_sharing::HostSharingRequirements::Shared(weight) => Requirements::Shared(Shared {
            weight_class: Some(weight_class_to_grpc(weight)?),
        }),
        host_sharing::HostSharingRequirements::ExclusiveAccess => {
            Requirements::ExclusiveAccess(ExclusiveAccess {})
        }
        host_sharing::HostSharingRequirements::OnePerToken(identifier, weight) => {
            Requirements::OnePerToken(OnePerToken {
                identifier,
                weight_class: Some(weight_class_to_grpc(weight)?),
            })
        }
    };

    Ok(buck2_test_proto::HostSharingRequirements {
        requirements: Some(requirements),
    })
}
