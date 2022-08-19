/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Translation between buck core data and the test spec data types
use anyhow::Context;
use buck2_core::cells::CellResolver;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_test_api::data::ConfiguredTarget;

use crate::session::TestSession;

pub fn build_configured_target_handle(
    target: ConfiguredProvidersLabel,
    session: &TestSession,
    cell_resolver: &CellResolver,
) -> anyhow::Result<ConfiguredTarget> {
    let name = test_target_name(&target);

    let label = target.target().unconfigured();
    let cell = label.pkg().cell_name().to_string();
    let package = label.pkg().cell_relative_path().to_string();
    let target_name = label.name().to_string();
    let configuration = target.cfg().to_string();
    let package_project_relative_path = cell_resolver
        .resolve_path(label.pkg().as_cell_path())
        .context("Failed to resolve the project relative path of package")?;

    Ok(ConfiguredTarget {
        handle: session.register(target),
        name,
        cell,
        package,
        target: target_name,
        configuration,
        package_project_relative_path: package_project_relative_path.into(),
    })
}

fn test_target_name(target: &ConfiguredProvidersLabel) -> String {
    // We emulate the target name that Buck v1 would provide. This matters because downstream
    // dependencies such as unittest finder expect exact matches here.
    let label = target.target().unconfigured();
    format!("{}:{}", label.pkg().cell_relative_path(), label.name())
}

pub fn convert_test_result(
    test_result: buck2_test_api::data::TestResult,
) -> anyhow::Result<buck2_data::TestResult> {
    let buck2_test_api::data::TestResult {
        name,
        status,
        msg,
        duration,
        details,
        ..
    } = test_result;
    Ok(buck2_data::TestResult {
        name,
        status: status.try_into().context("Invalid `status`")?,
        msg: msg.map(|msg| buck2_data::test_result::OptionalMsg { msg }),
        duration: duration.map(Into::into),
        details,
    })
}
