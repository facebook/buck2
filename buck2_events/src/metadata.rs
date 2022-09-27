/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Metadata collection, for telemetry purposes.
use std::collections::HashMap;
use std::env;

use buck2_core::facebook_only;

/// Collects metadata from the current binary and environment and writes it as map, suitable for telemetry purposes.
pub fn collect() -> HashMap<String, String> {
    facebook_only();
    fn add_env_var(map: &mut HashMap<String, String>, key: &'static str, var: &'static str) {
        if let Ok(data) = env::var(var) {
            map.insert(key.to_owned(), data);
        }
    }

    let mut map = HashMap::new();

    #[cfg(feature = "extra_logging")]
    {
        use once_cell::sync::Lazy;

        use crate::trace::TraceId;

        // Global trace ID
        static DAEMON_UUID: Lazy<TraceId> = Lazy::new(TraceId::new);
        map.insert("daemon_uuid".to_owned(), DAEMON_UUID.to_string());

        map.insert(
            "hostname".to_owned(),
            hostname::get_hostname().unwrap_or_else(|_| "".to_owned()),
        );
        map.insert(
            "username".to_owned(),
            user::current_username().unwrap_or_else(|_| "".to_owned()),
        );
        // The revision that built the buck2 binary currently in use.
        map.insert(
            "buck2_revision".to_owned(),
            build_info::BuildInfo::get_revision().to_owned(),
        );
        // The time when the buck2 binary servicing this command was built.
        map.insert(
            "buck2_build_time".to_owned(),
            build_info::BuildInfo::get_time_iso8601().to_owned(),
        );
        // The operating system - "linux" "darwin" "windows" etc.
        if let Ok(os_type) = sys_info::os_type() {
            map.insert("os".to_owned(), os_type.to_lowercase());
        }
        // The version of the operating system.
        if let Ok(version) = sys_info::os_release() {
            map.insert("os_version".to_owned(), version);
        }
    }
    #[cfg(not(feature = "extra_logging"))]
    {
        #[cfg(fbcode_build)]
        compile_error!("extra_logging must be enabled when compiling in fbcode");
    }

    add_env_var(&mut map, "sandcastle_job_info", "SANDCASTLE_JOB_INFO");
    add_env_var(&mut map, "sandcastle_alias", "SANDCASTLE_ALIAS");
    add_env_var(&mut map, "launched_via_wrapper", "BUCK2_WRAPPER");
    add_env_var(&mut map, "fbpackage_name", "FBPACKAGE_PACKAGE_NAME");
    add_env_var(&mut map, "fbpackage_version", "FBPACKAGE_PACKAGE_VERSION");
    add_env_var(&mut map, "fbpackage_release", "FBPACKAGE_PACKAGE_RELEASE");
    map
}
