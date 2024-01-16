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
use std::sync::OnceLock;

use buck2_core::facebook_only;
use buck2_wrapper_common::BUCK2_WRAPPER_ENV_VAR;

use crate::daemon_id::DAEMON_UUID;

/// Collects metadata from the current binary and environment and writes it as map, suitable for telemetry purposes.
pub fn collect() -> HashMap<String, String> {
    facebook_only();
    fn add_env_var(map: &mut HashMap<String, String>, key: &'static str, var: &'static str) {
        if let Ok(data) = env::var(var) {
            map.insert(key.to_owned(), data);
        }
    }

    let mut map = HashMap::new();

    let info = system_info();
    if let Some(hostname) = info.hostname {
        map.insert("hostname".to_owned(), hostname);
    }
    if let Some(username) = info.username {
        map.insert("username".to_owned(), username);
    }

    if let Some(rev) = buck2_build_info::revision() {
        map.insert("buck2_revision".to_owned(), rev.to_owned());
    }

    if let Some(time) = buck2_build_info::time_iso8601() {
        map.insert("buck2_build_time".to_owned(), time.to_owned());
    }

    if let Some(ts) = buck2_build_info::release_timestamp() {
        map.insert("buck2_release_timestamp".to_owned(), ts.to_owned());
    }

    if is_proc_translated::is_proc_translated() {
        map.insert("rosetta".to_owned(), "1".to_owned());
    }

    // Global trace ID
    map.insert("daemon_uuid".to_owned(), DAEMON_UUID.to_string());

    map.insert("os".to_owned(), info.os);
    if let Some(version) = info.os_version {
        map.insert("os_version".to_owned(), version);
    }

    add_env_var(&mut map, "sandcastle_job_info", "SANDCASTLE_JOB_INFO");
    add_env_var(&mut map, "sandcastle_alias", "SANDCASTLE_ALIAS");
    add_env_var(&mut map, "launched_via_wrapper", BUCK2_WRAPPER_ENV_VAR);
    add_env_var(&mut map, "fbpackage_name", "FBPACKAGE_PACKAGE_NAME");
    add_env_var(&mut map, "fbpackage_version", "FBPACKAGE_PACKAGE_VERSION");
    add_env_var(&mut map, "fbpackage_release", "FBPACKAGE_PACKAGE_RELEASE");
    add_env_var(
        &mut map,
        "skycastle_workflow_run_id",
        "SKYCASTLE_WORKFLOW_RUN_ID",
    );
    add_env_var(
        &mut map,
        "skycastle_workflow_alias",
        "SKYCASTLE_WORKFLOW_ALIAS",
    );
    map
}

pub struct SystemInfo {
    pub username: Option<String>,
    pub hostname: Option<String>,
    pub os: String,
    pub os_version: Option<String>,
}

pub fn system_info() -> SystemInfo {
    let hostname = hostname();
    let username;
    #[cfg(fbcode_build)]
    {
        username = user::current_username().ok();
    }
    #[cfg(not(fbcode_build))]
    {
        username = None;
    }

    SystemInfo {
        hostname,
        username,
        os: os_type(),
        os_version: sys_info::os_release().ok(),
    }
}

/// The operating system - "linux" "darwin" "windows" etc.
fn os_type() -> String {
    if cfg!(target_os = "linux") {
        "linux".to_owned()
    } else if cfg!(target_os = "macos") {
        "darwin".to_owned()
    } else if cfg!(target_os = "windows") {
        "windows".to_owned()
    } else {
        "unknown".to_owned()
    }
}

pub fn hostname() -> Option<String> {
    static CELL: OnceLock<Option<String>> = OnceLock::new();

    CELL.get_or_init(|| {
        hostname::get()
            .ok()
            .map(|res| res.to_string_lossy().into_owned())
    })
    .clone()
}
