/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use derive_more::Display;

#[derive(Debug, Display, Eq, PartialEq)]
pub(crate) enum OomEvidence {
    #[display("the kernel reported killing daemon PID {pid}")]
    KernelVictim { pid: i64, line: String },
    #[display("oomd reported killing cgroup `{cgroup}` containing the daemon")]
    OomdCgroup { cgroup: String, line: String },
    #[display("systemd-oomd reported killing cgroup `{cgroup}` containing the daemon")]
    SystemdOomdCgroup { cgroup: String, line: String },
}

impl OomEvidence {
    fn line(&self) -> &str {
        match self {
            Self::KernelVictim { line, .. }
            | Self::OomdCgroup { line, .. }
            | Self::SystemdOomdCgroup { line, .. } => line,
        }
    }
}

pub(crate) mod build_graph_stats;
pub(crate) mod build_id_writer;
pub(crate) mod classify_server_stderr;
pub(crate) mod console_output_limit;
pub(crate) mod emit_event;
pub(crate) mod errorconsole;
pub mod event_log;
pub(crate) mod health_check_subscriber;
pub(crate) mod observer;
#[cfg(target_os = "linux")]
pub(crate) mod oom;
pub mod re_log;
pub mod recorder;
pub(crate) mod simpleconsole;
pub mod stdout_stderr_forwarder;
pub mod subscriber;
pub mod superconsole;
pub(crate) mod system_warning;
pub(crate) mod test_id_writer;
