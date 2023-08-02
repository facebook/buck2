/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::path::Path;
use std::path::PathBuf;
use std::process::Command;
use std::process::Stdio;

use anyhow::Context;
use tracing::instrument;

use crate::buck::relative_to;
use crate::buck::truncate_line_ending;
use crate::buck::utf8_output;
use crate::buck::Buck;
use crate::json_project::Sysroot;

pub enum SysrootConfig {
    Sysroot(PathBuf),
    BuckConfig,
    Rustup,
}

/// Choose sysroot and sysroot_src based on platform.
///
/// `sysroot` is the directory that contains std crates:
/// <https://doc.rust-lang.org/rustc/command-line-arguments.html#--sysroot-override-the-system-root>
/// and also contains libexec helpers such as rust-analyzer-proc-macro-srv.
///
/// `sysroot_src` is the directory that contains the source to std crates:
/// <https://rust-analyzer.github.io/manual.html#non-cargo-based-projects>
#[instrument(ret)]
pub fn resolve_buckconfig_sysroot(
    project_root: &Path,
    relative_paths: bool,
) -> Result<Sysroot, anyhow::Error> {
    let buck = Buck::default();

    if cfg!(target_os = "linux") {
        let base: PathBuf = if relative_paths {
            PathBuf::from("")
        } else {
            project_root.into()
        };

        let sysroot_src = buck.resolve_sysroot_src()?;
        let sysroot_src = if relative_paths {
            sysroot_src
        } else {
            project_root.join(sysroot_src)
        };

        // TODO(diliopoulos): remove hardcoded path to toolchain sysroot and replace with:
        // buck2 run fbcode//third-party-buck/platform010/build/rust:bin/rustc -- --print sysroot
        let sysroot = Sysroot {
            sysroot: base.join("fbcode/third-party-buck/platform010/build/rust/llvm-fb-12"),
            sysroot_src: Some(sysroot_src),
        };

        return Ok(sysroot);
    }
    // Spawn both `rustc` and `buck audit config` in parallel without blocking.
    let fbsource_rustc = project_root.join("xplat/rust/toolchain/current/basic/bin/rustc");
    let mut sysroot_cmd = if cfg!(target_os = "macos") {
        // On Apple silicon, buck builds at Meta run under Rosetta.
        // So we force an x86-64 sysroot to avoid mixing architectures.
        let mut cmd = Command::new("arch");
        cmd.arg("-x86_64").arg(fbsource_rustc);
        cmd
    } else {
        Command::new(fbsource_rustc)
    };
    sysroot_cmd
        .arg("--print=sysroot")
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());
    let sysroot_child = sysroot_cmd.spawn()?;

    let sysroot_src = buck.resolve_sysroot_src()?;
    let sysroot_src = if relative_paths {
        sysroot_src
    } else {
        project_root.join(sysroot_src)
    };

    // Now block while we wait for both processes.
    let mut sysroot = utf8_output(sysroot_child.wait_with_output(), &sysroot_cmd)
        .context("error asking rustc for sysroot")?;
    truncate_line_ending(&mut sysroot);

    let mut sysroot: PathBuf = sysroot.into();
    if relative_paths {
        sysroot = relative_to(&sysroot, project_root);
    }

    let sysroot = Sysroot {
        sysroot,
        sysroot_src: Some(sysroot_src),
    };

    Ok(sysroot)
}

#[instrument(ret)]
pub fn resolve_rustup_sysroot() -> Result<Sysroot, anyhow::Error> {
    let mut cmd = Command::new("rustc");
    cmd.arg("--print=sysroot")
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());

    let output = utf8_output(cmd.output(), &cmd)?;
    let sysroot = PathBuf::from(output);

    let sysroot = Sysroot {
        sysroot,
        sysroot_src: None,
    };
    Ok(sysroot)
}
