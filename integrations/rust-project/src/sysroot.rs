/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::path::Path;
use std::path::PathBuf;
use std::process::Command;
use std::process::Stdio;

use anyhow::Context;
use anyhow::anyhow;
use tracing::instrument;

use crate::buck::Buck;
use crate::buck::truncate_line_ending;
use crate::buck::utf8_output;
use crate::cli::develop_with_sysroot;
use crate::project_json::Sysroot;
use crate::target::Target;

#[derive(Debug)]
pub(crate) enum SysrootConfig {
    Sysroot {
        sysroot: PathBuf,
        sysroot_src: Option<PathBuf>,
    },
    BuckConfig,
    Rustup {
        sysroot_src: Option<PathBuf>,
    },
}

/// Choose sysroot and sysroot_src based on platform.
///
/// `sysroot` is the directory that contains std crates:
/// <https://doc.rust-lang.org/rustc/command-line-arguments.html#--sysroot-override-the-system-root>
/// and also contains libexec helpers such as rust-analyzer-proc-macro-srv.
///
/// `sysroot_src` is the directory that contains the source to std crates:
/// <https://rust-analyzer.github.io/manual.html#non-cargo-based-projects>
#[instrument(skip_all)]
pub(crate) fn resolve_buckconfig_sysroot(
    buck: &Buck,
    project_root: &Path,
) -> Result<Sysroot, anyhow::Error> {
    let sysroot: PathBuf = {
        // TODO(diliopoulos): remove hardcoded path to toolchain sysroot and replace with something
        // derived from buck, e.g.
        //
        // $ buck cquery -u fbcode//buck2/integrations/rust-project:rust-project -a compiler fbcode//buck2/platform/rust:rust_bootstrap
        // ...
        //     "compiler": "fbcode//tools/build/buck/wrappers:rust-platform010-clang-17-nosan-compiler (fbcode//buck2/platform/execution:linux-x86_64#54c5d1cbad5316cb)"
        // $ buck cquery -u fbcode//buck2/integrations/rust-project:rust-project -a exe fbcode//tools/build/buck/wrappers:rust-platform010-clang-17-nosan-compiler
        // ...
        //     "exe": "fbcode//third-party-buck/platform010/build/rust/llvm-fb-17:bin/rustc (fbcode//buck2/platform/execution:linux-x86_64#54c5d1cbad5316cb)",
        let fbsource_rustc = project_root.join("xplat/rust/toolchain/current/basic/bin/rustc");
        let mut sysroot_cmd = Command::new(fbsource_rustc);
        sysroot_cmd
            .arg("--print=sysroot")
            .stdin(Stdio::null())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped());
        let sysroot_child = sysroot_cmd.spawn()?;

        let mut sysroot = utf8_output(sysroot_child.wait_with_output(), &sysroot_cmd)
            .context("error asking rustc for sysroot")?;
        truncate_line_ending(&mut sysroot);
        sysroot.into()
    };

    let sysroot_src = buck.resolve_sysroot_src()?;
    let sysroot_targets = Target::new(format!("fbsource//{}:", sysroot_src.to_string_lossy()));
    // the `library` path component needs to be appended to the `sysroot_src_path`
    // so that rust-analyzer will be able to find standard library sources.
    let sysroot_src = project_root.join(sysroot_src).join("library");

    let mut sysroot_project = develop_with_sysroot(
        buck,
        vec![sysroot_targets],
        Sysroot {
            sysroot: sysroot.clone(),
            sysroot_src: sysroot_src.clone(),
            sysroot_project: None,
        },
        true,
        false,
        false,
        false,
        &[], // sysroot doesn't get any extra cfgs
    )?;
    for krate in &mut sysroot_project.crates {
        if let Some(display_name) = &mut krate.display_name {
            *display_name = display_name
                .strip_suffix("-0.0.0") // rust-analyzer identifies lang crates by name, so we need `core-0.0.0` to be `core`
                .unwrap_or(display_name)
                .to_owned();
        }
    }

    Ok(Sysroot {
        sysroot,
        sysroot_src,
        sysroot_project: Some(sysroot_project),
    })
}

#[instrument(ret)]
pub(crate) fn resolve_rustup_sysroot(
    sysroot_src_override: Option<PathBuf>,
) -> Result<Sysroot, anyhow::Error> {
    let mut cmd = Command::new("rustc");
    cmd.arg("--print=sysroot")
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());

    let mut output = utf8_output(cmd.output(), &cmd)?;
    truncate_line_ending(&mut output);
    let sysroot = PathBuf::from(output);

    let sysroot = if let Some(sysroot_src) = sysroot_src_override {
        validate(&sysroot_src)
            .context("Invalid --sysroot-src, did not contain the standard library source code")?;
        Sysroot {
            sysroot,
            sysroot_src,
            sysroot_project: None,
        }
    } else {
        let sysroot_src = Sysroot::sysroot_src_for_sysroot(&sysroot);
        validate(&sysroot_src)
            .context("Rustup toolchain did not have rust-src component installed")?;
        Sysroot {
            sysroot_src,
            sysroot,
            sysroot_project: None,
        }
    };

    Ok(sysroot)
}

pub(crate) fn resolve_provided_sysroot(
    sysroot: &Path,
    sysroot_src_override: Option<&Path>,
) -> Result<Sysroot, anyhow::Error> {
    let sysroot = expand_tilde(sysroot)?.canonicalize().context(format!(
        "--sysroot path could not be canonicalized: {}",
        sysroot.display()
    ))?;
    let sysroot_src = if let Some(path) = sysroot_src_override {
        let path = expand_tilde(path)?.canonicalize().context(format!(
            "--sysroot-src path could not be canonicalized: {}",
            path.display()
        ))?;
        validate(&path)
            .context("Invalid --sysroot-src, did not contain the standard library source code")?;
        path
    } else {
        let path = Sysroot::sysroot_src_for_sysroot(&sysroot);
        validate(&path)
            .context("Provided --sysroot did not contain the standard library source code")?;
        path
    };
    Ok(Sysroot {
        sysroot,
        sysroot_src,
        sysroot_project: None,
    })
}

fn validate(sysroot_src: &Path) -> Result<(), anyhow::Error> {
    let core = sysroot_src.join("core");
    if !sysroot_src.exists() {
        return Err(anyhow!("No such directory {}", sysroot_src.display()));
    }
    if !core.exists() {
        return Err(anyhow!(
            "No `core` directory in {}. Are you sure this is a sysroot src directory?",
            sysroot_src.display()
        ));
    }
    Ok(())
}

fn expand_tilde(path: &Path) -> Result<PathBuf, anyhow::Error> {
    if path.starts_with("~") {
        let path = path.strip_prefix("~")?;
        let home = std::env::var("HOME").context("HOME environment variable not set")?;
        let home = PathBuf::from(home);
        Ok(home.join(path))
    } else {
        Ok(path.to_path_buf())
    }
}
