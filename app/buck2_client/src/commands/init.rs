/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::io::ErrorKind;
use std::io::Write;
use std::path::Path;
use std::process::Command;

use anyhow::Context;
use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::common::CommonConsoleOptions;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::final_console::FinalConsole;
use buck2_client_ctx::path_arg::PathArg;
use buck2_core::fs::fs_util;

/// Buck2 Init
///
/// This command is intented to be part-tutorial part-convenience
/// for generating buck2 projects. Given a path and optional name
/// (in the case that the folder name is not desirable).
#[derive(Debug, clap::Parser)]
#[clap(name = "install", about = "Initialize a buck2 project")]
pub struct InitCommand {
    #[clap(flatten)]
    console_opts: CommonConsoleOptions,

    /// The path to initialize the project in. The folder does not need to exist.
    #[clap(default_value = ".")]
    path: PathArg,

    /// The name for the project. If not provided will default to the last segment
    /// of the path.
    #[clap(short, long)]
    name: Option<String>,

    /// Don't generate a prelude or a toolchain.
    #[clap(long)]
    no_prelude: bool,

    /// Initialize the project even if the git repo at \[PATH\] has uncommitted changes.
    #[clap(long)]
    allow_dirty: bool,
}

impl InitCommand {
    pub fn exec(self, _matches: &clap::ArgMatches, ctx: ClientCommandContext) -> ExitResult {
        let console = self.console_opts.final_console();

        match exec_impl(self, ctx, &console) {
            Ok(_) => ExitResult::Status(0),
            Err(e) => {
                // include the backtrace with the error output
                // (same behaviour as returning the Error from main)
                console.print_error(&format!("{:?}", e))?;
                ExitResult::Status(1)
            }
        }
    }
}

fn exec_impl(
    cmd: InitCommand,
    ctx: ClientCommandContext,
    console: &FinalConsole,
) -> anyhow::Result<()> {
    let path = cmd.path.resolve(&ctx.working_dir);
    fs_util::create_dir_all(&path)?;
    let absolute = fs_util::canonicalize(&path)?;

    if absolute.is_file() {
        return Err(anyhow::anyhow!(
            "Target path {} cannot be an existing file",
            absolute.display()
        ));
    }

    let status = match Command::new("git")
        .args(["status", "--porcelain"])
        .current_dir(&absolute)
        .output()
    {
        Err(e) if e.kind().eq(&ErrorKind::NotFound) => {
            console.print_error(
                "Warning: no git found on path, can't check for dirty repo. Proceeding anyway.",
            )?;
            None
        }
        r => Some(r.context("Couldn't detect dirty status of folder.")?),
    };

    let changes = status.filter(|o| o.status.success()).map(|o| {
        String::from_utf8_lossy(&o.stdout)
            .trim()
            .lines()
            .any(|l| !l.starts_with("??"))
    });

    if let (Some(true), false) = (changes, cmd.allow_dirty) {
        return Err(anyhow::anyhow!(
            "Refusing to initialize in a dirty repo. Stash your changes or use `--allow-dirty` to override."
        ));
    }

    set_up_project(&absolute, !cmd.no_prelude)
}

fn initialize_buckconfig(path: &Path, prelude: bool) -> anyhow::Result<()> {
    let mut buckconfig = std::fs::File::create(path.join(".buckconfig"))?;
    writeln!(buckconfig, "[repositories]")?;
    writeln!(buckconfig, "root = .")?;
    writeln!(buckconfig, "prelude = prelude")?;

    // Add additional configs that depend on prelude / no-prelude mode
    if prelude {
        writeln!(buckconfig, "toolchains = toolchains")?;
        writeln!(buckconfig, "none = none")?;
        writeln!(buckconfig)?;
        writeln!(buckconfig, "[repository_aliases]")?;
        writeln!(buckconfig, "config = prelude")?;
        writeln!(buckconfig, "fbcode = none")?;
        writeln!(buckconfig, "fbsource = none")?;
        writeln!(buckconfig, "buck = none")?;
        writeln!(buckconfig)?;
        writeln!(buckconfig, "[parser]")?;
        writeln!(
            buckconfig,
            "target_platform_detector_spec = target:root//...->prelude//platforms:default"
        )?;
    } else {
        // For the no-prelude mode, create an empty prelude/prelude.bzl as Buck2 expects one.
        let prelude_dir = path.join("prelude");
        fs_util::create_dir(prelude_dir.as_path())?;
        fs_util::create_file(prelude_dir.join("prelude.bzl"))?;
    }
    Ok(())
}

fn initialize_toolchains_buck(path: &Path) -> anyhow::Result<()> {
    let mut buck = std::fs::File::create(path.join("BUCK"))?;

    writeln!(
        buck,
        "load(\"@prelude//toolchains:genrule.bzl\", \"system_genrule_toolchain\")",
    )?;
    writeln!(buck)?;
    writeln!(buck, "system_genrule_toolchain(")?;
    writeln!(buck, "    name = \"genrule\",")?;
    writeln!(buck, "    visibility = [\"PUBLIC\"],")?;
    writeln!(buck, ")")?;

    Ok(())
}

fn initialize_root_buck(path: &Path, prelude: bool) -> anyhow::Result<()> {
    let mut buck = std::fs::File::create(path.join("BUCK"))?;

    if prelude {
        writeln!(
            buck,
            "# A list of available rules and their signatures can be found here: https://buck2.build/docs/generated/starlark/prelude/prelude.bzl"
        )?;
        writeln!(buck)?;
        writeln!(buck, "genrule(")?;
        writeln!(buck, "    name = \"hello_world\",")?;
        writeln!(buck, "    out = \"out.txt\",")?;
        writeln!(buck, "    cmd = \"echo BUILT BY BUCK2> $OUT\",")?;
        writeln!(buck, ")")?;
    }
    // TODO: Add a doc pointers for rules
    Ok(())
}

fn set_up_project(path: &Path, prelude: bool) -> anyhow::Result<()> {
    if !Command::new("git")
        .arg("init")
        .current_dir(path)
        .status()?
        .success()
    {
        return Err(anyhow::anyhow!("Failure when running `git init`."));
    };

    if prelude {
        if !Command::new("git")
            .args([
                "submodule",
                "add",
                "https://github.com/facebookincubator/buck2-prelude.git",
                "prelude",
            ])
            .current_dir(path)
            .status()?
            .success()
        {
            return Err(anyhow::anyhow!(
                "Unable to clone the prelude. Is the folder in use?"
            ));
        }
    }

    // If the project already contains a .buckconfig, leave it alone
    if path.join(".buckconfig").exists() {
        return Ok(());
    }

    initialize_buckconfig(path, prelude)?;
    if prelude {
        let toolchains = path.join("toolchains");
        if !toolchains.exists() {
            fs_util::create_dir(&toolchains)?;
            initialize_toolchains_buck(toolchains.as_path())?;
        }
    }
    if !path.join("BUCK").exists() {
        initialize_root_buck(path, prelude)?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use buck2_core::fs::fs_util;

    use crate::commands::init::initialize_buckconfig;
    use crate::commands::init::initialize_root_buck;

    #[test]
    fn test_buckconfig_generation_with_prelude() -> anyhow::Result<()> {
        let tempdir = tempfile::tempdir()?;
        let tempdir_path = tempdir.path();
        fs_util::create_dir_all(tempdir_path)?;

        let buckconfig_path = tempdir_path.join(".buckconfig");
        initialize_buckconfig(tempdir_path, true)?;
        let actual_buckconfig = fs_util::read_to_string(buckconfig_path)?;
        let expected_buckconfig = "[repositories]
root = .
prelude = prelude
toolchains = toolchains
none = none

[repository_aliases]
config = prelude
fbcode = none
fbsource = none
buck = none

[parser]
target_platform_detector_spec = target:root//...->prelude//platforms:default
";
        assert_eq!(actual_buckconfig, expected_buckconfig);
        Ok(())
    }

    #[test]
    fn test_buckconfig_generation_without_prelude() -> anyhow::Result<()> {
        let tempdir = tempfile::tempdir()?;
        let tempdir_path = tempdir.path();
        fs_util::create_dir_all(tempdir_path)?;

        let buckconfig_path = tempdir_path.join(".buckconfig");
        initialize_buckconfig(tempdir_path, false)?;
        let actual_buckconfig = fs_util::read_to_string(buckconfig_path)?;
        let expected_buckconfig = "[repositories]
root = .
prelude = prelude
";
        assert_eq!(actual_buckconfig, expected_buckconfig);

        // Test we have an empty prelude directory and prelude.bzl file
        assert!(tempdir_path.join("prelude/prelude.bzl").exists());
        Ok(())
    }

    #[test]
    fn test_buckfile_generation_with_prelude() -> anyhow::Result<()> {
        let tempdir = tempfile::tempdir()?;
        let tempdir_path = tempdir.path();
        fs_util::create_dir_all(tempdir_path)?;

        let buck_path = tempdir_path.join("BUCK");
        initialize_root_buck(tempdir_path, true)?;
        let actual_buck = fs_util::read_to_string(buck_path)?;
        let expected_buck = "# A list of available rules and their signatures can be found here: https://buck2.build/docs/generated/starlark/prelude/prelude.bzl

genrule(
    name = \"hello_world\",
    out = \"out.txt\",
    cmd = \"echo BUILT BY BUCK2> $OUT\",
)
";
        assert_eq!(actual_buck, expected_buck);
        Ok(())
    }
}
