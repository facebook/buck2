/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::io;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;
use std::process::Output;
use std::process::Stdio;

use anyhow::Context;
use serde::Deserialize;
use tracing::debug;
use tracing::enabled;
use tracing::info;
use tracing::instrument;
use tracing::trace;
use tracing::warn;
use tracing::Level;

use crate::json_project::Edition;
use crate::json_project::JsonProject;
use crate::target::AliasedTargetInfo;
use crate::target::Kind;
use crate::target::MacroOutput;
use crate::target::Target;
use crate::target::TargetInfo;
use crate::target::TargetInfoEntry;
use crate::Crate;
use crate::Dep;

pub fn to_json_project(
    sysroot: Option<PathBuf>,
    targets: Vec<Target>,
    target_map: BTreeMap<Target, TargetInfo>,
    aliases: BTreeMap<Target, AliasedTargetInfo>,
    proc_macros: BTreeMap<Target, MacroOutput>,
) -> Result<JsonProject, anyhow::Error> {
    let targets: BTreeSet<_> = targets.into_iter().collect();
    let target_index = merge_unit_test_targets(target_map);

    let mut crates: Vec<Crate> = Vec::with_capacity(target_index.len());
    for (target, TargetInfoEntry { info, index: _ }) in &target_index {
        let mut deps = resolve_dependencies_aliases(info, &target_index, &aliases, &proc_macros);
        resolve_renamed_dependencies(info, &target_index, &mut deps);

        let edition = match &info.edition {
            Some(edition) => edition.clone(),
            None => Edition::Edition2021,
        };

        // we need to take the existing features and prefix `feature=`
        // before passing it to rust-analyzer via `rust-project.json`.
        let mut cfg = info
            .clone()
            .features
            .into_iter()
            .map(|f| format!("feature=\"{f}\""))
            .collect::<Vec<String>>();

        // Include "test" cfg so rust-analyzer picks up #[cfg(test)] code.
        cfg.push("test".to_owned());

        debug!(?target, "loading proc macro");
        // the mapping here is inverted, which means we need to search through the keys for the Target.
        // thankfully, most projects don't have to many proc macros, which means the size of this list
        // remains in the two digit space.
        let proc_macro_dylib_path = proc_macros
            .values()
            .find(|macro_output| macro_output.actual == *target)
            .map(|macro_output| macro_output.dylib.clone());

        // We don't need to push the source folder as rust-analyzer by default will use the root-module parent().
        // info.root_module() will output either the fbcode source file or the symlinked one based on if it's a mapped source or not
        let root_module = info.root_module();

        let crate_info = Crate {
            display_name: Some(info.name.clone()),
            root_module,
            edition,
            deps,
            is_workspace_member: targets.contains(target),
            source: None,
            cfg,
            target: None,
            env: BTreeMap::new(),
            is_proc_macro: info.proc_macro.unwrap_or(false),
            proc_macro_dylib_path,
        };
        crates.push(crate_info);
    }

    let (sysroot, sysroot_src) = match sysroot {
        Some(s) => (Some(s), None),
        None => rust_sysroot(&Buck.get_project_root()?)?,
    };

    let jp = JsonProject {
        sysroot,
        sysroot_src,
        crates,
        // needed to ignore the generated `rust-project.json` in diffs, but including the actual
        // string will mark this file as generated
        generated: "\x40generated",
    };

    Ok(jp)
}

fn resolve_dependencies_aliases(
    info: &TargetInfo,
    target_index: &BTreeMap<Target, TargetInfoEntry>,
    aliases: &BTreeMap<Target, AliasedTargetInfo>,
    proc_macros: &BTreeMap<Target, MacroOutput>,
) -> Vec<Dep> {
    let mut deps = vec![];
    for dependency_target in &info.deps {
        let dependency_target = match aliases.get(dependency_target) {
            Some(actual) => &actual.actual,
            None => {
                // we fall back to check the proc macros for aliases
                // (these should exist in the aliases map, but they don't. yolo.)
                match proc_macros.get(dependency_target) {
                    Some(MacroOutput { actual, .. }) => actual,
                    None => dependency_target,
                }
            }
        };

        if let Some(entry) = target_index.get(dependency_target) {
            trace!(?dependency_target, "present in target_index");
            let dep = Dep {
                crate_index: entry.index,
                name: entry.info.crate_name(),
            };
            deps.push(dep);
        } else {
            trace!(?dependency_target, "not present in target_index");
        }
    }

    deps
}

fn resolve_renamed_dependencies(
    info: &TargetInfo,
    target_index: &BTreeMap<Target, TargetInfoEntry>,
    deps: &mut Vec<Dep>,
) {
    // we handled named_deps when constructing the dependency, as rust-analyzer cares about the
    // the crate name for correct resolution. `named_deps` are distinct from `deps` in buck2 and
    // are not currently unified into a `buck.direct_dependencies` or `$deps` in buck2.
    // TODO: once https://fb.workplace.com/groups/buck2users/posts/3137264549863238/?comment_id=3137265756529784
    // is resolved, switch to `$deps`.
    for (renamed_crate, dependency_target) in &info.named_deps {
        if let Some(entry) = target_index.get(dependency_target) {
            trace!(old_name = ?entry.info.crate_name(), new_name = ?renamed_crate, "renamed crate");
            // if the renamed dependency was encountered before, rename the existing `Dep` rather
            // than create a new one with a new name but the same index. While this duplication doesn't
            // seem to have any noticeable impact in limited testing, the behavior will be closer to
            // that of Rusty and Cargo.
            //
            // However, if the renamed dependency wasn't encountered before, we create a new `Dep` with
            // the new name.
            //
            // The primary invariant that is being upheld is that each index should
            // have one associated name.
            match deps.iter_mut().find(|dep| dep.crate_index == entry.index) {
                Some(dep) => dep.name = renamed_crate.to_string(),
                None => {
                    let dep = Dep {
                        crate_index: entry.index,
                        name: renamed_crate.to_string(),
                    };
                    deps.push(dep);
                }
            };
        }
    }
}

fn merge_unit_test_targets(
    target_map: BTreeMap<Target, TargetInfo>,
) -> BTreeMap<Target, TargetInfoEntry> {
    let mut target_index = BTreeMap::new();

    let (tests, mut targets): (BTreeMap<Target, TargetInfo>, BTreeMap<Target, TargetInfo>) =
        target_map
            .into_iter()
            .partition(|(_, info)| info.kind == Kind::Test);

    let (generated_unit_tests, standalone_tests): (
        BTreeMap<Target, TargetInfo>,
        BTreeMap<Target, TargetInfo>,
    ) = tests.into_iter().partition(|(target, _)| {
        targets
            .iter()
            .any(|(_, value)| value.tests.contains(target))
    });

    targets.extend(standalone_tests);

    for (index, (target, mut info)) in targets.into_iter().enumerate() {
        trace!(?target, ?info, index, "adding dependency");

        // merge the `-unittest` target with the parent target.
        for test_dep in &info.tests {
            if let Some(test_info) = generated_unit_tests.get(test_dep) {
                for test_dep in &test_info.deps {
                    if !info.deps.contains(test_dep) {
                        info.deps.push(test_dep.clone())
                    }
                }
            }
        }
        target_index.insert(target.to_owned(), TargetInfoEntry { index, info });
    }
    target_index
}

// Choose sysroot and sysroot_src based on platform.
// sysroot is expected to contain libexec helpers such as rust-analyzer-proc-macro-srv.
// Non-linux platforms use fbsource/xplat/rust/toolchain/sysroot/VERSION for
// sysroot_src since their sysroot bundled with rustc doesn't ship source. Once it
// does, dispense with sysroot_src completely.
#[instrument(level = "debug", ret)]
fn rust_sysroot(fbsource: &Path) -> Result<(Option<PathBuf>, Option<PathBuf>), anyhow::Error> {
    if cfg!(target_os = "linux") {
        return Ok((
            Some(fbsource.join("fbcode/third-party-buck/platform010/build/rust")),
            None,
        ));
    }

    // Spawn both `rustc` and `buck audit config` in parallel without blocking.

    let fbsource_rustc = fbsource.join("xplat/rust/toolchain/current/rustc");
    let mut sysroot_cmd = if cfg!(target_os = "macos") {
        // On Apple silicon, buck builds at Meta run under Rosetta.
        // So we force an x86-64 sysroot to avoid mixing architectures.
        let mut arch_cmd = Command::new("arch");
        arch_cmd.arg("-x86_64");
        arch_cmd.arg(fbsource_rustc);
        arch_cmd
    } else {
        Command::new(fbsource_rustc)
    };
    sysroot_cmd.arg("--print=sysroot");
    sysroot_cmd.stdin(Stdio::null());
    sysroot_cmd.stdout(Stdio::piped());
    sysroot_cmd.stderr(Stdio::piped());
    let sysroot_child = sysroot_cmd.spawn()?;

    let mut buck_config_cmd = Buck.command();
    buck_config_cmd.args(["audit", "config", "--json", "--", "rust.sysroot_src_path"]);
    buck_config_cmd.stdin(Stdio::null());
    buck_config_cmd.stdout(Stdio::piped());
    buck_config_cmd.stderr(Stdio::piped());
    let buck_config_child = buck_config_cmd.spawn()?;

    // Now block while we wait for both processes.

    let mut sysroot = utf8_output(sysroot_child.wait_with_output(), &sysroot_cmd)
        .context("error asking rustc for sysroot")?;
    truncate_line_ending(&mut sysroot);

    let sysroot_src = {
        #[derive(Deserialize)]
        struct BuckConfig {
            #[serde(rename = "rust.sysroot_src_path")]
            rust_sysroot_src_path: PathBuf,
        }
        let BuckConfig {
            rust_sysroot_src_path,
        } = deserialize_output(buck_config_child.wait_with_output(), &buck_config_cmd)?;
        let mut p = fbsource.to_owned();
        p.extend(&rust_sysroot_src_path);
        p
    };

    Ok((Some(sysroot.into()), Some(sysroot_src)))
}

#[derive(Debug)]
pub struct Buck;

impl Buck {
    pub fn command(&self) -> Command {
        Command::new("buck2")
    }

    /// Resolve the root of the current buck tree all-up
    pub fn get_project_root(&self) -> Result<PathBuf, anyhow::Error> {
        let mut command = self.command();
        command.args(["root", "--kind=project"]);

        let mut stdout = utf8_output(command.output(), &command)?;
        truncate_line_ending(&mut stdout);

        if enabled!(Level::TRACE) {
            trace!(%stdout, "Got root from buck");
        }

        Ok(stdout.into())
    }

    /// Expands a Buck target expression.
    ///
    /// Since `rust-project` accepts Buck target experssions like '//common/rust/tools/rust-project/...',
    /// it is necessary to expand the target expression to query the *actual* targets.
    #[instrument(skip_all)]
    pub fn expand_targets(&self, targets: &[Target]) -> Result<Vec<Target>, anyhow::Error> {
        let mut command = self.command();
        command.args([
            "bxl",
            "prelude//rust/rust-analyzer/resolve_deps.bxl:expand_targets",
            "--",
            "--targets",
        ]);
        command.args(targets);
        tracing::info!("expanding provided target expressions");
        let raw = deserialize_output(command.output(), &command)?;
        if enabled!(Level::TRACE) {
            for target in &raw {
                trace!(%target, "Expanded target from buck");
            }
        }
        Ok(raw)
    }

    #[instrument(skip_all)]
    pub fn resolve_deps(
        &self,
        targets: &[Target],
    ) -> Result<BTreeMap<Target, TargetInfo>, anyhow::Error> {
        let mut command = self.command();
        command.args([
            "bxl",
            "prelude//rust/rust-analyzer/resolve_deps.bxl:query",
            "--",
            "--targets",
        ]);
        command.args(targets);

        if targets.len() <= 10 {
            // printing out 10 targets is pretty reasonable information for the user
            info!(
                targets_num = targets.len(),
                ?targets,
                "resolving dependencies..."
            );
        } else {
            // after 10 targets, however, things tend to get a bit unwieldy.
            info!(targets_num = targets.len(), "resolving dependencies...");
            debug!(?targets);
        }
        let raw = deserialize_output(command.output(), &command)?;

        if enabled!(Level::TRACE) {
            for (target, info) in &raw {
                trace!(%target, ?info, "Parsed target from buck");
            }
        }
        Ok(raw)
    }

    #[instrument(skip_all)]
    pub fn query_proc_macros(
        &self,
        targets: &[Target],
    ) -> Result<BTreeMap<Target, MacroOutput>, anyhow::Error> {
        let mut command = self.command();

        command.args([
            "bxl",
            "prelude//rust/rust-analyzer/resolve_deps.bxl:expand_proc_macros",
            "--",
            "--targets",
        ]);
        command.args(targets);

        info!("building proc macros");
        let raw: BTreeMap<Target, MacroOutput> = deserialize_output(command.output(), &command)?;

        Ok(raw)
    }

    #[instrument(skip_all)]
    pub fn query_aliased_libraries(
        &self,
        targets: &[Target],
    ) -> Result<BTreeMap<Target, AliasedTargetInfo>, anyhow::Error> {
        let mut command = self.command();

        // Fetch all aliases used by transitive deps. This is so we
        // can translate an apparent dependency of e.g.
        // fbsource//third-party/rust:once_cell to the actual target
        // name of fbsource//third-party/rust:once_cell-1.15. This
        // query also fetches non-Rust aliases, but they shouldn't
        // hurt anything.
        command.args([
            "cquery",
            "--output-all-attributes",
            "kind('^alias$', deps(%Ss))",
        ]);
        command.args(targets);

        info!("resolving aliased libraries");
        let raw: BTreeMap<Target, AliasedTargetInfo> =
            deserialize_output(command.output(), &command)?;

        if enabled!(Level::TRACE) {
            for (target, info) in &raw {
                trace!(%target, ?info, "Parsed target from buck");
            }
        }
        Ok(raw)
    }

    #[instrument(skip_all)]
    pub fn query_owner(
        &self,
        files: Vec<PathBuf>,
    ) -> Result<HashMap<PathBuf, Vec<Target>>, anyhow::Error> {
        let mut command = self.command();

        command.args(["uquery", "--json", "owner(%s)", "--"]);
        command.args(&files);

        info!(?files, "Querying buck to determine owner");
        let out = deserialize_output(command.output(), &command)?;
        Ok(out)
    }
}

pub fn utf8_output(output: io::Result<Output>, command: &Command) -> Result<String, anyhow::Error> {
    match output {
        Ok(Output {
            stdout,
            stderr,
            status,
        }) if status.success() => String::from_utf8(stdout)
            .or_else(|err| {
                let context = cmd_err(command, err.as_bytes(), &stderr);
                Err(err).context(context)
            })
            .context("command returned non-utf8 output"),
        Ok(output) => Err(cmd_err(command, &output.stdout, &output.stderr))
            .with_context(|| format!("command ended with {}", output.status)),
        Err(err) => Err(err)
            .with_context(|| format!("command `{:?}`", command))
            .context("failed to execute command"),
    }
}

pub fn deserialize_output<T>(
    output: io::Result<Output>,
    command: &Command,
) -> Result<T, anyhow::Error>
where
    T: for<'a> Deserialize<'a>,
{
    match output {
        Ok(Output {
            stdout,
            stderr,
            status,
        }) if status.success() => {
            tracing::debug!("parsing command output");
            serde_json::from_slice(&stdout)
                .with_context(|| cmd_err(command, &stdout, &stderr))
                .context("failed to deserialize command output")
        }
        Ok(output) => Err(cmd_err(command, &output.stdout, &output.stderr))
            .with_context(|| format!("command ended with {}", output.status)),
        Err(err) => Err(err)
            .with_context(|| format!("command `{:?}`", command))
            .context("failed to execute command"),
    }
}

fn cmd_err(command: &Command, stdout: &[u8], stderr: &[u8]) -> anyhow::Error {
    anyhow::anyhow!(
        "command `{:?}`\nstdout:\n{}\nstderr:\n{}",
        command,
        String::from_utf8_lossy(stdout),
        String::from_utf8_lossy(stderr),
    )
}

/// Trim a trailing new line from `String`.
/// Useful when trimming command output.
pub fn truncate_line_ending(s: &mut String) {
    if let Some(x) = s.strip_suffix("\r\n").or_else(|| s.strip_suffix('\n')) {
        s.truncate(x.len());
    }
}
