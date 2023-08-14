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
use std::process::ExitStatus;
use std::process::Output;
use std::process::Stdio;

use anyhow::Context;
use serde::Deserialize;
use tracing::enabled;
use tracing::info;
use tracing::instrument;
use tracing::trace;
use tracing::warn;
use tracing::Level;

use crate::json_project::Edition;
use crate::json_project::JsonProject;
use crate::json_project::Sysroot;
use crate::target::AliasedTargetInfo;
use crate::target::ExpandedAndResolved;
use crate::target::Kind;
use crate::target::MacroOutput;
use crate::target::Target;
use crate::target::TargetInfo;
use crate::target::TargetInfoEntry;
use crate::Crate;
use crate::Dep;

pub fn to_json_project(
    sysroot: Sysroot,
    expanded_and_resolved: ExpandedAndResolved,
    aliases: BTreeMap<Target, AliasedTargetInfo>,
    relative_paths: bool,
) -> Result<JsonProject, anyhow::Error> {
    let mode = select_mode(None);
    let buck = Buck::new(mode);

    let ExpandedAndResolved {
        expanded_targets: targets,
        queried_proc_macros: proc_macros,
        resolved_deps: target_map,
    } = expanded_and_resolved;

    let targets: BTreeSet<_> = targets.into_iter().collect();
    let target_index = merge_unit_test_targets(target_map);
    let project_root = buck.resolve_project_root()?;

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

        // the mapping here is inverted, which means we need to search through the keys for the Target.
        // thankfully, most projects don't have to many proc macros, which means the size of this list
        // remains in the two digit space.
        let mut proc_macro_dylib_path = proc_macros
            .values()
            .find(|macro_output| macro_output.actual == *target)
            .map(|macro_output| macro_output.dylib.clone());
        if let Some(ref dylib) = proc_macro_dylib_path {
            trace!(?target, ?dylib, "target is a proc macro");
        }

        // We don't need to push the source folder as rust-analyzer by default will use the root-module parent().
        // info.root_module() will output either the fbcode source file or the symlinked one based on if it's a mapped source or not
        let mut root_module = info.root_module();

        if relative_paths {
            proc_macro_dylib_path = proc_macro_dylib_path.map(|p| relative_to(&p, &project_root));
            root_module = relative_to(&root_module, &project_root);
        }

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

    let jp = JsonProject {
        sysroot,
        crates,
        // needed to ignore the generated `rust-project.json` in diffs, but including the actual
        // string will mark this file as generated
        generated: "\x40generated",
    };

    Ok(jp)
}

/// If `path` starts with `base`, drop the prefix.
pub fn relative_to(path: &Path, base: &Path) -> PathBuf {
    match path.strip_prefix(base) {
        Ok(rel_path) => rel_path,
        Err(_) => path,
    }
    .to_owned()
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

/// For every test target, drop it from `target_map` and include test
/// target's dependencies in the target that references the test
/// target.
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

        // Merge the `-unittest` target with the parent target.
        let unittest_target = Target::new(format!("{target}-unittest"));
        if info.tests.contains(&unittest_target) {
            if let Some(test_info) = generated_unit_tests.get(&unittest_target) {
                for test_dep in &test_info.deps {
                    if !info.deps.contains(test_dep) && *test_dep != target {
                        info.deps.push(test_dep.clone())
                    }
                }
            }
        }

        target_index.insert(target.to_owned(), TargetInfoEntry { index, info });
    }
    target_index
}

#[derive(Debug, Default)]
pub struct Buck {
    mode: Option<String>,
}

impl Buck {
    pub fn new(mode: Option<String>) -> Self {
        Buck { mode }
    }

    pub fn command(&self) -> Command {
        Command::new("buck2")
    }

    /// Return the absolute path of the current Buck project root.
    pub fn resolve_project_root(&self) -> Result<PathBuf, anyhow::Error> {
        let mut command = self.command();

        command.args(["root", "--kind=project"]);

        let mut stdout = utf8_output(command.output(), &command)?;
        truncate_line_ending(&mut stdout);

        if enabled!(Level::TRACE) {
            trace!(%stdout, "got root from buck");
        }

        Ok(stdout.into())
    }

    pub fn resolve_sysroot_src(&self) -> Result<PathBuf, anyhow::Error> {
        let mut command = self.command();
        command.args(["audit", "config", "--json", "--", "rust.sysroot_src_path"]);
        command
            .stderr(Stdio::null())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped());

        // the `spawn()` here is load-bearing to allow spawning
        // this command concurrently with rustc.
        let child = command.spawn().context("Unable to spawn command")?;

        #[derive(Deserialize)]
        struct BuckConfig {
            #[serde(rename = "rust.sysroot_src_path")]
            sysroot_src_path: PathBuf,
        }
        let cfg: BuckConfig = deserialize_output(child.wait_with_output(), &command)?;
        // the `library` path component needs to be appended to the `sysroot_src_path`
        // so that rust-analyzer will be able to find standard library sources.
        let cfg = cfg.sysroot_src_path.join("library");
        Ok(cfg)
    }

    /// Determines the owning target(s) of the saved file and builds them.
    #[instrument]
    pub fn check_saved_file(
        &self,
        use_clippy: bool,
        saved_filed: &Path,
    ) -> Result<Vec<PathBuf>, anyhow::Error> {
        let mut command = self.command();

        command.args(["--isolation-dir", ".rust-analyzer"]);

        command.arg("bxl");
        command.args(["--oncall", "rust_devx", "-c", "client.id=rust-project"]);

        if let Some(mode) = &self.mode {
            command.arg(mode);
        }

        command.args([
            "prelude//rust/rust-analyzer/check.bxl:check",
            "-c=rust.failure_filter=true",
            "-c=rust.incremental=true",
        ]);

        // apply BXL scripts-specific arguments:
        command.args(["--", "--file"]);
        command.arg(saved_filed.as_os_str());

        command.args(["--use-clippy", &use_clippy.to_string()]);

        let files = deserialize_output(command.output(), &command)?;
        Ok(files)
    }

    #[instrument(skip_all)]
    pub fn expand_and_resolve(&self, targets: &[Target]) -> anyhow::Result<ExpandedAndResolved> {
        let mut command = self.command();
        command.args(["--isolation-dir", ".rust-analyzer"]);
        command.arg("bxl");
        command.args(["--oncall", "rust_devx", "-c", "client.id=rust-project"]);
        if let Some(mode) = &self.mode {
            command.arg(mode);
        }
        command.args([
            "prelude//rust/rust-analyzer/resolve_deps.bxl:expand_and_resolve",
            "-c=rust.failure_filter=true",
            "-c=rust.incremental=true",
            "--",
            "--targets",
        ]);
        command.args(targets);
        deserialize_output(command.output(), &command)
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
        command.arg("cquery");
        if let Some(mode) = &self.mode {
            command.arg(mode);
        }
        command.args(["--output-all-attributes", "kind('^alias$', deps(%Ss))"]);
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

        command.args(["uquery", "--json", "owner(\"%s\")", "--"]);
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
                let context = cmd_err(command, status, &stderr);
                Err(err).context(context)
            })
            .context("command returned non-utf8 output"),
        Ok(Output {
            stdout: _,
            stderr,
            status,
        }) => Err(cmd_err(command, status, &stderr))
            .with_context(|| format!("command ended with {}", status)),
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
        }) => {
            tracing::debug!(?command, "parsing command output");
            serde_json::from_slice(&stdout)
                .with_context(|| cmd_err(command, status, &stderr))
                .context("failed to deserialize command output")
        }
        Err(err) => Err(err)
            .with_context(|| format!("command `{:?}`", command))
            .context("failed to execute command"),
    }
}

fn cmd_err(command: &Command, status: ExitStatus, stderr: &[u8]) -> anyhow::Error {
    anyhow::anyhow!(
        "command `{:?}` (exit code: {})\nstderr:\n{}",
        command,
        status,
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

pub fn select_mode(mode: Option<String>) -> Option<String> {
    if let Some(mode) = mode {
        Some(mode)
    } else if cfg!(target_os = "macos") {
        Some("@//mode/mac".to_owned())
    } else if cfg!(target_os = "windows") {
        Some("@//mode/win".to_owned())
    } else {
        // fallback to the platform default mode. This is likely slower than optimal, but
        // `rust-project build` will work.
        None
    }
}

/// When we merge targets with their tests, we shouldn't end up
/// with a target that depends on itself.
#[test]
fn merge_tests_no_cycles() {
    let mut targets = BTreeMap::new();

    targets.insert(
        Target::new("//foo"),
        TargetInfo {
            name: "foo".to_owned(),
            label: "foo".to_owned(),
            kind: Kind::Library,
            edition: None,
            srcs: vec![],
            mapped_srcs: BTreeMap::new(),
            crate_name: None,
            crate_root: None,
            deps: vec![],
            tests: vec![Target::new("//foo-unittest")],
            named_deps: BTreeMap::new(),
            proc_macro: None,
            features: vec![],
            source_folder: PathBuf::from("/tmp"),
        },
    );

    targets.insert(
        Target::new("//foo-unittest"),
        TargetInfo {
            name: "foo-unittest".to_owned(),
            label: "foo-unittest".to_owned(),
            kind: Kind::Test,
            edition: None,
            srcs: vec![],
            mapped_srcs: BTreeMap::new(),
            crate_name: None,
            crate_root: None,
            deps: vec![Target::new("//foo")],
            tests: vec![],
            named_deps: BTreeMap::new(),
            proc_macro: None,
            features: vec![],
            source_folder: PathBuf::from("/tmp"),
        },
    );

    let res = merge_unit_test_targets(targets.clone());
    let merged_target = res.get(&Target::new("//foo")).unwrap();
    assert_eq!(*merged_target.info.deps, vec![]);
}

#[test]
fn merge_target_multiple_tests_no_cycles() {
    let mut targets = BTreeMap::new();

    targets.insert(
        Target::new("//foo"),
        TargetInfo {
            name: "foo".to_owned(),
            label: "foo".to_owned(),
            kind: Kind::Library,
            edition: None,
            srcs: vec![],
            mapped_srcs: BTreeMap::new(),
            crate_name: None,
            crate_root: None,
            deps: vec![Target::new("//foo@rust")],
            tests: vec![
                Target::new("//foo_test"),
                Target::new("//foo@rust-unittest"),
            ],
            named_deps: BTreeMap::new(),
            proc_macro: None,
            features: vec![],
            source_folder: PathBuf::from("/tmp"),
        },
    );

    targets.insert(
        Target::new("//foo@rust"),
        TargetInfo {
            name: "foo@rust".to_owned(),
            label: "foo@rust".to_owned(),
            kind: Kind::Library,
            edition: None,
            srcs: vec![],
            mapped_srcs: BTreeMap::new(),
            crate_name: None,
            crate_root: None,
            deps: vec![],
            tests: vec![
                Target::new("//foo_test"),
                Target::new("//foo@rust-unittest"),
            ],
            named_deps: BTreeMap::new(),
            proc_macro: None,
            features: vec![],
            source_folder: PathBuf::from("/tmp"),
        },
    );

    targets.insert(
        Target::new("//foo_test"),
        TargetInfo {
            name: "foo_test".to_owned(),
            label: "foo_test".to_owned(),
            kind: Kind::Test,
            edition: None,
            srcs: vec![],
            mapped_srcs: BTreeMap::new(),
            crate_name: None,
            crate_root: None,
            // foo_test depends on foo, which is reasonable, but
            // we need to be careful when merging test
            // dependencies of foo@rust to avoid creating cycles.
            deps: vec![Target::new("//foo"), Target::new("//bar")],
            tests: vec![],
            named_deps: BTreeMap::new(),
            proc_macro: None,
            features: vec![],
            source_folder: PathBuf::from("/tmp"),
        },
    );

    targets.insert(
        Target::new("//foo@rust-unittest"),
        TargetInfo {
            name: "foo@rust-unittest".to_owned(),
            label: "foo@rust-unittest".to_owned(),
            kind: Kind::Test,
            edition: None,
            srcs: vec![],
            mapped_srcs: BTreeMap::new(),
            crate_name: None,
            crate_root: None,
            deps: vec![Target::new("//test-framework")],
            tests: vec![],
            named_deps: BTreeMap::new(),
            proc_macro: None,
            features: vec![],
            source_folder: PathBuf::from("/tmp"),
        },
    );

    let res = merge_unit_test_targets(targets.clone());
    let merged_foo_target = res.get(&Target::new("//foo")).unwrap();
    assert_eq!(
        *merged_foo_target.info.deps,
        vec![Target::new("//foo@rust")],
        "Additional dependencies should only come from the foo-unittest crate"
    );

    let merged_foo_rust_target = res.get(&Target::new("//foo@rust")).unwrap();
    assert_eq!(
        *merged_foo_rust_target.info.deps,
        vec![Target::new("//test-framework")],
        "Test dependencies should only come from the foo@rust-unittest crate"
    );
}
