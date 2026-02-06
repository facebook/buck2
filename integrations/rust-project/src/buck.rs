/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::ffi::OsStr;
use std::fs;
use std::io;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;
use std::process::ExitStatus;
use std::process::Output;
use std::process::Stdio;

use anyhow::Context;
use rustc_hash::FxHashMap;
use rustc_hash::FxHashSet;
use serde::Deserialize;
use tracing::Level;
use tracing::enabled;
use tracing::info;
use tracing::instrument;
use tracing::trace;
use tracing::warn;

use crate::Crate;
use crate::Dep;
use crate::cli::Input;
use crate::project_json::Build;
use crate::project_json::Edition;
use crate::project_json::ProjectJson;
use crate::project_json::Runnable;
use crate::project_json::RunnableKind;
use crate::project_json::Source;
use crate::project_json::Sysroot;
use crate::target::AliasedTargetInfo;
use crate::target::ExpandedAndResolved;
use crate::target::Kind;
use crate::target::MacroOutput;
use crate::target::Target;
use crate::target::TargetInfo;

#[cfg(fbcode_build)]
const CLIENT_METADATA_RUST_PROJECT: &str = "--client-metadata=id=rust-project";

pub(crate) fn to_project_json(
    sysroot: Sysroot,
    expanded_and_resolved: ExpandedAndResolved,
    aliases: FxHashMap<Target, AliasedTargetInfo>,
    check_cycles: bool,
    include_all_buildfiles: bool,
    use_clippy: bool,
    extra_cfgs: &[String],
    buck: &Buck,
) -> Result<ProjectJson, anyhow::Error> {
    let project_root = buck.resolve_project_root()?;

    let ExpandedAndResolved {
        expanded_targets: _,
        queried_proc_macros: proc_macros,
        resolved_deps: target_map,
    } = expanded_and_resolved;

    let target_index = merge_unit_test_targets(target_map);

    // A rust-project.json uses file indexes to associate dependencies with the
    // relevant crate.
    //
    // "crates": [
    //   {
    //     "display_name": "my-project",
    //     "deps": [
    //       {
    //         "crate": 1,
    //         "name": "my-lib"
    //       }
    //     ]
    //   },
    //   {
    //     "display_name": "my-lib",
    //     "deps": []
    //   }
    // ]
    //
    // This means that we must iterate over targets in a consistent order, so
    // the indexes are correct. Build an ordered Vec and a corresponding HashMap
    // from target name to index.
    let targets_vec = target_index.keys().cloned().collect::<Vec<Target>>();

    let mut targets_to_ids: FxHashMap<&Target, usize> = FxHashMap::default();
    for (index, target) in targets_vec.iter().enumerate() {
        targets_to_ids.insert(target, index);
    }

    let mut crates: Vec<Crate> = Vec::with_capacity(targets_vec.len());
    for target in &targets_vec {
        let info = target_index.get(target).unwrap();

        let dep_targets = resolve_aliases(&info.deps, &aliases, &proc_macros);
        let deps = as_deps(&dep_targets, info, &targets_to_ids, &target_index);

        let edition = match &info.edition {
            Some(edition) => edition.clone(),
            None => Edition::Edition2021,
        };

        // the mapping here is inverted, which means we need to search through the keys for the Target.
        // thankfully, most projects don't have to many proc macros, which means the size of this list
        // remains in the two digit space.
        let proc_macro_dylib_path = proc_macros
            .values()
            .find(|macro_output| macro_output.actual == *target)
            .map(|macro_output| macro_output.dylib.clone());
        if let Some(ref dylib) = proc_macro_dylib_path {
            trace!(?target, ?dylib, "target is a proc macro");
        }

        // corresponds to the BUCK/TARGETS file of a target.
        let build_file = project_root.join(info.project_relative_buildfile.clone());

        // We don't need to push the source folder as rust-analyzer by default will use the root-module parent().
        // info.root_module() will output either the fbcode source file or the symlinked one based on if it's a mapped source or not
        let root_module = info.root_module(&project_root);

        let mut env = FxHashMap::default();

        // Populate the environment variables the target configuration's environment variables,
        // but ignore OUT_DIR as we handle that later.
        env.extend(info.env.clone().into_iter());

        // If $CARGO_MANIFEST_DIR is set, resolve it to an absolute path.
        if let Some(rel_cargo_manifest_dir) = info.env.get("CARGO_MANIFEST_DIR") {
            let cargo_manifest_dir = info.source_folder.join(rel_cargo_manifest_dir);
            env.insert(
                "CARGO_MANIFEST_DIR".to_owned(),
                cargo_manifest_dir.to_string_lossy().into_owned(),
            );
        }

        let mut include_dirs = vec![];
        if let Some(out_dir) = info.env.get("OUT_DIR") {
            // to ensure that the `OUT_DIR` is included as part of the `PackageRoot` in rust-analyzer,
            // manually insert the parent of the `out_dir` into `include_dirs`.
            if let Some(parent) = Path::new(out_dir).parent() {
                include_dirs.push(parent.to_owned());
            }
        }

        // We want to include all the directories from srcs, because sometimes the source files on
        // disk aren't in the same layout as buck-out. In this situation, the root_module directory
        // isn't sufficient to find all files in the module tree.
        //
        // You could construct a pathological case using mapped_srcs that means we miss
        // directories here, but this covers all the cases I've tested.
        let mut src_dirs: Vec<PathBuf> = info
            .srcs
            .iter()
            .filter_map(|p| p.parent())
            .map(|p| p.to_owned())
            .collect();
        src_dirs.sort();
        include_dirs.extend(src_dirs.into_iter());

        // We want to include the directory that contains the root module. Typically this file
        // will be listed the srcs declaration, but it may be a generated file listed in mapped_srcs.
        if let Some(parent) = root_module.parent() {
            include_dirs.push(parent.to_owned());
        }

        include_dirs = remove_duplicates_preserve_order(include_dirs);

        let build = if include_all_buildfiles || info.in_workspace {
            let build = Build {
                label: target.clone(),
                build_file: build_file.to_owned(),
                target_kind: info.kind.clone().into(),
            };
            Some(build)
        } else {
            None
        };

        let crate_info = Crate {
            display_name: Some(info.display_name()),
            root_module,
            edition,
            deps,
            is_workspace_member: info.in_workspace,
            source: Some(Source {
                include_dirs,
                exclude_dirs: vec![],
            }),
            cfg: info
                .cfg()
                .into_iter()
                .chain(extra_cfgs.iter().cloned())
                .collect(),
            env,
            build,
            is_proc_macro: info.proc_macro.unwrap_or(false),
            proc_macro_dylib_path,
            target: None,
        };
        crates.push(crate_info);
    }

    if check_cycles {
        check_cycles_in_crate_graph(&crates);
    }

    let mut runnables = vec![];

    #[cfg(fbcode_build)]
    {
        runnables.extend([Runnable {
            program: "buck".to_owned(),
            args: vec![
                "test".to_owned(),
                CLIENT_METADATA_RUST_PROJECT.to_owned(),
                "{label}".to_owned(),
                "--".to_owned(),
                "{test_id}".to_owned(),
                "--print-passing-details".to_owned(),
            ],
            cwd: project_root.to_owned(),
            kind: RunnableKind::TestOne,
        }]);
    }

    // OSS `buck2 test` supports different args, chiefly --test-arg
    //
    #[cfg(not(fbcode_build))]
    {
        let rust_project_executable = std::env::args().next().unwrap();
        runnables.extend([
            Runnable {
                kind: RunnableKind::Flycheck,
                program: rust_project_executable,
                args: {
                    let mut args = vec!["check".to_owned(), "{label}".to_owned()];
                    if !use_clippy {
                        args.push("--use-clippy".to_owned());
                        args.push("false".to_owned());
                    }
                    args
                },
                cwd: project_root.clone(),
            },
            Runnable {
                kind: RunnableKind::Run,
                program: "buck2".to_string(),
                args: vec!["run".to_owned(), "{label}".to_string()],
                cwd: project_root.clone(),
            },
            Runnable {
                kind: RunnableKind::TestOne,
                program: "buck2".to_string(),
                args: vec![
                    "test".to_owned(),
                    "-c=client.id=rust-project".to_owned(),
                    "{label}".to_owned(),
                    "--".to_owned(),
                    // R-A substitutes {test_id} with e.g. `mycrate::tests::one`
                    // --test-arg tells `buck2 test` to pass that string through to
                    // the test program, which we will assume to be the rust test
                    // harness. Same overall effect as `cargo test -- mycrate::tests::one`,
                    //
                    // But this is a bit less than ideal, because buck will build
                    // all of the related test binaries, including integration tests.
                    // We don't know your naming conventions for library tests.
                    // You might have a rust_test target named `mycrate-test`. So
                    // we might need a way (probably buck metadata in the library
                    // target, or just querying the related tests)
                    // to tell rust-project about these.
                    "--test-arg".to_owned(),
                    "{test_id}".to_owned(),
                ],
                cwd: project_root.clone(),
            },
        ]);
    }

    let jp = ProjectJson {
        sysroot: Box::new(sysroot),
        crates,
        runnables,
        // needed to ignore the generated `rust-project.json` in diffs, but including the actual
        // string will mark this file as generated
        generated: String::from("\x40generated"),
    };

    Ok(jp)
}

/// Check that there are no cycles in the crate dependency graph: a
/// crate should never transitively depend on itself.
///
/// If a cycle is found, print the offending crate and terminate.
fn check_cycles_in_crate_graph(crates: &[Crate]) {
    // From a start crate ID, each ID we can reach, along with an example route.
    let mut reachable: FxHashMap<usize, FxHashMap<usize, Vec<usize>>> = FxHashMap::default();

    // Initialize the reachable crates from immediate dependencies.
    for (idx, krate) in crates.iter().enumerate() {
        let mut routes: FxHashMap<usize, Vec<usize>> = FxHashMap::default();
        for dep in &krate.deps {
            routes.insert(dep.crate_index, vec![idx, dep.crate_index]);
        }

        reachable.insert(idx, routes);
    }

    let mut changed = true;
    while changed {
        changed = false;

        let mut new_reachable = reachable.clone();

        // Iterate all the dependencies, and add any transitive
        // dependencies that weren't already in `reachable`.
        for (crate_idx, deps_idxs) in reachable.iter() {
            for (dep_idx, route) in deps_idxs.iter() {
                for transitive_dep_idx in reachable[dep_idx].keys() {
                    if transitive_dep_idx == crate_idx {
                        let mut cycle_route = route.clone();
                        cycle_route.push(*transitive_dep_idx);

                        tracing::error!(
                            crate = crates[*crate_idx].display_name,
                            route = format_route(&cycle_route, crates),
                            "Found a cycle",
                        );
                        std::process::exit(2);
                    }

                    if !deps_idxs.contains_key(transitive_dep_idx) {
                        let mut new_route = route.clone();
                        new_route.push(*transitive_dep_idx);

                        new_reachable
                            .get_mut(crate_idx)
                            .expect("We should always have initialized the dependencies for each crate.")
                            .insert(*transitive_dep_idx, new_route);
                        changed = true;
                    }
                }
            }
        }

        reachable = new_reachable;
    }
}

fn format_route(route: &[usize], crates: &[Crate]) -> String {
    let mut formatted_crates = vec![];
    for idx in route {
        formatted_crates.push(format!(
            "{} ({idx})",
            crates[*idx]
                .display_name
                .clone()
                .unwrap_or("<unnamed>".to_owned()),
        ));
    }

    formatted_crates.join(" -> ")
}

/// If any target in `targets` is an alias, resolve it to the actual target.
fn resolve_aliases(
    targets: &[Target],
    aliases: &FxHashMap<Target, AliasedTargetInfo>,
    proc_macros: &FxHashMap<Target, MacroOutput>,
) -> Vec<Target> {
    let mut seen = FxHashSet::default();
    let mut resolved_targets = vec![];

    for target in targets {
        let destination_target = match aliases.get(target) {
            Some(actual) => &actual.actual,
            None => {
                // we fall back to check the proc macros for aliases
                // (these should exist in the aliases map, but they don't. yolo.)
                match proc_macros.get(target) {
                    Some(MacroOutput { actual, .. }) => actual,
                    None => target,
                }
            }
        };

        if !seen.contains(destination_target) {
            resolved_targets.push(destination_target.to_owned());
            seen.insert(destination_target);
        }
    }

    resolved_targets
}

/// Convert `dep_targets` to `Dep` values.
fn as_deps(
    dep_targets: &[Target],
    info: &TargetInfo,
    target_to_ids: &FxHashMap<&Target, usize>,
    target_index: &FxHashMap<Target, TargetInfo>,
) -> Vec<Dep> {
    let overridden_names = info.overridden_dep_names();

    let mut seen_targets = FxHashSet::default();

    let mut deps = vec![];
    for dep_target in dep_targets {
        seen_targets.insert(dep_target);

        let Some(info) = target_index.get(dep_target) else {
            trace!(?dep_target, "not present in target_index");
            continue;
        };

        let crate_index = *target_to_ids.get(dep_target).unwrap();
        let name = match overridden_names.get(dep_target) {
            Some(n) => n.replace('-', "_"),
            None => info.crate_name(),
        };

        deps.push(Dep { crate_index, name });
    }

    for (target, name) in overridden_names.into_iter() {
        if !seen_targets.contains(&target) {
            let Some(crate_index) = target_to_ids.get(&target) else {
                continue;
            };
            deps.push(Dep {
                crate_index: *crate_index,
                name: name.replace('-', "_"),
            });
        }
    }

    deps.sort_by_key(|dep| dep.crate_index);

    deps
}

/// For every test target, drop it from `target_map` and include test
/// target's dependencies in the target that references the test
/// target.
fn merge_unit_test_targets(
    target_map: FxHashMap<Target, TargetInfo>,
) -> FxHashMap<Target, TargetInfo> {
    let mut target_index = FxHashMap::default();

    let (tests, mut targets): (FxHashMap<Target, TargetInfo>, FxHashMap<Target, TargetInfo>) =
        target_map
            .into_iter()
            .partition(|(_, info)| info.kind == Kind::Test);

    let (generated_unit_tests, standalone_tests): (
        FxHashMap<Target, TargetInfo>,
        FxHashMap<Target, TargetInfo>,
    ) = tests.into_iter().partition(|(test_target, _)| {
        test_target.ends_with("-unittest")
            && targets
                .iter()
                .any(|(_, value)| value.test_deps.contains(test_target))
    });

    targets.extend(standalone_tests);

    for (index, (target, mut info)) in targets.into_iter().enumerate() {
        trace!(?target, ?info, index, "adding dependency");

        // Merge the `-unittest` target with the parent target.
        let unittest_target = Target::new(format!("{target}-unittest"));
        if info.test_deps.contains(&unittest_target) {
            if let Some(test_info) = generated_unit_tests.get(&unittest_target) {
                for test_dep in &test_info.deps {
                    if !info.deps.contains(test_dep) && *test_dep != target {
                        info.deps.push(test_dep.clone())
                    }
                }

                info.in_workspace |= test_info.in_workspace;
            }
        }

        target_index.insert(target.to_owned(), info);
    }
    target_index
}

#[derive(Debug, Default)]
pub(crate) struct Buck {
    command: String,
    mode: Option<String>,
}

impl Buck {
    pub(crate) fn new(command: Option<String>, mode: Option<String>) -> Self {
        Buck {
            command: command.unwrap_or_else(|| "buck2".into()),
            mode,
        }
    }

    /// Invoke `buck2` with the given subcommands.
    ///
    /// Care should be taken to ensure that buck is invoked with the same set
    /// options and configuration to avoid invalidating caches.
    fn command<I, S>(&self, subcommands: I) -> Command
    where
        I: IntoIterator<Item = S>,
        S: AsRef<OsStr>,
    {
        let mut cmd = self.command_without_config(subcommands);
        cmd.args([
            CLIENT_METADATA_RUST_PROJECT,
            "-c=rust.rust_project_build=true",
            // Buck owner() queries stop at the innermost BUCK file unless
            // package_boundary_exceptions is set.
            //
            // This is arguably a bug in buck, because it's possible for a parent BUCK
            // file to own a file in a subdirectory that has its own BUCK file.
            //
            // Buck probably didn't intend to allow this pattern: it doesn't work when you
            // use `srcs = glob()`, but it does work for srcs with explicit paths.
            //
            // The intent of package_boundary_exceptions (added to buck2 in D34073360,
            // rolled out in D4339610) was to enforce boundaries with an explicit opt-out
            // list.
            //
            // However, due to the confusion with srcs, we can end up with owner() not
            // finding the target even when the package is not opted-out. Instead, opt-out
            // all packages for this query, so owner() always looks at parent BUCK files
            // and finds the relevant target.
            "-c=project.package_boundary_exceptions=.",
        ]);

        cmd
    }

    /// Invoke `buck` with the given subcommands.
    ///
    /// This method should only be used with with buck commands that do not accept
    /// configuration options, such as `root`. [`Buck::command`] should be preferred.
    fn command_without_config<I, S>(&self, subcommands: I) -> Command
    where
        I: IntoIterator<Item = S>,
        S: AsRef<OsStr>,
    {
        let mut cmd = Command::new(&self.command);

        // rust-analyzer invokes the check-on-save command with `RUST_BACKTRACE=short`
        // set. Unfortunately, buck2 doesn't handle that well and becomes extremely
        // slow when the daemon is started with backtrace variables set. Until that is
        // fixed, just unset them here.
        cmd.env_remove("RUST_BACKTRACE")
            .env_remove("RUST_LIB_BACKTRACE");

        cmd.args(["--isolation-dir", ".rust-analyzer"]);
        cmd.args(subcommands);
        cmd.args(["--oncall", "rust_devx"]);

        cmd
    }

    /// Return the absolute path of the current Buck project root.
    pub(crate) fn resolve_project_root(&self) -> Result<PathBuf, anyhow::Error> {
        let mut command = self.command_without_config(["root"]);
        command.arg("--kind=project");

        let mut stdout = utf8_output(command.output(), &command)?;
        truncate_line_ending(&mut stdout);

        if enabled!(Level::TRACE) {
            trace!(%stdout, "got root from buck");
        }

        Ok(stdout.into())
    }

    pub(crate) fn resolve_sysroot_src(&self) -> Result<PathBuf, anyhow::Error> {
        let mut command = self.command(["audit", "config"]);
        command.args(["--json", "--", "rust.sysroot_src_path"]);
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
        Ok(cfg.sysroot_src_path)
    }

    /// Determines the owning target(s) of the saved file and builds them.
    #[instrument]
    pub(crate) fn check_saved_file(
        &self,
        use_clippy: bool,
        saved_file: &Path,
    ) -> Result<CheckOutput, anyhow::Error> {
        let mut command = self.command(["bxl"]);

        if let Some(mode) = &self.mode {
            command.arg(mode);
        }

        command.arg("prelude//rust/rust-analyzer/check.bxl:check");

        // apply BXL scripts-specific arguments:
        command.arg("--");
        command.args(["--file"]);
        command.arg(saved_file.as_os_str());
        command.args(["--use-clippy", &use_clippy.to_string()]);

        // Set working directory to the containing directory of the target file.
        // This fixes cases where the working directory happens to be an
        // unrelated buck project (e.g. www).
        if let Some(parent_dir) = saved_file.parent() {
            command.current_dir(parent_dir);
        }

        tracing::debug!(?command, "running bxl");

        let output = command.output();

        let files = deserialize_output(output, &command)?;
        Ok(files)
    }

    #[instrument(fields(use_clippy, target = %target))]
    pub(crate) fn check_target(
        &self,
        use_clippy: bool,
        target: &Target,
    ) -> Result<CheckOutput, anyhow::Error> {
        let mut command = self.command(["bxl"]);

        if let Some(mode) = &self.mode {
            command.arg(mode);
        }

        command.arg("prelude//rust/rust-analyzer/check.bxl:check");

        command.arg("--");
        command.arg("--target");
        command.arg(target);
        command.args(["--use-clippy", &use_clippy.to_string()]);

        tracing::debug!(?command, "running bxl");

        let output = command.output();

        let files = deserialize_output(output, &command)?;
        Ok(files)
    }

    #[instrument(skip_all)]
    pub(crate) fn expand_and_resolve(
        &self,
        targets: &[Target],
        exclude_workspaces: bool,
    ) -> anyhow::Result<ExpandedAndResolved> {
        if targets.is_empty() {
            return Ok(ExpandedAndResolved::default());
        }

        let mut command = self.command(["bxl"]);
        if let Some(mode) = &self.mode {
            command.arg(mode);
        }
        command.args([
            "prelude//rust/rust-analyzer/resolve_deps.bxl:resolve_targets",
            "--",
            "--exclude_workspaces",
            exclude_workspaces.to_string().as_str(),
            "--targets",
        ]);
        command.args(targets);

        tracing::debug!(?command, "running bxl");

        deserialize_file_output(command.output(), &command)
    }

    /// Given a list of `targets`, find all the aliases in their transitive dependencies, and
    /// expand all those aliases to the final target.
    pub(crate) fn query_aliased_libraries(
        &self,
        targets: &[Target],
        universe_targets: &[Target],
    ) -> anyhow::Result<FxHashMap<Target, AliasedTargetInfo>> {
        let mut alias_map = self.query_aliased_dependencies(targets, universe_targets)?;

        info!("resolving aliased targets");
        // Recursively expand aliases until we find a target that isn't an alias, or
        // we've queried buck 5 times.
        for _ in 0..5 {
            let alias_destinations = alias_map
                .values()
                .map(|info| info.actual.clone())
                .collect::<Vec<_>>();

            let new_aliases =
                match self.query_aliased_targets(&alias_destinations, &universe_targets) {
                    Ok(new_aliases) => new_aliases,
                    Err(_) => {
                        warn!("buck cquery failed, falling back to best-effort uquery");
                        self.query_aliased_targets_lossy(&alias_destinations)
                    }
                };

            if new_aliases.is_empty() {
                break;
            }

            for destination in alias_map.values_mut() {
                if let Some(new_destination) = new_aliases.get(&destination.actual) {
                    destination.actual = new_destination.actual.clone();
                }
            }
        }

        Ok(alias_map)
    }

    /// Given a list of targets, for all targets that are aliases, return the targets
    /// that the aliases point to.
    ///
    /// Note that the pointed-to targets might themselves be aliases as well.
    fn query_aliased_targets(
        &self,
        targets: &[Target],
        universe_targets: &[Target],
    ) -> anyhow::Result<FxHashMap<Target, AliasedTargetInfo>> {
        let mut command = self.command(["cquery"]);

        if let Some(mode) = &self.mode {
            command.arg(mode);
        }
        command.args(["--output-attribute", "actual", "kind('^alias$', %Ss)"]);
        command.args(targets);

        let universe_arg = universe_targets
            .iter()
            .map(|t| format!("{t}"))
            .collect::<Vec<_>>()
            .join(",");
        command.args(["--target-universe", &universe_arg]);

        deserialize_output(command.output(), &command)
    }

    fn query_aliased_targets_lossy(
        &self,
        targets: &[Target],
    ) -> FxHashMap<Target, AliasedTargetInfo> {
        let mut command = self.command(["uquery"]);

        if let Some(mode) = &self.mode {
            command.arg(mode);
        }
        command.args(["--output-attribute", "actual", "kind('^alias$', %Ss)"]);
        command.args(targets);

        let Ok(output) = command.output() else {
            warn!("Buck uquery failed");
            return FxHashMap::default();
        };

        let Ok(v) = serde_json::from_slice::<serde_json::Value>(&output.stdout) else {
            warn!("Failed to parse buck uquery output");
            return FxHashMap::default();
        };
        deserialize_uquery_alias_info(v)
    }

    /// Return a mapping for all the transitive dependencies of `targets` that are aliases.
    fn query_aliased_dependencies(
        &self,
        targets: &[Target],
        universe_targets: &[Target],
    ) -> Result<FxHashMap<Target, AliasedTargetInfo>, anyhow::Error> {
        // FIXME: Do this in bxl as well instead of manually writing a separate query

        let mut command = self.command(["cquery"]);

        // Fetch all aliases used by transitive deps. This is so we
        // can translate an apparent dependency of e.g.
        // fbsource//third-party/rust:once_cell to the actual target
        // name of fbsource//third-party/rust:once_cell-1.15. This
        // query also fetches non-Rust aliases, but they shouldn't
        // hurt anything.
        if let Some(mode) = &self.mode {
            command.arg(mode);
        }
        command.args(["--output-attribute", "actual", "kind('^alias$', deps(%Ss))"]);
        command.args(targets);

        let universe_arg = universe_targets
            .iter()
            .map(|t| format!("{t}"))
            .collect::<Vec<_>>()
            .join(",");
        command.args(["--target-universe", &universe_arg]);

        info!("resolving aliased dependencies");
        let raw: FxHashMap<Target, AliasedTargetInfo> =
            deserialize_output(command.output(), &command)?;

        if enabled!(Level::TRACE) {
            for (target, info) in &raw {
                trace!(%target, ?info, "parsed target from buck");
            }
        }
        Ok(raw)
    }

    /// Find the buildfile that owns each file specified, and return the path to
    /// each buildfile along with all the targets it contains.
    #[instrument(skip_all)]
    pub(crate) fn query_owners(
        &self,
        input: Input,
        max_extra_targets: usize,
    ) -> Result<FxHashMap<PathBuf, Vec<Target>>, anyhow::Error> {
        let mut command = self.command(["bxl"]);

        command.args([
            "prelude//rust/rust-analyzer/resolve_deps.bxl:resolve_owning_buildfile",
            "--",
        ]);

        info!(kind = "progress", ?input, "finding relevant buck targets");

        match input {
            Input::Targets(targets) => {
                command.arg("--targets");
                command.args(targets);
            }
            Input::Files(files) => {
                command.arg("--files");
                command.args(files);
            }
            Input::Buildfile(files) => {
                command.arg("--buildfiles");
                command.args(files);
            }
        };

        command.arg("--max_extra_targets");
        command.arg(max_extra_targets.to_string());

        let out: FxHashMap<PathBuf, Vec<Target>> = deserialize_output(command.output(), &command)?;

        for (k, v) in out.iter() {
            info!("Found {} with {} targets", k.display(), v.len());
        }

        Ok(out)
    }
}

#[derive(Debug, Deserialize)]
pub(crate) struct CheckOutput {
    pub(crate) diagnostic_paths: Vec<PathBuf>,
    pub(crate) project_root: PathBuf,
}

pub(crate) fn utf8_output(
    output: io::Result<Output>,
    command: &Command,
) -> Result<String, anyhow::Error> {
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
            .with_context(|| format!("command ended with {status}")),
        Err(err) => Err(err)
            .with_context(|| format!("command `{command:?}`"))
            .context("failed to execute command"),
    }
}

fn deserialize_output<T>(output: io::Result<Output>, command: &Command) -> Result<T, anyhow::Error>
where
    T: for<'a> Deserialize<'a>,
{
    match output {
        Ok(Output {
            stdout,
            stderr,
            status,
        }) => {
            // If we have a non-zero exit code due to a compiler crash (a rustc
            // ICE), then we should exit with a non-zero exit code. If the
            // compiler is crashing, we may not be showing all diagnostics, so
            // the user should know something is wrong.
            //
            // Ignore non-zero exit codes otherwise. It's possible to configure
            // a build to fail on warnings, such that we get well-formed JSON of
            // the rustc diagnostics but the exit code is non-zero (D46666035).
            if !status.success() {
                let stderr_str = String::from_utf8_lossy(&stderr);
                if stderr_str.contains("error: the compiler unexpectedly panicked") {
                    return Err(anyhow::anyhow!("{}", stderr_str));
                }
            }

            tracing::debug!(?command, "parsing command output");
            deserialize_json_bytes(&stdout)
                .with_context(|| cmd_err(command, status, &stderr))
                .context("failed to deserialize command output")
        }
        Err(err) => Err(err)
            .with_context(|| format!("command `{command:?}`"))
            .context("failed to execute command"),
    }
}

fn deserialize_file_output<T>(
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
            tracing::debug!(?command, "parsing file output");
            serde_json_from_stdout_path(&stdout)
                .with_context(|| cmd_err(command, status, &stderr))
                .context("failed to deserialize command output")
        }
        Err(err) => Err(err)
            .with_context(|| format!("command `{command:?}`"))
            .context("failed to execute command"),
    }
}

fn serde_json_from_stdout_path<T>(stdout: &[u8]) -> Result<T, anyhow::Error>
where
    T: for<'a> Deserialize<'a>,
{
    let file_path = std::str::from_utf8(stdout)?;
    let file_path = Path::new(file_path.lines().next().context("no file path in output")?);
    let contents =
        fs::read_to_string(file_path).with_context(|| format!("failed to read {file_path:?}"))?;
    deserialize_json_str(&contents).context("failed to deserialize file")
}

fn cmd_err(command: &Command, status: ExitStatus, stderr: &[u8]) -> anyhow::Error {
    anyhow::anyhow!(
        "command `{:?}` (exit code: {})\nstderr:\n{}",
        command,
        status,
        String::from_utf8_lossy(stderr),
    )
}

/// Deserialize bytes with serde_path_to_error so error messages report
/// the offending fields.
fn deserialize_json_bytes<T>(bytes: &[u8]) -> Result<T, anyhow::Error>
where
    T: for<'a> Deserialize<'a>,
{
    let mut deserializer = serde_json::Deserializer::from_slice(bytes);
    serde_path_to_error::deserialize(&mut deserializer).map_err(|err| {
        anyhow::anyhow!(
            "JSON parse error in object '{}': {}",
            err.path(),
            err.inner()
        )
    })
}

/// Deserialize string with serde_path_to_error so error messages report
/// the offending fields.
fn deserialize_json_str<T>(s: &str) -> Result<T, anyhow::Error>
where
    T: for<'a> Deserialize<'a>,
{
    let mut deserializer = serde_json::Deserializer::from_str(s);
    serde_path_to_error::deserialize(&mut deserializer).map_err(|err| {
        anyhow::anyhow!(
            "JSON parse error in object '{}': {}",
            err.path(),
            err.inner()
        )
    })
}

/// Trim a trailing new line from `String`.
/// Useful when trimming command output.
pub(crate) fn truncate_line_ending(s: &mut String) {
    if let Some(x) = s.strip_suffix("\r\n").or_else(|| s.strip_suffix('\n')) {
        s.truncate(x.len());
    }
}

pub(crate) fn select_mode(mode: Option<&str>) -> Option<String> {
    if let Some(mode) = mode {
        Some(mode.to_owned())
    } else if cfg!(all(fbcode_build, target_os = "windows")) {
        Some("@fbcode//mode/win".to_owned())
    } else {
        // fallback to the platform default mode. This is likely slower than optimal, but
        // `rust-project check` will work.
        None
    }
}

fn remove_duplicates_preserve_order(paths: Vec<PathBuf>) -> Vec<PathBuf> {
    let mut uniq_paths = Vec::with_capacity(paths.len());
    let mut seen = FxHashSet::default();

    for path in paths {
        if seen.insert(path.clone()) {
            uniq_paths.push(path);
        }
    }

    uniq_paths
}

fn deserialize_uquery_alias_info(v: serde_json::Value) -> FxHashMap<Target, AliasedTargetInfo> {
    let serde_json::Value::Object(v_items) = v else {
        return FxHashMap::default();
    };

    let mut map = FxHashMap::default();
    for (target_str, alias_info) in v_items {
        let Some(actual) = alias_info.get("actual") else {
            continue;
        };
        let Some(actual_str) = unwrap_selector_best_effort(actual) else {
            continue;
        };
        let actual = Target::new(actual_str);
        map.insert(Target::new(target_str), AliasedTargetInfo { actual });
    }

    map
}

/// If this JSON value is a plain string, return it. If it's a selector, try to guess
/// the best value to return.
fn unwrap_selector_best_effort(v: &serde_json::Value) -> Option<String> {
    match v {
        serde_json::Value::String(s) => Some(s.clone()),
        serde_json::Value::Object(map) => {
            let map_type = map.get("__type")?;
            if map_type != "selector" {
                return None;
            }

            let entries = map.get("entries")?;
            let serde_json::Value::Object(entries_map) = entries else {
                return None;
            };

            // If the selector has a DEFAULT value, use that.
            //
            // {
            //   "__type": "selector",
            //   "entries": {
            //     "DEFAULT": "some-value",
            //     "platform-specific": "other-value"
            //   }
            // }
            if let Some(v) = entries_map.get("DEFAULT") {
                return unwrap_selector_best_effort(v);
            };

            // Otherwise, arbitrarily pick the first value.
            if let Some(v) = entries_map.values().next() {
                return unwrap_selector_best_effort(v);
            };

            None
        }
        _ => None,
    }
}

/// When we merge targets with their tests, we shouldn't end up
/// with a target that depends on itself.
#[test]
fn merge_tests_no_cycles() {
    let mut targets = FxHashMap::default();

    targets.insert(
        Target::new("//foo"),
        TargetInfo {
            name: "foo".to_owned(),
            label: "foo".to_owned(),
            kind: Kind::Library,
            edition: None,
            srcs: vec![],
            mapped_srcs: FxHashMap::default(),
            crate_name: None,
            crate_dynamic: None,
            crate_root: PathBuf::default(),
            deps: vec![],
            test_deps: vec![Target::new("//foo-unittest")],
            named_deps: FxHashMap::default(),
            proc_macro: None,
            features: vec![],
            env: FxHashMap::default(),
            source_folder: PathBuf::from("/tmp"),
            project_relative_buildfile: PathBuf::from("foo/BUCK"),
            in_workspace: false,
            rustc_flags: vec![],
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
            mapped_srcs: FxHashMap::default(),
            crate_name: None,
            crate_dynamic: None,
            crate_root: PathBuf::default(),
            deps: vec![Target::new("//foo")],
            test_deps: vec![],
            named_deps: FxHashMap::default(),
            proc_macro: None,
            features: vec![],
            env: FxHashMap::default(),
            source_folder: PathBuf::from("/tmp"),
            project_relative_buildfile: PathBuf::from("foo-unittest/BUCK"),
            in_workspace: false,
            rustc_flags: vec![],
        },
    );

    let res = merge_unit_test_targets(targets.clone());
    let merged_target = res.get(&Target::new("//foo")).unwrap();
    assert_eq!(*merged_target.deps, vec![]);
}

#[test]
fn merge_target_multiple_tests_no_cycles() {
    let mut targets = FxHashMap::default();

    targets.insert(
        Target::new("//foo"),
        TargetInfo {
            name: "foo".to_owned(),
            label: "foo".to_owned(),
            kind: Kind::Library,
            edition: None,
            srcs: vec![],
            mapped_srcs: FxHashMap::default(),
            crate_name: None,
            crate_dynamic: None,
            crate_root: PathBuf::default(),
            deps: vec![Target::new("//foo@rust")],
            test_deps: vec![
                Target::new("//foo_test"),
                Target::new("//foo@rust-unittest"),
            ],
            named_deps: FxHashMap::default(),
            proc_macro: None,
            features: vec![],
            env: FxHashMap::default(),
            source_folder: PathBuf::from("/tmp"),
            project_relative_buildfile: PathBuf::from("foo/BUCK"),
            in_workspace: false,
            rustc_flags: vec![],
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
            mapped_srcs: FxHashMap::default(),
            crate_name: None,
            crate_dynamic: None,
            crate_root: PathBuf::default(),
            deps: vec![],
            test_deps: vec![
                Target::new("//foo_test"),
                Target::new("//foo@rust-unittest"),
            ],
            named_deps: FxHashMap::default(),
            proc_macro: None,
            features: vec![],
            env: FxHashMap::default(),
            source_folder: PathBuf::from("/tmp"),
            project_relative_buildfile: PathBuf::from("foo/BUCK"),
            in_workspace: false,
            rustc_flags: vec![],
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
            mapped_srcs: FxHashMap::default(),
            crate_name: None,
            crate_dynamic: None,
            crate_root: PathBuf::default(),
            // foo_test depends on foo, which is reasonable, but
            // we need to be careful when merging test
            // dependencies of foo@rust to avoid creating cycles.
            deps: vec![Target::new("//foo"), Target::new("//bar")],
            test_deps: vec![],
            named_deps: FxHashMap::default(),
            proc_macro: None,
            features: vec![],
            env: FxHashMap::default(),
            source_folder: PathBuf::from("/tmp"),
            project_relative_buildfile: PathBuf::from("foo_test/BUCK"),
            in_workspace: false,
            rustc_flags: vec![],
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
            mapped_srcs: FxHashMap::default(),
            crate_name: None,
            crate_dynamic: None,
            crate_root: PathBuf::default(),
            deps: vec![Target::new("//test-framework")],
            test_deps: vec![],
            named_deps: FxHashMap::default(),
            proc_macro: None,
            features: vec![],
            env: FxHashMap::default(),
            source_folder: PathBuf::from("/tmp"),
            project_relative_buildfile: PathBuf::from("foo/BUCK"),
            in_workspace: false,
            rustc_flags: vec![],
        },
    );

    let res = merge_unit_test_targets(targets.clone());
    let merged_foo_target = res.get(&Target::new("//foo")).unwrap();
    assert_eq!(
        *merged_foo_target.deps,
        vec![Target::new("//foo@rust")],
        "Additional dependencies should only come from the foo-unittest crate"
    );

    let merged_foo_rust_target = res.get(&Target::new("//foo@rust")).unwrap();
    assert_eq!(
        *merged_foo_rust_target.deps,
        vec![Target::new("//test-framework")],
        "Test dependencies should only come from the foo@rust-unittest crate"
    );
}

#[test]
fn integration_tests_preserved() {
    let mut targets = FxHashMap::default();

    targets.insert(
        Target::new("//foo"),
        TargetInfo {
            name: "foo".to_owned(),
            label: "foo".to_owned(),
            kind: Kind::Library,
            edition: None,
            srcs: vec![],
            mapped_srcs: FxHashMap::default(),
            crate_name: None,
            crate_dynamic: None,
            crate_root: PathBuf::default(),
            deps: vec![],
            test_deps: vec![Target::new("//foo-integration-test")],
            named_deps: FxHashMap::default(),
            proc_macro: None,
            features: vec![],
            env: FxHashMap::default(),
            source_folder: PathBuf::from("/tmp"),
            project_relative_buildfile: PathBuf::from("foo/BUCK"),
            in_workspace: false,
            rustc_flags: vec![],
        },
    );

    targets.insert(
        Target::new("//foo-integration-test"),
        TargetInfo {
            name: "foo-integration-test".to_owned(),
            label: "foo-integration-test".to_owned(),
            kind: Kind::Test,
            edition: None,
            srcs: vec![],
            mapped_srcs: FxHashMap::default(),
            crate_name: None,
            crate_dynamic: None,
            crate_root: PathBuf::default(),
            deps: vec![Target::new("//foo")],
            test_deps: vec![],
            named_deps: FxHashMap::default(),
            proc_macro: None,
            features: vec![],
            env: FxHashMap::default(),
            source_folder: PathBuf::from("/tmp"),
            project_relative_buildfile: PathBuf::from("foo/BUCK"),
            in_workspace: false,
            rustc_flags: vec![],
        },
    );

    let res = merge_unit_test_targets(targets.clone());
    assert!(res.contains_key(&Target::new("//foo-integration-test")));
}

#[test]
fn named_deps_underscores() {
    let mut target_index = FxHashMap::default();
    target_index.insert(
        Target::new("//bar"),
        TargetInfo {
            name: "bar".to_owned(),
            label: "bar".to_owned(),
            kind: Kind::Library,
            edition: None,
            srcs: vec![],
            mapped_srcs: FxHashMap::default(),
            crate_name: None,
            crate_dynamic: None,
            crate_root: PathBuf::default(),
            deps: vec![],
            test_deps: vec![],
            named_deps: FxHashMap::default(),
            proc_macro: None,
            features: vec![],
            env: FxHashMap::default(),
            source_folder: PathBuf::from("/tmp"),
            project_relative_buildfile: PathBuf::from("bar/BUCK"),
            in_workspace: false,
            rustc_flags: vec![],
        },
    );

    let mut named_deps = FxHashMap::default();
    named_deps.insert("bar-baz".to_owned(), Target::new("//bar"));

    let info = TargetInfo {
        name: "foo".to_owned(),
        label: "foo".to_owned(),
        kind: Kind::Library,
        edition: None,
        srcs: vec![],
        mapped_srcs: FxHashMap::default(),
        crate_name: None,
        crate_dynamic: None,
        crate_root: PathBuf::default(),
        deps: vec![],
        test_deps: vec![],
        named_deps,
        proc_macro: None,
        features: vec![],
        env: FxHashMap::default(),
        source_folder: PathBuf::from("/tmp"),
        project_relative_buildfile: PathBuf::from("foo/BUCK"),
        in_workspace: false,
        rustc_flags: vec![],
    };

    let mut targets_to_ids = FxHashMap::default();
    let bar_target = Target::new("//bar");
    targets_to_ids.insert(&bar_target, 0);

    let dep_targets = resolve_aliases(&info.deps, &FxHashMap::default(), &FxHashMap::default());
    let deps = as_deps(&dep_targets, &info, &targets_to_ids, &target_index);

    assert_eq!(
        deps,
        vec![Dep {
            crate_index: 0,
            name: "bar_baz".to_owned()
        }]
    );
}

#[test]
fn alias_of_existing_target() {
    let targets = vec![
        Target::new("//foo"),
        Target::new("//foo-alias"),
        Target::new("//bar"),
    ];

    let mut aliases = FxHashMap::default();
    aliases.insert(
        Target::new("//foo-alias"),
        AliasedTargetInfo {
            actual: Target::new("//foo"),
        },
    );

    let dep_targets = resolve_aliases(&targets, &aliases, &FxHashMap::default());

    assert_eq!(
        dep_targets,
        vec![Target::new("//foo"), Target::new("//bar"),]
    );
}

#[test]
fn test_select_mode() {
    // Test default behavior without the fbcode_build cfg
    if cfg!(not(fbcode_build)) {
        assert_eq!(select_mode(None), None);
        assert_eq!(
            select_mode(Some("custom-mode")),
            Some("custom-mode".to_owned())
        );
    }

    if cfg!(all(fbcode_build, target_os = "windows")) {
        assert_eq!(select_mode(None), Some("@fbcode//mode/win".to_owned()));
        assert_eq!(
            select_mode(Some("custom-mode")),
            Some("custom-mode".to_owned())
        );
    }

    if cfg!(all(fbcode_build, not(target_os = "windows"))) {
        assert_eq!(select_mode(None), None);
        assert_eq!(
            select_mode(Some("custom-mode")),
            Some("custom-mode".to_owned())
        );
    }
}
