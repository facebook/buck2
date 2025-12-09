/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fs;
use std::os::fd::OwnedFd;
use std::os::unix::process::CommandExt;
use std::path::PathBuf;
use std::process::Command;
use std::sync::Arc;

use buck2_fs::paths::file_name::FileName;
use dupe::Dupe;
use nix::fcntl::OFlag;
use nix::sys::stat::Mode;

use crate::cgroup_files::CgroupFile;
use crate::cgroup_files::MemoryStat;
use crate::path::CgroupPath;
use crate::path::CgroupPathBuf;

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Environment)]
enum CgroupError {
    #[error("{msg} IO error: {io_err}")]
    Io { msg: String, io_err: std::io::Error },
    #[error("Failed to configure cgroup at {path}, because of error {io_err}")]
    ConfigurationFailed {
        path: String,
        io_err: std::io::Error,
    },
}

/// Resource constraints inherited from ancestor cgroups in the hierarchy.
///
/// For the various resource constraints that can be imposed on a cgroup, this struct represents the
/// effective limit that a particular cgroup sees from the combination of its parents in aggregate.
///
/// Note that in containerized environments, we may not be able to see the limits imposed on us.
#[derive(Debug, Clone, Copy, Default)]
pub(crate) struct EffectiveResourceConstraints {
    pub memory_max: Option<u64>,
    pub memory_high: Option<u64>,
    pub memory_swap_max: Option<u64>,
    pub memory_swap_high: Option<u64>,
}

/// A list of enabled controllers.
///
/// Each one is formatted as `+controller`, as that's a bit easier to use in practice.
#[derive(Dupe, Clone)]
pub(crate) struct EnabledControllers(Arc<[String]>);

pub trait MemoryMonitoring {}

pub struct NoMemoryMonitoring;

impl MemoryMonitoring for NoMemoryMonitoring {}

pub struct WithMemoryMonitoring {
    memory_stat: CgroupFile,
}

impl MemoryMonitoring for WithMemoryMonitoring {}

/// The kind of node in the cgroup tree this cgroup is.
///
/// Cgroups come in two flavors: Internal cgroups, which are those that can have child groups but
/// cannot contain processes, and leaf cgroups, for which the opposite is the case.
pub trait CgroupKind {}

pub struct CgroupKindInternal {
    controllers: EnabledControllers,
}

impl CgroupKind for CgroupKindInternal {}

pub struct CgroupKindLeaf {
    // `cgroup.procs`
    procs: Arc<CgroupFile>,
}

impl CgroupKind for CgroupKindLeaf {}

/// Just after creating a cgroup we may not yet know what kind it is.
pub struct CgroupKindUndecided;

impl CgroupKind for CgroupKindUndecided {}

/// Handle to a cgroup
///
/// Generic over whether we are monitoring the memory use of the cgroup; having memory monitoring on
/// is relatively cheap but does require holding open a couple extra FDs.
pub struct Cgroup<M: MemoryMonitoring, K: CgroupKind> {
    /// Store the dirfd, not the more standard `DIR*`, because this one is thread safe
    dir: OwnedFd,
    path: CgroupPathBuf,
    memory: M,
    kind: K,
}

pub type CgroupMinimal = Cgroup<NoMemoryMonitoring, CgroupKindUndecided>;

pub type CgroupLeaf = Cgroup<WithMemoryMonitoring, CgroupKindLeaf>;

pub type CgroupInternal = Cgroup<WithMemoryMonitoring, CgroupKindInternal>;

impl<M: MemoryMonitoring, K: CgroupKind> Cgroup<M, K> {
    pub fn path(&self) -> &CgroupPath {
        &self.path
    }

    pub(crate) fn read_enabled_controllers(&self) -> buck2_error::Result<EnabledControllers> {
        let controllers_file = CgroupFile::open(
            &self.dir,
            FileName::unchecked_new("cgroup.controllers"),
            false,
        )?;
        let controllers = controllers_file.read_to_string()?;
        Ok(EnabledControllers(
            controllers
                .split_whitespace()
                .map(|s| format!("+{}", s))
                .collect(),
        ))
    }

    fn memory_high_path(&self) -> PathBuf {
        self.path().as_path().join("memory.high")
    }

    /// Set the memory.high limit for this cgroup
    pub fn set_memory_high(&self, memory_high: &str) -> buck2_error::Result<()> {
        let memory_high_file_path = self.memory_high_path();
        fs::write(&memory_high_file_path, memory_high).map_err(|e| {
            CgroupError::ConfigurationFailed {
                path: memory_high_file_path.to_string_lossy().to_string(),
                io_err: e,
            }
        })?;
        Ok(())
    }

    fn read_resource_constraints(&self) -> buck2_error::Result<EffectiveResourceConstraints> {
        let memory_high =
            CgroupFile::open(&self.dir, FileName::unchecked_new("memory.high"), false)?
                .read_max_or_int()?;
        let memory_max = CgroupFile::open(&self.dir, FileName::unchecked_new("memory.max"), false)?
            .read_max_or_int()?;
        let memory_swap_high = CgroupFile::open(
            &self.dir,
            FileName::unchecked_new("memory.swap.high"),
            false,
        )?
        .read_max_or_int()?;
        let memory_swap_max =
            CgroupFile::open(&self.dir, FileName::unchecked_new("memory.swap.max"), false)?
                .read_max_or_int()?;
        Ok(EffectiveResourceConstraints {
            memory_high,
            memory_max,
            memory_swap_high,
            memory_swap_max,
        })
    }

    pub(crate) fn read_effective_resouce_constraints(
        &self,
    ) -> buck2_error::Result<EffectiveResourceConstraints> {
        let Some(parent_path) = self.path.parent() else {
            // The root cgroup doesn't have memory restrictions (the files don't even exist)
            return Ok(EffectiveResourceConstraints::default());
        };
        let parent = CgroupMinimal::try_from_path(parent_path.to_buf())?
            .read_effective_resouce_constraints()?;
        let me = self.read_resource_constraints()?;

        fn min_options(a: Option<u64>, b: Option<u64>) -> Option<u64> {
            a.into_iter().chain(b).reduce(std::cmp::min)
        }

        Ok(EffectiveResourceConstraints {
            memory_max: min_options(parent.memory_max, me.memory_max),
            memory_high: min_options(parent.memory_high, me.memory_high),
            memory_swap_max: min_options(parent.memory_swap_max, me.memory_swap_max),
            memory_swap_high: min_options(parent.memory_swap_high, me.memory_swap_high),
        })
    }
}

impl Cgroup<NoMemoryMonitoring, CgroupKindUndecided> {
    pub fn try_from_path(path: CgroupPathBuf) -> buck2_error::Result<Self> {
        let dir = nix::fcntl::open(
            path.as_path(),
            OFlag::O_CLOEXEC | OFlag::O_DIRECTORY,
            Mode::empty(),
        )
        .map_err(|e| CgroupError::Io {
            msg: format!("Failed to open cgroup directory: {path:?}"),
            io_err: e.into(),
        })?;

        let cgroup = Self {
            path,
            dir,
            memory: NoMemoryMonitoring,
            kind: CgroupKindUndecided,
        };

        Ok(cgroup)
    }

    /// Treat this cgroup as a leaf cgroup
    pub fn into_leaf(self) -> buck2_error::Result<Cgroup<NoMemoryMonitoring, CgroupKindLeaf>> {
        Ok(Cgroup {
            kind: CgroupKindLeaf {
                procs: Arc::new(CgroupFile::open(
                    &self.dir,
                    FileName::unchecked_new("cgroup.procs"),
                    true,
                )?),
            },
            dir: self.dir,
            path: self.path,
            memory: self.memory,
        })
    }

    /// Enable subtree controllers on this cgroup as specified and convert to an internal cgroup
    pub(crate) fn enable_subtree_control_and_into_internal(
        self,
        enabled_controllers: EnabledControllers,
    ) -> buck2_error::Result<Cgroup<NoMemoryMonitoring, CgroupKindInternal>> {
        let subtree_control = CgroupFile::open(
            &self.dir,
            FileName::unchecked_new("cgroup.subtree_control"),
            true,
        )?;
        for controller in &*enabled_controllers.0 {
            subtree_control.write(controller.as_bytes())?;
        }

        Ok(Cgroup {
            dir: self.dir,
            path: self.path,
            memory: self.memory,
            kind: CgroupKindInternal {
                controllers: enabled_controllers,
            },
        })
    }
}

impl<M: MemoryMonitoring> Cgroup<M, CgroupKindLeaf> {
    /// Configures a Command to run in this cgroup using pre_exec.
    ///
    /// Uses pre_exec hook which runs in the child process after fork() but before exec().
    /// This ensures the process is migrated to the cgroup before it starts executing.
    ///
    /// Parent process: Command new() -> ... -> spwan()
    ///                  ↓
    /// System: fork() creates child process
    ///                  ↓
    /// Child process: pre_exec() closure runs (cgroup migration)
    ///                  ↓
    /// Child process: exec() run the command
    pub fn setup_command(&self, command: &mut Command) -> buck2_error::Result<()> {
        // NOTE: We need to make sure that this fd is kept alive until the point where the command
        // is spawned/forked. The lifetimes on this type don't prevent the user from dropping the
        // `Cgroup` before that happens.
        //
        // This is accomplished by capturing the `Arc<CgroupFile>` into the `pre_exec` closure. The
        // closure is not dropped until the surrounding command is dropped, at which point the spawn
        // must have happened.
        //
        // Some testing shows that the closure is not dropped in the child process at all; that's
        // good, as we don't actually want to do the `free` that may be implicated by dropping the
        // `Arc`
        let procs = self.kind.procs.dupe();

        // 0 means current process
        let pre_exec = move || procs.write(b"0");
        // Safety: The unsafe block is required for pre_exec which is inherently unsafe due to fork/exec restrictions.
        // However, it's safe here because:
        // 1. We only call async-signal-safe functions (write to file)
        // 2. No memory allocation or complex operations that could deadlock
        unsafe {
            command.pre_exec(pre_exec);
        }
        Ok(())
    }

    pub fn add_process(&self, pid: u32) -> buck2_error::Result<()> {
        let pid = pid.to_string();
        Ok(self.kind.procs.write(pid.as_bytes())?)
    }
}

impl<M: MemoryMonitoring, K: CgroupKind> Cgroup<M, K> {
    /// Make a child cgroup
    ///
    /// Use strongly discouraged; almost always use `make_internal_child` or `make_leaf_child` instead
    pub(crate) fn discouraged_make_child(
        &self,
        child: &FileName,
    ) -> buck2_error::Result<CgroupMinimal> {
        nix::sys::stat::mkdirat(&self.dir, child.as_str(), Mode::all())?;

        let fd = nix::fcntl::openat(
            &self.dir,
            child.as_str(),
            OFlag::O_CLOEXEC | OFlag::O_DIRECTORY,
            Mode::empty(),
        )?;

        Ok(CgroupMinimal {
            dir: fd,
            path: self.path.join(child.as_forward_rel_path()),
            memory: NoMemoryMonitoring,
            kind: CgroupKindUndecided,
        })
    }
}

impl<M: MemoryMonitoring> Cgroup<M, CgroupKindInternal> {
    pub(crate) fn make_internal_child(
        &self,
        child: &FileName,
    ) -> buck2_error::Result<Cgroup<NoMemoryMonitoring, CgroupKindInternal>> {
        let c = self.discouraged_make_child(child)?;
        c.enable_subtree_control_and_into_internal(self.kind.controllers.dupe())
    }

    pub(crate) fn make_leaf_child(
        &self,
        child: &FileName,
    ) -> buck2_error::Result<Cgroup<NoMemoryMonitoring, CgroupKindLeaf>> {
        let c = self.discouraged_make_child(child)?;
        c.into_leaf()
    }
}

impl<K: CgroupKind> Cgroup<NoMemoryMonitoring, K> {
    pub(crate) fn enable_memory_monitoring(
        self,
    ) -> buck2_error::Result<Cgroup<WithMemoryMonitoring, K>> {
        Ok(Cgroup {
            memory: WithMemoryMonitoring {
                memory_stat: CgroupFile::open(
                    &self.dir,
                    FileName::unchecked_new("memory.stat"),
                    false,
                )?,
            },
            dir: self.dir,
            path: self.path,
            kind: self.kind,
        })
    }
}

impl<K: CgroupKind> Cgroup<WithMemoryMonitoring, K> {
    pub fn read_memory_stat(&self) -> buck2_error::Result<MemoryStat> {
        self.memory.memory_stat.read_memory_stat()
    }
}

pub struct CgroupFreezeGuard {
    file: CgroupFile,
}

impl Drop for CgroupFreezeGuard {
    fn drop(&mut self) {
        drop(self.file.write(b"0"));
    }
}

impl<M: MemoryMonitoring, K: CgroupKind> Cgroup<M, K> {
    pub fn freeze(&self) -> buck2_error::Result<CgroupFreezeGuard> {
        let f = CgroupFile::open(&self.dir, FileName::unchecked_new("cgroup.freeze"), true)?;
        f.write(b"1")?;
        Ok(CgroupFreezeGuard { file: f })
    }
}

#[cfg(test)]
impl CgroupMinimal {
    pub(crate) fn create_minimal_for_test() -> Option<Self> {
        use buck2_fs::paths::abs_norm_path::AbsNormPath;
        use buck2_util::process::background_command;

        if !cfg!(buck_build) || !cfg!(target_os = "linux") {
            return None;
        }

        let prep_script = std::env::var("PREP_CGROUP_SCRIPT").unwrap();
        let path = background_command(&prep_script)
            .stdout(std::process::Stdio::piped())
            .output()
            .unwrap()
            .stdout;
        let path = String::from_utf8(path).unwrap();
        let path = CgroupPath::new(AbsNormPath::new(path.trim()).unwrap());
        Some(Self::try_from_path(path.to_buf()).unwrap())
    }
}

#[cfg(test)]
impl Cgroup<NoMemoryMonitoring, CgroupKindLeaf> {
    pub(crate) fn create_leaf_for_test() -> Option<Self> {
        Some(Cgroup::create_minimal_for_test()?.into_leaf().unwrap())
    }
}

#[cfg(test)]
impl Cgroup<NoMemoryMonitoring, CgroupKindInternal> {
    pub(crate) fn create_internal_for_test() -> Option<Self> {
        let cgroup = Cgroup::create_minimal_for_test()?;
        let controllers = cgroup.read_enabled_controllers().unwrap();
        Some(
            cgroup
                .enable_subtree_control_and_into_internal(controllers)
                .unwrap(),
        )
    }
}

#[cfg(test)]
mod tests {
    use buck2_fs::fs_util;
    use buck2_fs::paths::file_name::FileName;
    use buck2_util::process::background_command;

    use crate::cgroup::Cgroup;
    use crate::cgroup_files::CgroupFile;

    #[test]
    fn self_test_harness() {
        let Some(cgroup) = Cgroup::create_leaf_for_test() else {
            return;
        };

        assert_eq!(
            fs_util::try_exists(cgroup.path().as_abs_path()).unwrap(),
            true
        );
        assert_eq!(cgroup.kind.procs.read_to_string().unwrap().len(), 0);
    }

    #[test]
    fn repro_drop_cgroup_before_command_spawn() {
        let Some(cgroup) = Cgroup::create_leaf_for_test() else {
            return;
        };

        let mut cmd = background_command("true");
        cgroup.setup_command(&mut cmd).unwrap();

        drop(cgroup);

        assert!(cmd.status().unwrap().success());
    }

    #[test]
    fn test_cpu_controller_enabled() {
        // FIXME(JakobDegen): This isn't really a good test, we should do it at a higher level and
        // actually run a command. But setting up tests inside cgroups is a bit hard right now, so
        // just do this
        let Some(cgroup) = Cgroup::create_internal_for_test() else {
            return;
        };
        let leaf = cgroup
            .make_leaf_child(FileName::unchecked_new("leaf"))
            .unwrap();

        let enabled =
            fs_util::read_to_string(leaf.path().as_abs_path().join("cgroup.controllers")).unwrap();
        assert!(enabled.contains("memory"));
        assert!(enabled.contains("cpu"));
    }

    #[test]
    fn test_reading_effective_cgroup_constraints() {
        let Some(cgroup) = Cgroup::create_internal_for_test() else {
            return;
        };

        fn pages(b: u64) -> String {
            (b * 4096).to_string()
        }

        let subg1 = cgroup
            .make_internal_child(FileName::unchecked_new("subg1"))
            .unwrap();
        CgroupFile::open(&subg1.dir, FileName::unchecked_new("memory.high"), true)
            .unwrap()
            .write(pages(1000).as_bytes())
            .unwrap();
        CgroupFile::open(&subg1.dir, FileName::unchecked_new("memory.max"), true)
            .unwrap()
            .write(pages(1000).as_bytes())
            .unwrap();

        let subg2 = subg1
            .make_internal_child(FileName::unchecked_new("subg2"))
            .unwrap();
        CgroupFile::open(&subg2.dir, FileName::unchecked_new("memory.high"), true)
            .unwrap()
            .write(pages(500).as_bytes())
            .unwrap();

        let constraints = subg2.read_effective_resouce_constraints().unwrap();
        assert_eq!(constraints.memory_high, Some(500 * 4096));
        assert_eq!(constraints.memory_max, Some(1000 * 4096));

        // Depending on where this test runs, these might be non-zero and we can't really do much
        // about that, so just do our best
        let base_constraints = cgroup.read_effective_resouce_constraints().unwrap();
        assert_eq!(
            constraints.memory_swap_high,
            base_constraints.memory_swap_high
        );
        assert_eq!(
            constraints.memory_swap_max,
            base_constraints.memory_swap_max
        );
    }
}
