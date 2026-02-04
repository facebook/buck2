/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::os::fd::OwnedFd;
use std::os::unix::process::CommandExt;
#[cfg(test)]
use std::process::Child;
use std::process::Command;
use std::sync::Arc;
#[cfg(test)]
use std::time::Duration;

use buck2_fs::paths::file_name::FileName;
use buck2_fs::paths::file_name::FileNameBuf;
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
    memory_current: CgroupFile,
    swap_current: CgroupFile,
    memory_pressure: CgroupFile,
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
    dir: Arc<OwnedFd>,
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

    pub(crate) fn dir_fd(&self) -> &OwnedFd {
        &self.dir
    }

    pub(crate) async fn read_enabled_controllers(&self) -> buck2_error::Result<EnabledControllers> {
        let controllers_file = CgroupFile::open(
            self.dir.dupe(),
            FileNameBuf::unchecked_new("cgroup.controllers"),
            false,
        )
        .await?;
        let controllers = controllers_file.read_to_string().await?;
        Ok(EnabledControllers(
            controllers
                .split_whitespace()
                .map(|s| format!("+{}", s))
                .collect(),
        ))
    }

    /// Set the memory.high limit for this cgroup
    pub async fn set_memory_high(&self, memory_high: &str) -> buck2_error::Result<()> {
        CgroupFile::open(
            self.dir.dupe(),
            FileNameBuf::unchecked_new("memory.high"),
            true,
        )
        .await?
        .write(memory_high.to_owned())
        .await
    }

    /// Set the memory.max limit for this cgroup
    pub async fn set_memory_max(&self, memory_max: &str) -> buck2_error::Result<()> {
        CgroupFile::open(
            self.dir.dupe(),
            FileNameBuf::unchecked_new("memory.max"),
            true,
        )
        .await?
        .write(memory_max.to_owned())
        .await
    }

    async fn read_resource_constraints(&self) -> buck2_error::Result<EffectiveResourceConstraints> {
        let read = |f| async move {
            let f = CgroupFile::open(self.dir.dupe(), FileNameBuf::unchecked_new(f), false).await?;
            buck2_error::Ok(f.read_max_or_int().await?)
        };
        let (memory_high, memory_max, memory_swap_high, memory_swap_max) = tokio::try_join!(
            read("memory.high"),
            read("memory.max"),
            read("memory.swap.high"),
            read("memory.swap.max"),
        )?;
        Ok(EffectiveResourceConstraints {
            memory_high,
            memory_max,
            memory_swap_high,
            memory_swap_max,
        })
    }

    pub(crate) async fn read_effective_resouce_constraints(
        &self,
    ) -> buck2_error::Result<EffectiveResourceConstraints> {
        let Some(parent_path) = self.path.parent() else {
            // The root cgroup doesn't have memory restrictions (the files don't even exist)
            return Ok(EffectiveResourceConstraints::default());
        };
        let (parent, me) = tokio::try_join!(
            async {
                Box::pin(
                    CgroupMinimal::try_from_path(parent_path.to_buf())
                        .await?
                        .read_effective_resouce_constraints(),
                )
                .await
            },
            self.read_resource_constraints()
        )?;

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
    pub(crate) fn sync_try_from_path(path: CgroupPathBuf) -> buck2_error::Result<Self> {
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
            dir: Arc::new(dir),
            memory: NoMemoryMonitoring,
            kind: CgroupKindUndecided,
        };

        Ok(cgroup)
    }

    pub async fn try_from_path(path: CgroupPathBuf) -> buck2_error::Result<Self> {
        tokio::task::spawn_blocking(|| Self::sync_try_from_path(path)).await?
    }

    /// Treat this cgroup as a leaf cgroup
    pub async fn into_leaf(
        self,
    ) -> buck2_error::Result<Cgroup<NoMemoryMonitoring, CgroupKindLeaf>> {
        Ok(Cgroup {
            kind: CgroupKindLeaf {
                procs: Arc::new(
                    CgroupFile::open(
                        self.dir.dupe(),
                        FileNameBuf::unchecked_new("cgroup.procs"),
                        true,
                    )
                    .await?,
                ),
            },
            dir: self.dir,
            path: self.path,
            memory: self.memory,
        })
    }

    /// Enable subtree controllers on this cgroup as specified and convert to an internal cgroup
    pub(crate) async fn enable_subtree_control_and_into_internal(
        self,
        enabled_controllers: EnabledControllers,
    ) -> buck2_error::Result<Cgroup<NoMemoryMonitoring, CgroupKindInternal>> {
        let subtree_control = CgroupFile::open(
            self.dir.dupe(),
            FileNameBuf::unchecked_new("cgroup.subtree_control"),
            true,
        )
        .await?;
        for controller in &*enabled_controllers.0 {
            subtree_control.write(controller.clone()).await?;
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
    pub fn setup_command(&self, command: &mut Command) {
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
        let pre_exec = move || procs.sync_write(b"0");
        // Safety: The unsafe block is required for pre_exec which is inherently unsafe due to fork/exec restrictions.
        // However, it's safe here because:
        // 1. We only call async-signal-safe functions (write to file)
        // 2. No memory allocation or complex operations that could deadlock
        unsafe {
            command.pre_exec(pre_exec);
        }
    }

    #[cfg(test)]
    pub(crate) fn spawn_use_some_memory_process(
        &self,
        allocate_count: u64,
        each_tick_allocate_memory_mbs: f64,
        tick_duration: Option<Duration>,
        pre_exit_sleep_duration: Option<Duration>,
    ) -> buck2_error::Result<Child> {
        use std::process::Stdio;

        use buck2_util::process::background_command;

        let bin = std::env::var("USE_SOME_MEMORY_BIN").unwrap();
        let mut cmd = background_command(bin);
        self.setup_command(&mut cmd);
        cmd.stdout(Stdio::null());
        cmd.stderr(Stdio::null());
        cmd.arg("--allocate-count")
            .arg(allocate_count.to_string())
            .arg("--each-tick-allocate-memory")
            .arg(each_tick_allocate_memory_mbs.to_string());
        if let Some(tick_duration) = tick_duration {
            cmd.arg("--tick-duration")
                .arg(tick_duration.as_secs_f64().to_string());
        }
        if let Some(pre_exit_sleep_duration) = pre_exit_sleep_duration {
            cmd.arg("--pre-exit-sleep-duration")
                .arg(pre_exit_sleep_duration.as_secs_f64().to_string());
        }
        Ok(cmd.spawn()?)
    }
}

impl<M: MemoryMonitoring, K: CgroupKind> Cgroup<M, K> {
    fn sync_discouraged_make_child_impl(
        dir: &OwnedFd,
        path: &CgroupPath,
        child: &FileName,
    ) -> buck2_error::Result<CgroupMinimal> {
        nix::sys::stat::mkdirat(dir, child.as_str(), Mode::all())?;

        let fd = nix::fcntl::openat(
            dir,
            child.as_str(),
            OFlag::O_CLOEXEC | OFlag::O_DIRECTORY,
            Mode::empty(),
        )?;

        Ok(CgroupMinimal {
            dir: Arc::new(fd),
            path: path.join(child.as_forward_rel_path()),
            memory: NoMemoryMonitoring,
            kind: CgroupKindUndecided,
        })
    }

    pub(crate) fn sync_discouraged_make_child(
        &self,
        child: &FileName,
    ) -> buck2_error::Result<CgroupMinimal> {
        Self::sync_discouraged_make_child_impl(&self.dir, &self.path, child)
    }

    /// Make a child cgroup
    ///
    /// Use strongly discouraged; almost always use `make_internal_child` or `make_leaf_child` instead
    pub(crate) async fn discouraged_make_child(
        &self,
        child: FileNameBuf,
    ) -> buck2_error::Result<CgroupMinimal> {
        let dir = self.dir.dupe();
        let path = self.path.clone();
        tokio::task::spawn_blocking(move || {
            Self::sync_discouraged_make_child_impl(&dir, &path, &child)
        })
        .await?
    }
}

impl<M: MemoryMonitoring> Cgroup<M, CgroupKindInternal> {
    /// Make a child cgroup
    ///
    /// Unlike above, not discouraged because we know this to be an internal cgroup
    pub(crate) async fn make_child(
        &self,
        child: FileNameBuf,
    ) -> buck2_error::Result<CgroupMinimal> {
        self.discouraged_make_child(child).await
    }

    pub(crate) async fn make_internal_child(
        &self,
        child: FileNameBuf,
    ) -> buck2_error::Result<Cgroup<NoMemoryMonitoring, CgroupKindInternal>> {
        let c = self.make_child(child).await?;
        c.enable_subtree_control_and_into_internal(self.kind.controllers.dupe())
            .await
    }

    pub(crate) async fn make_leaf_child(
        &self,
        child: FileNameBuf,
    ) -> buck2_error::Result<Cgroup<NoMemoryMonitoring, CgroupKindLeaf>> {
        let c = self.make_child(child).await?;
        c.into_leaf().await
    }
}

impl<K: CgroupKind> Cgroup<NoMemoryMonitoring, K> {
    pub(crate) async fn enable_memory_monitoring(
        self,
    ) -> buck2_error::Result<Cgroup<WithMemoryMonitoring, K>> {
        let open = |f| {
            let d = &self.dir;
            async move { CgroupFile::open(d.dupe(), FileNameBuf::unchecked_new(f), false).await }
        };
        let (memory_stat, memory_current, memory_swap_current, memory_pressure) = tokio::try_join!(
            open("memory.stat"),
            open("memory.current"),
            open("memory.swap.current"),
            open("memory.pressure"),
        )?;
        Ok(Cgroup {
            memory: WithMemoryMonitoring {
                memory_stat,
                memory_current,
                swap_current: memory_swap_current,
                memory_pressure,
            },
            dir: self.dir,
            path: self.path,
            kind: self.kind,
        })
    }
}

impl<K: CgroupKind> Cgroup<WithMemoryMonitoring, K> {
    pub async fn read_memory_stat(&self) -> buck2_error::Result<MemoryStat> {
        self.memory.memory_stat.read_memory_stat().await
    }

    pub async fn read_memory_current(&self) -> buck2_error::Result<u64> {
        self.memory.memory_current.read_int().await
    }

    pub async fn read_swap_current(&self) -> buck2_error::Result<u64> {
        self.memory.swap_current.read_int().await
    }

    pub async fn read_memory_pressure(&self) -> buck2_error::Result<f64> {
        Ok(self
            .memory
            .memory_pressure
            .read_resource_pressure()
            .await?
            .some
            .avg10)
    }
}

pub struct CgroupFreezeGuard {
    file: CgroupFile,
}

impl Drop for CgroupFreezeGuard {
    fn drop(&mut self) {
        drop(self.file.sync_write(b"0"));
    }
}

impl<M: MemoryMonitoring, K: CgroupKind> Cgroup<M, K> {
    pub async fn freeze(&self) -> buck2_error::Result<CgroupFreezeGuard> {
        let f = CgroupFile::open(
            self.dir.dupe(),
            FileNameBuf::unchecked_new("cgroup.freeze"),
            true,
        )
        .await?;
        f.write(b"1").await?;
        Ok(CgroupFreezeGuard { file: f })
    }
}

#[cfg(test)]
impl CgroupMinimal {
    pub(crate) async fn create_minimal_for_test() -> Option<Self> {
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

        // Attempt to actually spawn a process into the cgroup, see below for why
        let cgroup = Self::try_from_path(path.to_buf())
            .await
            .unwrap()
            .into_leaf()
            .await
            .unwrap();
        let mut cmd = background_command("true");
        cgroup.setup_command(&mut cmd);
        let res = cmd.status();
        drop(cgroup);

        // At this point the one process in that cgroup is dead, so it's ok to treat it as a minimal
        // one again
        let cgroup = Self::try_from_path(path.to_buf()).await.unwrap();
        let controllers = cgroup.read_enabled_controllers().await.unwrap();
        let parent = cgroup
            .enable_subtree_control_and_into_internal(controllers)
            .await
            .unwrap();

        match res {
            Ok(_) => {
                // Use a fresh child cgroup - we don't want to use the one above since it has some
                // leftover metrics corresponding to the process we just ran in it
                let child = parent
                    .discouraged_make_child(FileNameBuf::unchecked_new("test_group"))
                    .await
                    .unwrap();
                Some(child)
            }
            Err(e) if e.kind() == std::io::ErrorKind::PermissionDenied => {
                // The script above generates a cgroup for us to use by starting a new systemd user
                // service. However, depending on the cgroup in which *this test* is running, we may
                // not be able to spawn or move any processes into there. Practically, this actually
                // depends on whether the buck that is running this test has resource control
                // enabled, since that effects the cgroup this test runs in.
                //
                // In case where that's an issue, work around it by attempting to use sudo perms to
                // move ourselves somewhere from where we can do what we need. This unfortunately
                // means that attempting to run multiple tests like this in the same process won't
                // work, but alas this is the best we can do
                let leaf = parent
                    .make_child(FileNameBuf::unchecked_new("_buck_leaf"))
                    .await
                    .unwrap();
                // Move ourselves into the cgroup we just created
                let cmd = format!(
                    "echo {} | sudo tee {}/cgroup.procs",
                    std::process::id(),
                    leaf.path(),
                );
                assert!(
                    background_command("bash")
                        .arg("-c")
                        .arg(cmd)
                        .status()
                        .unwrap()
                        .success()
                );
                Some(
                    parent
                        .make_child(FileNameBuf::unchecked_new("test_group"))
                        .await
                        .unwrap(),
                )
            }
            Err(e) => panic!("Failed to open cgroup.procs: {e:?}"),
        }
    }
}

#[cfg(test)]
impl Cgroup<NoMemoryMonitoring, CgroupKindLeaf> {
    pub(crate) async fn create_leaf_for_test() -> Option<Self> {
        Some(
            Cgroup::create_minimal_for_test()
                .await?
                .into_leaf()
                .await
                .unwrap(),
        )
    }
}

#[cfg(test)]
impl Cgroup<NoMemoryMonitoring, CgroupKindInternal> {
    pub(crate) async fn create_internal_for_test() -> Option<Self> {
        let cgroup = Cgroup::create_minimal_for_test().await?;
        let controllers = cgroup.read_enabled_controllers().await.unwrap();
        Some(
            cgroup
                .enable_subtree_control_and_into_internal(controllers)
                .await
                .unwrap(),
        )
    }
}

#[cfg(test)]
mod tests {
    use std::time::Duration;

    use buck2_fs::fs_util::uncategorized as fs_util;
    use buck2_fs::paths::file_name::FileNameBuf;
    use buck2_util::process::background_command;
    use dupe::Dupe;

    use crate::cgroup::Cgroup;
    use crate::cgroup_files::CgroupFile;

    #[tokio::test]
    async fn self_test_harness() {
        let Some(cgroup) = Cgroup::create_leaf_for_test().await else {
            return;
        };

        assert!(fs_util::try_exists(cgroup.path().as_abs_path()).unwrap());
        assert_eq!(cgroup.kind.procs.read_to_string().await.unwrap().len(), 0);
    }

    #[tokio::test]
    async fn repro_drop_cgroup_before_command_spawn() {
        let Some(cgroup) = Cgroup::create_leaf_for_test().await else {
            return;
        };

        let mut cmd = background_command("true");
        cgroup.setup_command(&mut cmd);

        drop(cgroup);

        assert!(cmd.status().unwrap().success());
    }

    #[tokio::test]
    async fn test_cpu_controller_enabled() {
        // FIXME(JakobDegen): This isn't really a good test, we should do it at a higher level and
        // actually run a command. But setting up tests inside cgroups is a bit hard right now, so
        // just do this
        let Some(cgroup) = Cgroup::create_internal_for_test().await else {
            return;
        };
        let leaf = cgroup
            .make_leaf_child(FileNameBuf::unchecked_new("leaf"))
            .await
            .unwrap();

        let enabled =
            fs_util::read_to_string(leaf.path().as_abs_path().join("cgroup.controllers")).unwrap();
        assert!(enabled.contains("memory"));
        assert!(enabled.contains("cpu"));
    }

    #[tokio::test]
    async fn test_reading_effective_cgroup_constraints() {
        let Some(cgroup) = Cgroup::create_internal_for_test().await else {
            return;
        };

        fn pages(b: u64) -> String {
            (b * 4096).to_string()
        }

        let subg1 = cgroup
            .make_internal_child(FileNameBuf::unchecked_new("subg1"))
            .await
            .unwrap();
        CgroupFile::open(
            subg1.dir.dupe(),
            FileNameBuf::unchecked_new("memory.high"),
            true,
        )
        .await
        .unwrap()
        .write(pages(1000))
        .await
        .unwrap();
        CgroupFile::open(
            subg1.dir.dupe(),
            FileNameBuf::unchecked_new("memory.max"),
            true,
        )
        .await
        .unwrap()
        .write(pages(1000))
        .await
        .unwrap();

        let subg2 = subg1
            .make_internal_child(FileNameBuf::unchecked_new("subg2"))
            .await
            .unwrap();
        CgroupFile::open(
            subg2.dir.dupe(),
            FileNameBuf::unchecked_new("memory.high"),
            true,
        )
        .await
        .unwrap()
        .write(pages(500))
        .await
        .unwrap();

        let constraints = subg2.read_effective_resouce_constraints().await.unwrap();
        assert_eq!(constraints.memory_high, Some(500 * 4096));
        assert_eq!(constraints.memory_max, Some(1000 * 4096));

        // Depending on where this test runs, these might be non-zero and we can't really do much
        // about that, so just do our best
        let base_constraints = cgroup.read_effective_resouce_constraints().await.unwrap();
        assert_eq!(
            constraints.memory_swap_high,
            base_constraints.memory_swap_high
        );
        assert_eq!(
            constraints.memory_swap_max,
            base_constraints.memory_swap_max
        );
    }

    #[tokio::test]
    async fn test_read_memory_metrics() {
        let Some(cgroup) = Cgroup::create_leaf_for_test().await else {
            return;
        };
        let cgroup = cgroup.enable_memory_monitoring().await.unwrap();

        let stat = cgroup.read_memory_stat().await.unwrap();
        // Never spawned a process into it, so should be empty
        assert_eq!(stat.anon, 0);
        assert_eq!(stat.file, 0);
        assert_eq!(cgroup.read_memory_current().await.unwrap(), 0);
        assert_eq!(cgroup.read_swap_current().await.unwrap(), 0);
        assert_eq!(cgroup.read_memory_pressure().await.unwrap(), 0.0);

        cgroup.set_memory_high("19000000").await.unwrap();
        let mut child = cgroup
            .spawn_use_some_memory_process(1, 20.0, None, Some(Duration::from_secs(100)))
            .unwrap();

        // Give the process some time to start
        std::thread::sleep(Duration::from_secs(10));
        let stat = cgroup.read_memory_stat().await.unwrap();
        assert!(stat.anon > 15000000, "{:?}", stat);
        let memory_current = cgroup.read_memory_current().await.unwrap();
        assert!(memory_current > 15000000, "{:?}", memory_current);
        let swap_current = cgroup.read_swap_current().await.unwrap();
        assert!(swap_current > 500000, "{:?}", swap_current);
        let memory_pressure = cgroup.read_memory_pressure().await.unwrap();
        assert!(memory_pressure > 5.0, "{:?}", memory_pressure);

        child.kill().unwrap();
        child.wait().unwrap();
    }
}
