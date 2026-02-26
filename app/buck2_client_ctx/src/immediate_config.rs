/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::sync::OnceLock;
use std::time::SystemTime;

use buck2_common::argv::ArgFileKind;
use buck2_common::argv::ArgFilePath;
use buck2_common::init::DaemonStartupConfig;
use buck2_common::invocation_roots::InvocationRoots;
use buck2_common::invocation_roots::find_invocation_roots;
use buck2_common::legacy_configs::cells::BuckConfigBasedCells;
#[cfg(fbcode_build)]
use buck2_common::legacy_configs::key::BuckconfigKeyRef;
use buck2_core::buck2_env;
use buck2_core::cells::CellAliasResolver;
use buck2_core::cells::CellResolver;
use buck2_core::cells::cell_path::CellPathRef;
use buck2_core::cells::cell_root_path::CellRootPathBuf;
use buck2_core::fs::project::ProjectRoot;
use buck2_error::BuckErrorContext;
use buck2_error::internal_error;
use buck2_fs::fs_util;
use buck2_fs::paths::abs_norm_path::AbsNormPath;
use buck2_fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_fs::paths::abs_path::AbsPath;
use buck2_fs::working_dir::AbsWorkingDir;
use prost::Message;

/// Limited view of the root config. This does not follow includes.
struct ImmediateConfig {
    cell_resolver: CellResolver,
    cwd_cell_alias_resolver: CellAliasResolver,
    daemon_startup_config: DaemonStartupConfig,
    #[cfg(fbcode_build)]
    show_sentiment: bool,
}

impl ImmediateConfig {
    /// Performs a parse of the root `.buckconfig` for the cell _only_ without following includes
    /// and without parsing any configs for any referenced cells. This means this function might return
    /// an empty mapping if the root `.buckconfig` does not contain the cell definitions.
    fn parse(roots: &InvocationRoots) -> buck2_error::Result<ImmediateConfig> {
        // This function is non-reentrant, and blocking for a bit should be ok
        let cells = futures::executor::block_on(BuckConfigBasedCells::parse_with_config_args(
            &roots.project_root,
            &[],
        ))?;

        let cwd_cell_alias_resolver = futures::executor::block_on(
            cells.get_cell_alias_resolver_for_cwd_fast(&roots.project_root, &roots.cwd),
        )?;

        Ok(ImmediateConfig {
            cell_resolver: cells.cell_resolver,
            cwd_cell_alias_resolver,
            daemon_startup_config: DaemonStartupConfig::new(&cells.root_config)
                .buck_error_context("Error loading daemon startup config")?,
            #[cfg(fbcode_build)]
            show_sentiment: cells
                .root_config
                .get(BuckconfigKeyRef {
                    section: "experiments",
                    property: "sentiment",
                })
                .is_some_and(|v| v == "true"),
        })
    }
}

/// Lazy-computed immediate config data. This is produced by reading the root buckconfig (but not
/// processing any includes).
struct ImmediateConfigContextData {
    cell_resolver: CellResolver,
    cwd_cell_alias_resolver: CellAliasResolver,
    daemon_startup_config: DaemonStartupConfig,
    project_filesystem: ProjectRoot,
    #[cfg(fbcode_build)]
    show_sentiment: bool,
}

pub struct ImmediateConfigContext<'a> {
    // Deliberately use `OnceLock` rather than `Lazy` because `Lazy` forces
    // us to have a shared reference to the underlying `buck2_error::Error` which
    // we cannot use to correct chain the errors. Using `OnceLock` means
    // we don't get the result by a shared reference but instead as local
    // value which can be returned.
    data: OnceLock<ImmediateConfigContextData>,
    cwd: &'a AbsWorkingDir,
    trace: Vec<AbsNormPathBuf>,
}

impl<'a> ImmediateConfigContext<'a> {
    pub fn new(cwd: &'a AbsWorkingDir) -> Self {
        Self {
            data: OnceLock::new(),
            cwd,
            trace: Vec::new(),
        }
    }

    pub(crate) fn push_trace(&mut self, path: &AbsNormPath) {
        self.trace.push(path.to_buf());
    }

    pub(crate) fn trace(&self) -> &[AbsNormPathBuf] {
        &self.trace
    }

    pub fn daemon_startup_config(&self) -> buck2_error::Result<&DaemonStartupConfig> {
        Ok(&self.data()?.daemon_startup_config)
    }

    #[cfg(fbcode_build)]
    pub fn show_sentiment(&self) -> bool {
        self.data().map(|d| d.show_sentiment).unwrap_or(false)
    }

    /// Resolves a cell path (i.e., contains `//`) into an absolute path. The cell path must have
    /// been split into two components: `cell_alias` and `cell_path`. For example, if the cell path
    /// is `cell//path/to/file`, then:
    ///   - `cell_alias` would be `cell`
    ///   - `cell_relative_path` would be `path/to/file`
    pub(crate) fn resolve_cell_path(
        &self,
        cell_alias: &str,
        cell_relative_path: &str,
    ) -> buck2_error::Result<AbsNormPathBuf> {
        let data = self.data()?;

        let cell = data.cwd_cell_alias_resolver.resolve(cell_alias)?;
        let cell = data.cell_resolver.get(cell)?;
        let path = cell.path().join_normalized(cell_relative_path)?;
        Ok(data.project_filesystem.resolve(&path))
    }

    pub(crate) fn resolve_project_path(
        &self,
        path: CellPathRef,
    ) -> buck2_error::Result<AbsNormPathBuf> {
        let data = self.data()?;
        Ok(data
            .project_filesystem
            .resolve(data.cell_resolver.resolve_path(path)?))
    }

    pub fn resolve_alias_to_path_in_cwd(
        &self,
        alias: &str,
    ) -> buck2_error::Result<CellRootPathBuf> {
        let data = self.data()?;
        let cell = data.cwd_cell_alias_resolver.resolve(alias)?;
        Ok(data.cell_resolver.get(cell)?.path().to_buf())
    }

    fn data(&self) -> buck2_error::Result<&ImmediateConfigContextData> {
        self.data
            .get_or_try_init(|| {
                let roots = find_invocation_roots(self.cwd)?;
                let paranoid_info_path = roots.paranoid_info_path()?;

                // See comment in `ImmediateConfig` about why we use `OnceLock` rather than `Lazy`
                let cfg = ImmediateConfig::parse(&roots)?;

                // It'd be nice to deal with this a little differently by having this be a separate
                // type.
                let mut daemon_startup_config = cfg.daemon_startup_config;

                match is_paranoid_enabled(&paranoid_info_path) {
                    Ok(paranoid) => {
                        daemon_startup_config.paranoid = paranoid;
                    }
                    Err(e) => {
                        tracing::warn!(
                            "Failed to determine whether paranoid is enabled in `{}`: {:#}",
                            paranoid_info_path,
                            e
                        );
                    }
                };

                buck2_error::Ok(ImmediateConfigContextData {
                    cell_resolver: cfg.cell_resolver,
                    cwd_cell_alias_resolver: cfg.cwd_cell_alias_resolver,
                    daemon_startup_config,
                    project_filesystem: roots.project_root,
                    #[cfg(fbcode_build)]
                    show_sentiment: cfg.show_sentiment,
                })
            })
            .buck_error_context("Error creating cell resolver")
    }

    pub(crate) fn resolve_argfile_kind(
        &self,
        canonicalized_path: AbsNormPathBuf,
        flag: Option<&str>,
    ) -> Result<buck2_common::argv::ArgFileKind, buck2_error::Error> {
        let is_py = canonicalized_path.extension() == Some("py".as_ref());
        let resolved_path =
            match self.data() {
                Ok(data) if canonicalized_path.starts_with(data.project_filesystem.root()) => {
                    ArgFilePath::Project(data.cell_resolver.get_cell_path_from_abs_path(
                        &canonicalized_path,
                        &data.project_filesystem,
                    )?)
                }
                _ => ArgFilePath::External(canonicalized_path),
            };
        if is_py {
            Ok(ArgFileKind::PythonExecutable(
                resolved_path,
                flag.map(ToOwned::to_owned),
            ))
        } else {
            Ok(ArgFileKind::Path(resolved_path))
        }
    }
}

fn is_paranoid_enabled(path: &AbsPath) -> buck2_error::Result<bool> {
    if let Some(p) = buck2_env!("BUCK_PARANOID", type=bool)? {
        return Ok(p);
    }

    let bytes = match fs_util::read_if_exists(path)? {
        Some(b) => b,
        None => return Ok(false),
    };

    let info = buck2_cli_proto::ParanoidInfo::decode(bytes.as_slice())
        .buck_error_context("Invalid data ")?;

    let now = SystemTime::now();
    let expires_at = SystemTime::try_from(
        info.expires_at
            .ok_or_else(|| internal_error!("Missing expires_at"))?,
    )
    .buck_error_context("Invalid expires_at")?;
    Ok(now < expires_at)
}
