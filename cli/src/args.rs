/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{
    convert::TryFrom,
    ffi::OsStr,
    fs::File,
    io::{self, BufRead},
    path::Path,
    process::Command,
    str,
};

use anyhow::{anyhow, Context as _};
use buck2_common::legacy_configs::BuckConfigBasedCells;
use buck2_core::{
    cells::CellResolver,
    fs::{
        anyhow as fs,
        paths::{AbsPath, AbsPathBuf},
        project::ProjectFilesystem,
    },
};
use once_cell::unsync::OnceCell;
use termwiz::istty::IsTty;
use thiserror::Error;

use crate::roots::find_current_roots;

#[derive(Error, Debug)]
enum ArgExpansionError {
    #[error("Missing flag file path after --flagfile argument")]
    MissingFlagFilePath,
    #[error("Unable to read flag file at `{path}`")]
    MissingFlagFileOnDisk { source: anyhow::Error, path: String },
    #[error("Unable to read line in flag file `{path}`")]
    FlagFileReadError { source: anyhow::Error, path: String },
    #[error("Python mode file `{path}` output is not UTF-8")]
    PythonOutputNotUtf8 { path: String },
    #[error("No flag file path after @ symbol in argfile argument")]
    MissingFlagFilePathInArgfile,
    #[error("Failed to compute parent dir of argfile")]
    FailedToComputeParentOfArgFile,
    #[error("Python argfile at `{path}` exited with non-zero status, stderr: {err:?}")]
    PythonExecutableFailed { path: String, err: String },
}

/// Argument resolver that can expand cell paths, e.g., `cell//path/to/file`
struct ArgCellPathResolver {
    // Deliberately use `OnceCell` rather than `Lazy` because `Lazy` forces
    // us to have a shared reference to the underlying `anyhow::Error` which
    // we cannot use to correct chain the errors. Using `OnceCell` means
    // we don't get the result by a shared reference but instead as local
    // value which can be returned.
    data: OnceCell<ArgCellPathResolverData>,
}

/// Defines the data which is lazy computed to avoid doing I/O
/// unless necessary (e.g., encountering a cell-relative path).
struct ArgCellPathResolverData {
    cell_resolver: CellResolver,
    project_filesystem: ProjectFilesystem,
}

impl ArgCellPathResolver {
    pub fn new() -> Self {
        Self {
            data: OnceCell::new(),
        }
    }

    /// Resolves an argument which can possibly be a cell-relative path.
    /// If the argument is not a cell-relative path, it returns `None`.
    /// Otherwise, it tries to resolve the cell and returns a `Result`.
    pub fn resolve_cell_path_arg(
        &self,
        path: &str,
        cwd: &AbsPath,
    ) -> Option<anyhow::Result<AbsPathBuf>> {
        path.split_once("//")
            .map(|(cell_alias, cell_relative_path)| {
                self.resolve_cell_path(cell_alias, cell_relative_path, cwd)
            })
    }

    /// Resolves a cell path (i.e., contains `//`) into an absolute path. The cell path must have
    /// been split into two components: `cell_alias` and `cell_path`. For example, if the cell path
    /// is `cell//path/to/file`, then:
    ///   - `cell_alias` would be `cell`
    ///   - `cell_relative_path` would be `path/to/file`
    fn resolve_cell_path(
        &self,
        cell_alias: &str,
        cell_relative_path: &str,
        cwd: &AbsPath,
    ) -> anyhow::Result<AbsPathBuf> {
        let resolver_data = self
            .data
            .get_or_try_init(|| {
                let roots = find_current_roots()?;
                let project_root = AbsPath::new(&roots.project_root)?.to_owned();

                // See comment in `ArgCellPathResolver` about why we use `OnceCell` rather than `Lazy`
                let project_filesystem = ProjectFilesystem::new(project_root);
                let cell_resolver =
                    BuckConfigBasedCells::parse_immediate_cell_mapping(&project_filesystem)?;

                anyhow::Ok(ArgCellPathResolverData {
                    cell_resolver,
                    project_filesystem,
                })
            })
            .context("Error creating cell resolver")?;

        resolver_data.cell_resolver.resolve_cell_relative_path(
            cell_alias,
            cell_relative_path,
            &resolver_data.project_filesystem,
            cwd,
        )
    }
}

pub struct ArgExpansionContext {
    arg_resolver: ArgCellPathResolver,
}

impl ArgExpansionContext {
    pub fn new() -> Self {
        Self {
            arg_resolver: ArgCellPathResolver::new(),
        }
    }

    /// Log that a relative flag file was not found in CWD, but was found, and used, from the cell root
    ///
    /// This prints directly to stderr (sometimes in color). This should be safe, because flagfile
    /// expansion runs *very* early in the CLI process lifetime.
    pub fn log_relative_path_from_cell_root(&self, requested_path: &str) -> anyhow::Result<()> {
        let (prefix, reset) = if io::stderr().is_tty() {
            ("\x1b[33m", "\x1b[0m")
        } else {
            ("WARNING: ", "")
        };
        crate::eprintln!(
            "{}`@{}` was specified, but not found. Using file at `//{}`.",
            prefix,
            requested_path,
            requested_path
        )?;
        crate::eprintln!(
            "This behavior is being deprecated. Please use `@//{}` instead{}",
            requested_path,
            reset
        )?;
        Ok(())
    }
}

#[derive(Clone)]
enum ArgFile {
    PythonExecutable(AbsPathBuf, Option<String>),
    Path(AbsPathBuf),
}

// Expands any argfiles passed as command line parameters. There are
// two ways to do: `@argfile` or `--flagfile PATH`.
//
// Caveats:
//  - `--` and `--flagfile` cannot be values of other options
//  - `--flagfile=X` is _not_ supported, you need to pass
//    `--flagfile X` instead.
//  - `--flagfil` is _not_ supported.
//
// TODO: This function should also return tracking information, so
//       that we know where args come from. This would be useful
//       in cases where the argfiles contain `--config` flags.
//
// TODO: It does _not_ support executable argfiles (e.g., Python)
//       which are supported by Buck v1. See `BuckArgsMethods` in
//       Buck v1 for reference.
pub fn expand_argfiles(args: Vec<String>, cwd: &Path) -> anyhow::Result<Vec<String>> {
    let abs_cwd = AbsPath::new(cwd)?;
    expand_argfiles_with_context(args, &ArgExpansionContext::new(), abs_cwd)
}

fn expand_argfiles_with_context(
    args: Vec<String>,
    context: &ArgExpansionContext,
    cwd: &AbsPath,
) -> anyhow::Result<Vec<String>> {
    let mut expanded_args = Vec::new();
    let mut arg_iterator = args.into_iter();

    while let Some(next_arg) = arg_iterator.next() {
        match next_arg.as_str() {
            "--" => {
                expanded_args.push(next_arg);
                expanded_args.extend(arg_iterator);
                break;
            }
            "--flagfile" => {
                let flagfile = match arg_iterator.next() {
                    Some(val) => val,
                    None => return Err(anyhow!(ArgExpansionError::MissingFlagFilePath)),
                };
                // TODO: We want to detect cyclic inclusion
                let expanded_flagfile_args = resolve_and_expand_argfile(&flagfile, context, cwd)?;
                expanded_args.extend(expanded_flagfile_args);
            }
            next_arg if next_arg.starts_with('@') => {
                let flagfile = next_arg.strip_prefix('@').unwrap();
                if flagfile.is_empty() {
                    return Err(anyhow!(ArgExpansionError::MissingFlagFilePathInArgfile));
                }
                // TODO: We want to detect cyclic inclusion
                let expanded_flagfile_args = resolve_and_expand_argfile(flagfile, context, cwd)?;
                expanded_args.extend(expanded_flagfile_args);
            }
            _ => expanded_args.push(next_arg),
        }
    }

    Ok(expanded_args)
}

// Resolves a path argument to an absolute path, reads the flag file and expands
// it into a list of arguments.
fn resolve_and_expand_argfile(
    path: &str,
    context: &ArgExpansionContext,
    cwd: &AbsPath,
) -> anyhow::Result<Vec<String>> {
    let flagfile = resolve_flagfile(path, context, cwd)
        .with_context(|| format!("Error resolving flagfile `{}`", path))?;
    let flagfile_path = match flagfile.to_owned() {
        ArgFile::Path(file_path) => file_path,
        ArgFile::PythonExecutable(file_path, _) => file_path,
    };
    let flagfile_lines = expand_argfile_contents(&flagfile)?;
    // Cell resolution inside flag files must be performed relative to the cell
    // which contains the flag file itself.
    let effective_cwd = flagfile_path
        .parent()
        .ok_or_else(|| anyhow!(ArgExpansionError::FailedToComputeParentOfArgFile))?;
    expand_argfiles_with_context(flagfile_lines, context, effective_cwd)
}

fn expand_argfile_contents(flagfile: &ArgFile) -> anyhow::Result<Vec<String>> {
    match flagfile {
        ArgFile::Path(path) => {
            let mut lines = Vec::new();
            let file =
                File::open(path).map_err(|source| ArgExpansionError::MissingFlagFileOnDisk {
                    source: source.into(),
                    path: path.to_string_lossy().into_owned(),
                })?;
            let reader = io::BufReader::new(file);
            for line_result in reader.lines() {
                let line = line_result.map_err(|source| ArgExpansionError::FlagFileReadError {
                    source: source.into(),
                    path: path.to_string_lossy().into_owned(),
                })?;
                lines.push(line);
            }
            Ok(lines)
        }
        ArgFile::PythonExecutable(path, flag) => {
            let mut cmd = Command::new("python3");
            let cmd_with_arg = match flag {
                Some(flag) => cmd
                    .arg(path.as_os_str())
                    .arg("--flavors")
                    .arg(OsStr::new(flag)),
                None => cmd.arg(path.as_os_str()),
            };
            let cmd_out = cmd_with_arg
                .output()
                .expect("python executable failed to run");
            if cmd_out.status.success() {
                Ok(str::from_utf8(&cmd_out.stdout)
                    .map_err(|_| ArgExpansionError::PythonOutputNotUtf8 {
                        path: path.to_string_lossy().into_owned(),
                    })?
                    .lines()
                    .map(|s| s.to_owned())
                    .collect::<Vec<String>>())
            } else {
                Err(anyhow!(ArgExpansionError::PythonExecutableFailed {
                    path: path.to_string_lossy().into_owned(),
                    err: String::from_utf8_lossy(&cmd_out.stderr).to_string(),
                }))
            }
        }
    }
}

// Resolves a path argument to an absolute path, so that it can be read.
fn resolve_flagfile(
    path: &str,
    context: &ArgExpansionContext,
    cwd: &AbsPath,
) -> anyhow::Result<ArgFile> {
    let (path_part, flag) = match path.split_once('#') {
        Some((pypath, pyflag)) => (pypath.to_owned(), Some(pyflag.to_owned())),
        None => (path.to_owned(), None),
    };

    let resolved_path = if let Some(cell_resolved_path) =
        context.arg_resolver.resolve_cell_path_arg(&path_part, cwd)
    {
        cell_resolved_path.context("Error resolving cell path")?
    } else {
        let p = Path::new(&path_part);
        if !p.is_absolute() {
            let abs_path = match fs::canonicalize(p) {
                Ok(abs_path) => Ok(abs_path),
                Err(original_error) => {
                    let cell_relative_path = context
                        .arg_resolver
                        .resolve_cell_path("", &path_part, cwd)?;
                    // If the relative path does not exist relative to the cwd,
                    // attempt to make it relative to the cell root. If *that*
                    // doesn't exist, just report the original error back, and
                    // don't tip users off that they can use relative-to-cell paths.
                    // We want to deprecate that.
                    match fs::canonicalize(cell_relative_path) {
                        Ok(abs_path) => {
                            context.log_relative_path_from_cell_root(&path_part)?;
                            Ok(abs_path)
                        }
                        Err(_) => Err(ArgExpansionError::MissingFlagFileOnDisk {
                            source: original_error,
                            path: p.to_string_lossy().into_owned(),
                        }),
                    }
                }
            }?;
            AbsPathBuf::try_from(abs_path)?
        } else {
            AbsPathBuf::try_from(p.to_owned())?
        }
    };

    if path_part.ends_with(".py") {
        Ok(ArgFile::PythonExecutable(resolved_path, flag))
    } else {
        Ok(ArgFile::Path(resolved_path))
    }
}
