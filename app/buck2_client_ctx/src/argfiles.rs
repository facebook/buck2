/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fs::File;
use std::io;
use std::io::BufRead;
use std::path::Path;
use std::process::Command;
use std::str;

use buck2_core::fs::fs_util;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::paths::abs_path::AbsPath;
use buck2_core::fs::paths::abs_path::AbsPathBuf;
use buck2_core::fs::working_dir::AbsWorkingDir;
use buck2_core::is_open_source;
use buck2_error::BuckErrorContext;
use buck2_util::process::background_command;
use termwiz::istty::IsTty;

use crate::immediate_config::ImmediateConfigContext;

#[derive(buck2_error::Error, Debug)]
enum ArgExpansionError {
    #[error("Missing flag file path after --flagfile argument")]
    MissingFlagFilePath,
    #[error("Unable to read flag file at `{path}`")]
    MissingFlagFileOnDisk { path: String },
    #[error("Unable to read flag file at `{path}`")]
    MissingFlagFileOnDiskWithSource {
        source: buck2_error::Error,
        path: String,
    },
    #[error("Unable to read line in flag file `{path}`")]
    FlagFileReadError {
        source: buck2_error::Error,
        path: String,
    },
    #[error("Python mode file `{path}` output is not UTF-8")]
    PythonOutputNotUtf8 { path: String },
    #[error("No flag file path after @ symbol in argfile argument")]
    MissingFlagFilePathInArgfile,
    #[error("Python argfile at `{path}` exited with non-zero status, stderr: {err:?}")]
    PythonExecutableFailed { path: String, err: String },
    #[error("Python argfile command ({cmd:?}) execution failed")]
    PythonExecutionFailed { source: io::Error, cmd: Command },
    #[error("Unable to read line from stdin")]
    StdinReadError { source: buck2_error::Error },
}

/// Log that a relative flag file was not found in CWD, but was found, and used, from the cell root
///
/// This prints directly to stderr (sometimes in color). This should be safe, because flagfile
/// expansion runs *very* early in the CLI process lifetime.
pub fn log_relative_path_from_cell_root(requested_path: &str) -> buck2_error::Result<()> {
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
        "This behavior is being deprecated. Please use `\"@//{}\"` instead{}",
        requested_path,
        reset
    )?;
    Ok(())
}

#[derive(Clone, Debug)]
enum ArgFile {
    PythonExecutable(AbsPathBuf, Option<String>),
    Path(AbsPathBuf),
    Stdin,
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
pub fn expand_argfiles_with_context(
    args: Vec<String>,
    context: &mut ImmediateConfigContext,
    cwd: &AbsWorkingDir,
) -> buck2_error::Result<Vec<String>> {
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
                    None => {
                        return Err(ArgExpansionError::MissingFlagFilePath.into());
                    }
                };
                // TODO: We want to detect cyclic inclusion
                let expanded_flagfile_args = resolve_and_expand_argfile(&flagfile, context, cwd)?;
                expanded_args.extend(expanded_flagfile_args);
            }
            next_arg if next_arg.starts_with('@') => {
                let flagfile = next_arg.strip_prefix('@').unwrap();
                if flagfile.is_empty() {
                    return Err(ArgExpansionError::MissingFlagFilePathInArgfile.into());
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
    context: &mut ImmediateConfigContext,
    cwd: &AbsWorkingDir,
) -> buck2_error::Result<Vec<String>> {
    let flagfile = resolve_flagfile(path, context, cwd)
        .with_buck_error_context(|| format!("Error resolving flagfile `{}`", path))?;
    let flagfile_lines = expand_argfile_contents(&flagfile)?;
    expand_argfiles_with_context(flagfile_lines, context, cwd)
}

fn expand_argfile_contents(flagfile: &ArgFile) -> buck2_error::Result<Vec<String>> {
    match flagfile {
        ArgFile::Path(path) => {
            let mut lines = Vec::new();
            let file = File::open(path).map_err(|source| {
                ArgExpansionError::MissingFlagFileOnDiskWithSource {
                    source: source.into(),
                    path: path.to_string_lossy().into_owned(),
                }
            })?;
            let reader = io::BufReader::new(file);
            for line_result in reader.lines() {
                let line = line_result.map_err(|source| ArgExpansionError::FlagFileReadError {
                    source: source.into(),
                    path: path.to_string_lossy().into_owned(),
                })?;
                if line.is_empty() {
                    continue;
                }
                lines.push(line);
            }
            Ok(lines)
        }
        ArgFile::PythonExecutable(path, flag) => {
            let mut cmd = background_command(if is_open_source() {
                "python3"
            } else {
                "fbpython"
            });
            cmd.env("BUCK2_ARG_FILE", "1");
            cmd.arg(path.as_os_str());
            if let Some(flag) = flag.as_deref() {
                cmd.args(["--flavors", flag]);
            }
            let cmd_out = cmd
                .output()
                .map_err(|source| ArgExpansionError::PythonExecutionFailed { cmd, source })?;
            if cmd_out.status.success() {
                Ok(str::from_utf8(&cmd_out.stdout)
                    .map_err(|_| ArgExpansionError::PythonOutputNotUtf8 {
                        path: path.to_string_lossy().into_owned(),
                    })?
                    .lines()
                    .filter(|line| !line.is_empty())
                    .map(|s| s.to_owned())
                    .collect::<Vec<String>>())
            } else {
                Err(ArgExpansionError::PythonExecutableFailed {
                    path: path.to_string_lossy().into_owned(),
                    err: String::from_utf8_lossy(&cmd_out.stderr).to_string(),
                }
                .into())
            }
        }
        ArgFile::Stdin => io::stdin()
            .lock()
            .lines()
            .filter_map(|line| match line {
                Ok(x) if x.is_empty() => None,
                Ok(x) => Some(Ok(x)),
                Err(err) => Some(Err(ArgExpansionError::StdinReadError {
                    source: err.into(),
                }
                .into())),
            })
            .collect(),
    }
}

// Resolves a path argument to an absolute path, so that it can be read.
fn resolve_flagfile(
    path: &str,
    context: &mut ImmediateConfigContext,
    cwd: &AbsWorkingDir,
) -> buck2_error::Result<ArgFile> {
    if path == "-" {
        return Ok(ArgFile::Stdin);
    }

    let (path_part, flag) = match path.split_once('#') {
        Some((pypath, pyflag)) => (pypath, Some(pyflag)),
        None => (path, None),
    };

    let resolved_path = match path_part.split_once("//") {
        Some((cell_alias, cell_relative_path)) => context
            .resolve_cell_path(cell_alias, cell_relative_path)
            .buck_error_context("Error resolving cell path")?
            .into_abs_path_buf(),
        None => {
            let p = Path::new(path_part);
            match AbsPath::new(path) {
                Ok(abs_path) => {
                    // FIXME(JakobDegen): Checks for normalization for historical reasons, not sure
                    // why we'd want that
                    AbsNormPathBuf::new(abs_path.as_path().to_path_buf())?.into_abs_path_buf()
                }
                Err(_) => {
                    let abs_p = cwd.resolve(p);
                    if fs_util::try_exists(&abs_p)? {
                        abs_p
                    } else {
                        let cell_relative_path = context.resolve_cell_path("", path_part)?;
                        // If the relative path does not exist relative to the cwd,
                        // attempt to make it relative to the cell root. If *that*
                        // doesn't exist, just report the original error back, and
                        // don't tip users off that they can use relative-to-cell paths.
                        // We want to deprecate that.
                        match fs_util::try_exists(&cell_relative_path) {
                            Ok(true) => {
                                log_relative_path_from_cell_root(path_part)?;
                                cell_relative_path.into_abs_path_buf()
                            }
                            _ => {
                                return Err(ArgExpansionError::MissingFlagFileOnDisk {
                                    path: p.to_path_buf().to_string_lossy().into_owned(),
                                }
                                .into());
                            }
                        }
                    }
                }
            }
        }
    };

    // FIXME(JakobDegen): Don't canonicalize
    context.push_trace(&fs_util::canonicalize(&resolved_path)?);
    if path_part.ends_with(".py") {
        Ok(ArgFile::PythonExecutable(
            resolved_path,
            flag.map(ToOwned::to_owned),
        ))
    } else {
        Ok(ArgFile::Path(resolved_path))
    }
}

#[cfg(test)]
mod tests {
    use buck2_core::fs::paths::abs_path::AbsPath;
    use buck2_core::fs::working_dir::AbsWorkingDir;

    use super::*;

    #[test]
    fn test_expand_argfile_content() {
        let tempdir = tempfile::tempdir().unwrap();
        let root = AbsPath::new(tempdir.path()).unwrap();
        let mode_file = root.join("mode-file");
        // Test skips empty lines.
        fs_util::write(&mode_file, "a\n\nb\n").unwrap();
        let lines = expand_argfile_contents(&ArgFile::Path(mode_file)).unwrap();
        assert_eq!(vec!["a".to_owned(), "b".to_owned()], lines);
    }

    #[test]
    fn test_relative_inclusion() {
        // Currently all @-files both on the command line and in files are relative to the current directory.
        // This matches gcc/clang, so write a test we don't inadvertantly change it.
        let tempdir = tempfile::tempdir().unwrap();
        let root = AbsPath::new(tempdir.path()).unwrap();
        fs_util::create_dir(root.join("foo")).unwrap();
        fs_util::create_dir(root.join("foo/bar")).unwrap();
        fs_util::write(root.join("foo/bar/arg1.txt"), "@bar/arg2.txt").unwrap();
        fs_util::write(root.join("foo/bar/arg2.txt"), "--magic").unwrap();
        fs_util::write(root.join(".buckconfig"), "[cells]\nroot = .").unwrap();
        let cwd = AbsWorkingDir::unchecked_new(
            AbsNormPathBuf::new(root.canonicalize().unwrap().join("foo")).unwrap(),
        );
        let mut context = ImmediateConfigContext::new(&cwd);
        let res =
            expand_argfiles_with_context(vec!["@bar/arg1.txt".to_owned()], &mut context, &cwd)
                .unwrap();
        assert_eq!(res, vec!["--magic".to_owned()]);
    }
}
