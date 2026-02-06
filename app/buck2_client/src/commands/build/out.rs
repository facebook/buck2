/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::borrow::Cow;
use std::io;
use std::path::Path;

use buck2_cli_proto::BuildTarget;
use buck2_cli_proto::build_target::BuildOutput;
use buck2_client_ctx::exit_result::ClientIoError;
use buck2_client_ctx::output_destination_arg::OutputDestinationArg;
use buck2_core::fs::project::ProjectRoot;
use buck2_error::BuckErrorContext;
use buck2_error::buck2_error;
use buck2_error::internal_error;
use buck2_fs::async_fs_util;
use buck2_fs::error::IoResultExt;
use buck2_fs::fs_util;
use buck2_fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_fs::paths::abs_path::AbsPath;
use buck2_fs::paths::abs_path::AbsPathBuf;
use buck2_fs::paths::forward_rel_path::ForwardRelativePath;
use buck2_fs::working_dir::AbsWorkingDir;
use futures::TryStreamExt;

#[derive(Clone)]
struct CopyContext {
    // Any symlink pointing outside of this path would be copied/recreated as an absolute symlink to original target.
    // Should be canonicalized.
    relative_symlink_boundary: AbsNormPathBuf,
}

/// Given a list of targets built by this command, extracts a reasonable default output from the list and writes it
/// to the path given by `out`.
///
/// In order to extract a "reasonable default output", this function will bail if any of the following are true:
///  1. Multiple top-level targets were built, in which case the correct output to write is ambiguous,
///  2. A single top-level target was built, but it produced zero default outputs,
///  3. A single top-level target was built, but it produced more than two default outputs
///
/// Otherwise, we'll extract the single default output from the single top-level target and copy it to the output
/// path. If the given path is a directory then all output files will be copied inside of it.
///
/// Symbolic links are preserved. However, if a relative symlink within one of the outputs points outside that output,
/// it will be converted into an absolute link pointing to the same target as the original link.
/// On Windows platform relative symlinks are always converted to an absolute one, though the same rule applies to whether a copied
/// link would point to the same original target or a copied one.
///
/// As a special case, `--out -` is interpreted as `--out /dev/stdout` and allows multiple output files to be
/// written to it.
pub(super) async fn copy_to_out(
    targets: &[BuildTarget],
    root_path: &ProjectRoot,
    working_dir: &AbsWorkingDir,
    out: &OutputDestinationArg,
) -> buck2_error::Result<()> {
    struct OutputToBeCopied {
        from_path: AbsNormPathBuf,
        is_dir: bool,
    }

    let mut outputs_to_be_copied = Vec::new();
    for target in targets {
        let default_outputs: Vec<&BuildOutput> = target
            .outputs
            .iter()
            .filter(|output| {
                output
                    .providers
                    .as_ref()
                    .is_none_or(|p| p.default_info && !p.other)
            })
            .collect();

        let single_default_output = match default_outputs.len() {
            0 => {
                return Err(buck2_error!(
                    buck2_error::ErrorTag::CopyOutputs,
                    "target {} produced zero default outputs",
                    target.target
                ));
            }
            1 => &default_outputs[0],
            n => {
                return Err(buck2_error!(
                    buck2_error::ErrorTag::CopyOutputs,
                    "target {} produced {} outputs, choice of output is ambiguous",
                    target.target,
                    n
                ));
            }
        };

        let output_path = root_path
            .root()
            .join(ForwardRelativePath::new(&single_default_output.path)?);
        let output_meta = tokio::fs::metadata(&output_path)
            .await
            .buck_error_context("Error inspecting file metadata")?;
        let is_dir = output_meta.is_dir();

        outputs_to_be_copied.push(OutputToBeCopied {
            from_path: output_path,
            is_dir,
        });
    }

    match out {
        OutputDestinationArg::Stream => {
            // Check no output is a directory. We allow outputting any number of
            // files (including 0) to stdout.
            if let Some(dir_i) = outputs_to_be_copied.iter().position(|o| o.is_dir) {
                return Err(buck2_error!(
                    buck2_error::ErrorTag::CopyOutputs,
                    "target {} produces a default output that is a directory, and cannot be sent to stdout",
                    targets[dir_i].target,
                ));
            }
        }
        OutputDestinationArg::Path(..) => {
            // Check we are outputting exactly 1 target. Okay if directory.
            if outputs_to_be_copied.len() != 1 {
                return Err(buck2_error!(
                    buck2_error::ErrorTag::CopyOutputs,
                    "build command built multiple top-level targets, choice of output is ambiguous"
                ));
            }
        }
    }

    for to_be_copied in outputs_to_be_copied {
        match out {
            OutputDestinationArg::Stream => {
                let mut file = async_fs_util::open(&to_be_copied.from_path)
                    .await
                    .categorize_internal()?;
                tokio::io::copy(&mut file, &mut tokio::io::stdout())
                    .await
                    .map_err(convert_broken_pipe_error)?;
            }
            OutputDestinationArg::Path(path) => {
                let path = path.resolve(working_dir);
                if to_be_copied.is_dir {
                    let context = CopyContext {
                        relative_symlink_boundary: fs_util::canonicalize(&to_be_copied.from_path)
                            .categorize_internal()?,
                    };
                    copy_directory(&to_be_copied.from_path, &path, &context).await?;
                } else {
                    copy_file(&to_be_copied.from_path, &path).await?;
                }
            }
        }
    }

    Ok(())
}

fn copy_symlink<P: AsRef<AbsPath>, Q: AsRef<AbsPath>>(
    src_path: P,
    dst_path: Q,
    context: &CopyContext,
) -> buck2_error::Result<()> {
    // Make symlinks overwrite items which were already present at destination path
    fs_util::remove_all(&dst_path)
        .categorize_internal()
        .buck_error_context(format!(
            "Removing pre-existing item at path {:?}",
            src_path.as_ref()
        ))?;
    let symlink_target_abs_path = fs_util::canonicalize(src_path.as_ref())
        .categorize_internal()
        .buck_error_context(format!(
            "Resolving symlink to be copied {:?}",
            src_path.as_ref()
        ))?;
    // Now recreate the symlink
    let symlink_target = {
        if symlink_target_abs_path.starts_with(&context.relative_symlink_boundary) {
            // Symlink is not pointing outside the original output we are copying.
            // Just keep it as it is.
            fs_util::read_link(&src_path)
                .categorize_internal()
                .buck_error_context(format!(
                    "Reading value of a symlink to be copied {:?}",
                    src_path.as_ref()
                ))?
        } else {
            // Force "copied" symlink to be absolute as it points outside of original output we are copying.
            symlink_target_abs_path.into_path_buf()
        }
    };
    fs_util::symlink(&symlink_target, &dst_path)
        .categorize_internal()
        .buck_error_context(format!(
            "Creating symlink at {:?} pointing to {:?}",
            dst_path.as_ref(),
            &symlink_target
        ))?;

    Ok(())
}

/// Recursively copies a directory to the output path, rooted at `dst`.
#[async_recursion::async_recursion]
async fn copy_directory<P, Q>(src: P, dst: Q, context: &CopyContext) -> buck2_error::Result<()>
where
    P: AsRef<AbsPath> + std::marker::Send,
    Q: AsRef<AbsPath> + std::marker::Send + std::marker::Copy + std::marker::Sync,
{
    tokio::fs::create_dir_all(dst.as_ref()).await?;
    let stream = tokio_stream::wrappers::ReadDirStream::new(
        tokio::fs::read_dir(src.as_ref())
            .await
            .buck_error_context(format!("reading directory {:?}", src.as_ref()))?,
    )
    .err_into::<buck2_error::Error>();

    stream
        .try_for_each(|entry| async move {
            let entry_source_path = AbsPathBuf::new(entry.path())?;
            let entry_destination_path = dst.as_ref().join(entry.file_name());
            let file_type = entry.file_type().await?;
            if file_type.is_dir() {
                copy_directory(&entry_source_path, &entry_destination_path, context)
                    .await
                    .buck_error_context(format!("Copying subdirectory {:?}", entry.path()))
            } else if file_type.is_symlink() {
                let copy_context = context.clone();
                tokio::task::spawn_blocking(move || {
                    copy_symlink(&entry_source_path, &entry_destination_path, &copy_context)
                })
                .await
                .buck_error_context(format!("Copying symlink {:?}", &entry.path()))?
            } else {
                tokio::fs::copy(&entry.path(), &entry_destination_path)
                    .await
                    .buck_error_context(format!("Copying file {:?}", entry.path()))
                    .map(|_| ())
            }
        })
        .await?;

    Ok(())
}

async fn copy_file(src: &Path, dst: &Path) -> buck2_error::Result<()> {
    if let Some(parent) = dst.parent() {
        if !parent.exists() {
            tokio::fs::create_dir_all(parent).await?;
        }
    }
    let dest_path = match dst.is_dir() {
        true => Cow::Owned(
            dst.join(
                src.file_name()
                    .ok_or_else(|| internal_error!("Failed getting output name"))?,
            ),
        ),
        false => Cow::Borrowed(dst),
    };

    // NOTE: We don't do the overwrite since we might be writing to e.g. a pipe here and we can't
    // do an atomic move into it.
    match tokio::fs::copy(src, &dest_path).await {
        Ok(..) => Ok(()),
        Err(e) if e.raw_os_error() == Some(libc::ETXTBSY) => {
            let dir = dest_path
                .parent()
                .ok_or_else(|| internal_error!("Output path has no parent"))?;
            let mut tmp_name = dest_path
                .file_name()
                .ok_or_else(|| internal_error!("Output path has no file name"))?
                .to_owned();
            tmp_name.push(".buck2.tmp");
            let tmp_path = dir.join(tmp_name);
            tokio::fs::copy(src, &tmp_path).await?;
            tokio::fs::rename(&tmp_path, dest_path).await?;
            Ok(())
        }
        Err(e) => Err(convert_broken_pipe_error(e)),
    }
}

fn convert_broken_pipe_error(e: io::Error) -> buck2_error::Error {
    buck2_error::Error::from(ClientIoError::from(e))
        .context("Error writing build artifact to --out")
}

#[cfg(test)]
mod tests {
    #[cfg(unix)]
    use std::path::PathBuf;

    use buck2_fs::fs_util::uncategorized as fs_util;
    use tempfile::TempDir;

    use super::*;

    #[tokio::test]
    async fn test_copy_directory() -> buck2_error::Result<()> {
        let src_dir = tempfile::tempdir()?;
        // <tmp>
        // ├── bar
        // │    ├── qux
        // │    │    └── buzz
        // │    ├── foo_in_bar -> ../foo
        // │    └── some_file
        // ├── bax -> bar/qux
        // ├── foo
        // ├── foo_abs -> <tmp>/foo
        // └── fool -> foo
        std::fs::create_dir_all(src_dir.path().join("bar/qux"))?;
        std::fs::write(src_dir.path().join("foo"), "some content")?;
        std::fs::write(src_dir.path().join("bar/some_file"), "more content")?;
        std::fs::write(src_dir.path().join("bar/qux/buzz"), "even more")?;
        let fool_path = src_dir.path().join("fool");
        let foo_abs_path = src_dir.path().join("foo_abs");
        let foo_in_bar_path = src_dir.path().join("bar/foo_in_bar");
        let bax_path = src_dir.path().join("bax");
        #[cfg(unix)]
        {
            std::os::unix::fs::symlink("foo", &fool_path)?;
            std::os::unix::fs::symlink(src_dir.path().join("foo"), &foo_abs_path)?;
            std::os::unix::fs::symlink("bar/qux", &bax_path)?;
            std::os::unix::fs::symlink("../foo", &foo_in_bar_path)?;
        }
        #[cfg(windows)]
        {
            std::os::windows::fs::symlink_file("foo", &fool_path)?;
            std::os::windows::fs::symlink_file(src_dir.path().join("foo"), &foo_abs_path)?;
            std::os::windows::fs::symlink_dir("bar\\qux", &bax_path)?;
            std::os::windows::fs::symlink_file("..\\foo", &foo_in_bar_path)?;
        }

        let dst_dir = tempfile::tempdir()?;
        let src_path = AbsPath::new(src_dir.path())?;
        let dst_path = AbsPath::new(dst_dir.path())?;
        let copy_context = CopyContext {
            relative_symlink_boundary: fs_util::canonicalize(src_path)?,
        };
        copy_directory(src_path, dst_path, &copy_context).await?;

        assert!(dst_dir.path().join("foo").is_file());
        assert!(dst_dir.path().join("bar/some_file").is_file());
        assert!(dst_dir.path().join("bar/qux/buzz").is_file());

        let copied_fool_path = std::fs::read_link(dst_dir.path().join("fool"))?;
        #[cfg(unix)]
        {
            assert_eq!(PathBuf::from("foo"), copied_fool_path);
        }
        #[cfg(windows)]
        {
            // Symlink value is canonicalized on Windows
            let dst_dir_canon_path = fs_util::canonicalize(dst_path)?;
            assert_eq!(dst_dir_canon_path.as_path().join("foo"), copied_fool_path);
        }

        let copied_foo_abs_path = std::fs::read_link(dst_dir.path().join("foo_abs"))?;
        #[cfg(unix)]
        {
            assert_eq!(src_dir.path().join("foo"), copied_foo_abs_path);
        }
        #[cfg(windows)]
        {
            // Symlink value is canonicalized on Windows
            let src_dir_canon_path = fs_util::canonicalize(src_path)?;
            assert_eq!(
                src_dir_canon_path.as_path().join("foo"),
                copied_foo_abs_path
            );
        }

        let copied_bax_path = std::fs::read_link(dst_dir.path().join("bax"))?;
        #[cfg(unix)]
        {
            assert_eq!(PathBuf::from("bar/qux"), copied_bax_path);
        }
        #[cfg(windows)]
        {
            // Symlink value is canonicalized on Windows
            let dst_dir_canon_path = fs_util::canonicalize(dst_path)?;
            assert_eq!(
                dst_dir_canon_path.as_path().join("bar\\qux"),
                copied_bax_path
            );
        }

        let copied_foo_in_bar_path = std::fs::read_link(dst_dir.path().join("bar/foo_in_bar"))?;
        #[cfg(unix)]
        {
            assert_eq!(PathBuf::from("../foo"), copied_foo_in_bar_path);
        }
        #[cfg(windows)]
        {
            // Symlink value is canonicalized on Windows
            let dst_dir_canon_path = fs_util::canonicalize(dst_path)?;
            assert_eq!(
                dst_dir_canon_path.as_path().join("foo"),
                copied_foo_in_bar_path
            );
        }

        // Second time to check everything overwrites fine
        copy_directory(src_path, dst_path, &copy_context)
            .await
            .buck_error_context("copy second time")?;

        Ok(())
    }

    #[tokio::test]
    async fn test_copy_directory_with_symlink_pointing_externally() -> buck2_error::Result<()> {
        let src_dir = tempfile::tempdir()?;
        let src_path = fs_util::canonicalize(AbsPath::new(src_dir.path())?)?;
        // <tmp>
        // ├── qux
        // │    ├── foo -> ../foo
        // │    └── bar -> ../bar
        // ├── bar
        // │    └── baz
        // └── foo
        std::fs::write(src_path.as_path().join("foo"), "some content")?;
        std::fs::create_dir_all(src_path.as_path().join("bar"))?;
        std::fs::write(src_path.as_path().join("bar/baz"), "some content")?;
        std::fs::create_dir_all(src_path.as_path().join("qux"))?;
        let foo_link_path = src_path.as_path().join("qux/foo");
        let bar_link_path = src_path.as_path().join("qux/bar");
        #[cfg(unix)]
        {
            std::os::unix::fs::symlink("../foo", &foo_link_path)?;
            std::os::unix::fs::symlink("../bar", &bar_link_path)?;
        }
        #[cfg(windows)]
        {
            std::os::windows::fs::symlink_file("..\\foo", &foo_link_path)?;
            std::os::windows::fs::symlink_dir("..\\bar", &bar_link_path)?;
        }
        let dst_dir = tempfile::tempdir()?;
        let dst_path = AbsPath::new(dst_dir.path())?;
        let copy_context = CopyContext {
            relative_symlink_boundary: src_path.join(ForwardRelativePath::unchecked_new("qux")),
        };
        copy_directory(&src_path, dst_path, &copy_context).await?;

        // Check both symlinks are valid and are absolute.

        let copied_foo_target = std::fs::read_link(dst_dir.path().join("qux/foo"))?;
        assert_eq!(src_path.as_path().join("foo"), copied_foo_target);

        let copied_bar_target = std::fs::read_link(dst_dir.path().join("qux/bar"))?;
        assert_eq!(src_path.as_path().join("bar"), copied_bar_target);

        Ok(())
    }

    #[tokio::test]
    async fn test_copy_symlink() -> buck2_error::Result<()> {
        test_copy_symlink_parametrized(&|_| Ok(()), false)?;
        test_copy_symlink_parametrized(&|_| Ok(()), true)?;
        // Check that symlink overwrites regular file in output directory
        let setup_file_to_overwrite = |dst_dir: &TempDir| {
            std::fs::write(dst_dir.path().join("foo_copy"), "some content")?;
            Ok(())
        };
        test_copy_symlink_parametrized(&setup_file_to_overwrite, false)?;
        test_copy_symlink_parametrized(&setup_file_to_overwrite, true)?;
        // Check that symlink overwrites directory in output directory
        let setup_directory_with_content_to_overwrite = |dst_dir: &TempDir| {
            std::fs::create_dir(dst_dir.path().join("foo_copy"))?;
            std::fs::write(dst_dir.path().join("foo_copy/some_file"), "some content")?;
            Ok(())
        };
        test_copy_symlink_parametrized(&setup_directory_with_content_to_overwrite, false)?;
        test_copy_symlink_parametrized(&setup_directory_with_content_to_overwrite, true)?;
        Ok(())
    }

    fn test_copy_symlink_parametrized(
        prepare_dst_dir: &dyn Fn(&TempDir) -> buck2_error::Result<()>,
        is_directory_symlink: bool,
    ) -> buck2_error::Result<()> {
        let src_dir = tempfile::tempdir()?;
        let src_symlink_target_path = src_dir.path().join("bar");
        let src_symlink_path = src_dir.path().join("foo");

        if is_directory_symlink {
            std::fs::create_dir(&src_symlink_target_path)?;
            #[cfg(windows)]
            {
                std::os::windows::fs::symlink_dir("bar", &src_symlink_path)?;
            }
        } else {
            std::fs::write(&src_symlink_target_path, "some content")?;
            #[cfg(windows)]
            {
                std::os::windows::fs::symlink_file("bar", &src_symlink_path)?;
            }
        }
        #[cfg(unix)]
        {
            std::os::unix::fs::symlink("bar", &src_symlink_path)?;
        }

        let dst_dir = tempfile::tempdir()?;
        prepare_dst_dir(&dst_dir)?;

        let dst_symlink_path = dst_dir.path().join("foo_copy");

        copy_symlink(
            AbsPath::new(&src_symlink_path)?,
            AbsPath::new(&dst_symlink_path)?,
            &CopyContext {
                relative_symlink_boundary: fs_util::canonicalize(AbsPath::new(src_dir.path())?)?,
            },
        )?;

        let target = std::fs::read_link(&dst_symlink_path)?;
        #[cfg(unix)]
        {
            assert_eq!(PathBuf::from("bar"), target);
        }
        #[cfg(windows)]
        {
            // Symlink value is canonicalized on Windows
            let dst_dir_canon_path = fs_util::canonicalize(AbsPath::new(dst_dir.path())?)?;
            assert_eq!(dst_dir_canon_path.as_path().join("bar"), target);
        }

        Ok(())
    }

    #[cfg(unix)]
    mod unix {

        use assert_matches::assert_matches;

        use super::super::*;

        #[tokio::test]
        async fn test_copy_file() -> buck2_error::Result<()> {
            let dir = tempfile::tempdir()?;
            let out = dir.path().join("sleep");

            let res = buck2_util::process::async_background_command("cp")
                .arg(Path::new("/bin/sleep"))
                .arg(&out)
                .spawn()?
                .wait()
                .await?;

            assert!(res.success());

            let mut proc = buck2_util::process::async_background_command(&out)
                .arg("10000")
                .kill_on_drop(true)
                .spawn()
                .buck_error_context("Error spawning")?;

            // This will fail if we don't handle ETXTBSY.
            copy_file(Path::new("/bin/sleep"), &out).await?;

            // Check that our sleep didn't end
            assert_matches!(proc.try_wait(), Ok(None));

            Ok(())
        }
    }
}
