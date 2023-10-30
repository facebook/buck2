/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Cow;
use std::io;
use std::path::Path;

use anyhow::Context;
use buck2_cli_proto::build_target::BuildOutput;
use buck2_cli_proto::BuildTarget;
use buck2_client_ctx::exit_result::ClientIoError;
use buck2_client_ctx::output_destination_arg::OutputDestinationArg;
use buck2_core::fs::async_fs_util;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePath;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::working_dir::WorkingDir;
use futures::TryStreamExt;

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
/// As a special case, `--out -` is interpreted as `--out /dev/stdout` and allows multiple output files to be
/// written to it.
pub(super) async fn copy_to_out(
    targets: &[BuildTarget],
    root_path: &ProjectRoot,
    working_dir: &WorkingDir,
    out: &OutputDestinationArg,
) -> anyhow::Result<()> {
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
                    .map_or(true, |p| p.default_info && !p.other)
            })
            .collect();

        let single_default_output = match default_outputs.len() {
            0 => {
                return Err(anyhow::anyhow!(
                    "target {} produced zero default outputs",
                    target.target
                ));
            }
            1 => &default_outputs[0],
            n => {
                return Err(anyhow::anyhow!(
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
            .context("Error inspecting file metadata")?;
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
                return Err(anyhow::anyhow!(
                    "target {} produces a default output that is a directory, and cannot be sent to stdout",
                    targets[dir_i].target,
                ));
            }
        }
        OutputDestinationArg::Path(..) => {
            // Check we are outputting exactly 1 target. Okay if directory.
            if outputs_to_be_copied.len() != 1 {
                return Err(anyhow::anyhow!(
                    "build command built multiple top-level targets, choice of output is ambiguous"
                ));
            }
        }
    }

    for to_be_copied in outputs_to_be_copied {
        match out {
            OutputDestinationArg::Stream => {
                let mut file = async_fs_util::open(&to_be_copied.from_path).await?;
                tokio::io::copy(&mut file, &mut tokio::io::stdout())
                    .await
                    .map_err(convert_broken_pipe_error)?;
            }
            OutputDestinationArg::Path(path) => {
                let path = path.resolve(working_dir);
                if to_be_copied.is_dir {
                    copy_directory(&to_be_copied.from_path, &path).await?;
                } else {
                    copy_file(&to_be_copied.from_path, &path).await?;
                }
            }
        }
    }

    Ok(())
}

/// Recursively copies a directory to the output path, rooted at `dst`.
#[async_recursion::async_recursion]
async fn copy_directory(src: &Path, dst: &Path) -> anyhow::Result<()> {
    tokio::fs::create_dir_all(dst).await?;
    let stream = tokio_stream::wrappers::ReadDirStream::new(
        tokio::fs::read_dir(src)
            .await
            .context(format!("reading directory {:?}", src))?,
    )
    .err_into::<anyhow::Error>();
    stream
        .try_for_each(|entry| async move {
            if entry.file_type().await?.is_dir() {
                copy_directory(&entry.path(), &dst.join(entry.file_name()))
                    .await
                    .context(format!("copying subdirectory {:?}", entry.path()))
            } else {
                tokio::fs::copy(&entry.path(), &dst.join(entry.file_name()))
                    .await
                    .context(format!("copying file {:?}", entry.path()))
                    .map(|_| ())
            }
        })
        .await?;

    Ok(())
}

async fn copy_file(src: &Path, dst: &Path) -> anyhow::Result<()> {
    if let Some(parent) = dst.parent() {
        if !parent.exists() {
            return Err(anyhow::anyhow!(
                "Directory `{}` does not exist",
                parent.display()
            ));
        }
    }
    let dest_path = match dst.is_dir() {
        true => Cow::Owned(dst.join(src.file_name().context("Failed getting output name")?)),
        false => Cow::Borrowed(dst),
    };

    // NOTE: We don't do the overwrite since we might be writing to e.g. a pipe here and we can't
    // do an atomic move into it.
    match tokio::fs::copy(src, &dest_path).await {
        Ok(..) => Ok(()),
        Err(e) if e.raw_os_error() == Some(libc::ETXTBSY) => {
            let dir = dest_path.parent().context("Output path has no parent")?;
            let mut tmp_name = dest_path
                .file_name()
                .context("Output path has no file name")?
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

fn convert_broken_pipe_error(e: io::Error) -> anyhow::Error {
    anyhow::Error::new(ClientIoError(e)).context("Error writing build artifact to --out")
}

#[cfg(test)]
mod test {
    #[cfg(unix)]
    mod unix {
        use std::path::Path;

        use anyhow::Context;
        use assert_matches::assert_matches;
        use tokio::process::Command;

        use super::super::*;

        #[tokio::test]
        async fn test_copy_file() -> anyhow::Result<()> {
            let dir = tempfile::tempdir()?;
            let out = dir.path().join("sleep");

            let res = Command::new("cp")
                .arg(Path::new("/bin/sleep"))
                .arg(&out)
                .spawn()?
                .wait()
                .await?;

            assert!(res.success());

            let mut proc = Command::new(&out)
                .arg("10000")
                .kill_on_drop(true)
                .spawn()
                .context("Error spawning")?;

            // This will fail if we don't handle ETXTBSY.
            copy_file(Path::new("/bin/sleep"), &out).await?;

            // Check that our sleep didn't end
            assert_matches!(proc.try_wait(), Ok(None));

            Ok(())
        }
    }
}
