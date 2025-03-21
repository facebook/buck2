/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;
use std::io::Write;
use std::path::Path;

use async_recursion::async_recursion;
use async_trait::async_trait;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::dice::file_ops::DiceFileOps;
use buck2_common::file_ops::FileOps;
use buck2_common::file_ops::RawPathMetadata;
use buck2_common::file_ops::RawSymlink;
use buck2_common::io::fs::FsIoProvider;
use buck2_common::io::IoProvider;
use buck2_core::cells::CellResolver;
use buck2_core::fs::paths::abs_path::AbsPath;
use buck2_core::fs::paths::file_name::FileName;
use buck2_core::fs::paths::file_name::FileNameBuf;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_execute::digest_config::HasDigestConfig;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;
use buck2_server_ctx::stdout_partial_output::StdoutPartialOutput;
use buck2_server_ctx::template::run_server_command;
use buck2_server_ctx::template::ServerCommandTemplate;
use buck2_util::commas::commas;
use dice::DiceTransaction;
use dupe::Dupe;
use gazebo::variants::VariantName;

use crate::ctx::ServerCommandContext;

pub(crate) async fn file_status_command(
    ctx: &ServerCommandContext<'_>,
    partial_result_dispatcher: PartialResultDispatcher<buck2_cli_proto::StdoutBytes>,
    req: buck2_cli_proto::FileStatusRequest,
) -> buck2_error::Result<buck2_cli_proto::GenericResponse> {
    run_server_command(
        FileStatusServerCommand { req },
        ctx,
        partial_result_dispatcher,
    )
    .await
}
struct FileStatusServerCommand {
    req: buck2_cli_proto::FileStatusRequest,
}

struct FileStatusResult<'a> {
    /// Number of things we check
    checked: usize,
    /// Number of ones that were bad
    bad: usize,
    /// Whether to write matches
    show_matches: bool,
    stdout: StdoutPartialOutput<'a>,
}

impl FileStatusResult<'_> {
    fn checking(&mut self) {
        self.checked += 1;
    }

    fn report<T>(
        &mut self,
        kind: &str,
        path: &ProjectRelativePath,
        fs: &T,
        dice: &T,
    ) -> buck2_error::Result<()>
    where
        T: PartialEq + fmt::Display + ?Sized,
    {
        if fs != dice {
            writeln!(
                self.stdout,
                "MISMATCH: {} at {}: fs = {}, dice = {}",
                kind, path, fs, dice,
            )?;
            self.bad += 1;
        } else if self.show_matches {
            writeln!(self.stdout, "Match: {} at {}: {}", kind, path, fs)?;
        }

        Ok(())
    }
}

#[derive(PartialEq)]
struct DirList(Vec<FileNameBuf>);

impl fmt::Display for DirList {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut comma = commas();
        for path in &self.0 {
            comma(f)?;
            write!(f, "{}", path)?;
        }
        Ok(())
    }
}

#[async_trait]
impl ServerCommandTemplate for FileStatusServerCommand {
    type StartEvent = buck2_data::FileStatusCommandStart;
    type EndEvent = buck2_data::FileStatusCommandEnd;
    type Response = buck2_cli_proto::GenericResponse;
    type PartialResult = buck2_cli_proto::StdoutBytes;

    async fn command(
        &self,
        server_ctx: &dyn ServerCommandContextTrait,
        mut stdout: PartialResultDispatcher<Self::PartialResult>,
        mut ctx: DiceTransaction,
    ) -> buck2_error::Result<Self::Response> {
        let cell_resolver = &ctx.get_cell_resolver().await?;
        let project_root = server_ctx.project_root();
        let digest_config = ctx.global_data().get_digest_config();

        let io = &FsIoProvider::new(project_root.dupe(), digest_config.cas_digest_config());
        let stdout = stdout.as_writer();

        let mut result = FileStatusResult {
            checked: 0,
            bad: 0,
            show_matches: self.req.show_matches,
            stdout,
        };

        let mut stderr = server_ctx.stderr()?;

        for path in &self.req.paths {
            let path = project_root.relativize_any(AbsPath::new(Path::new(path))?)?;
            writeln!(&mut stderr, "Check file status: {}", path)?;
            let result = &mut result;
            ctx.with_linear_recompute(|ctx| async move {
                check_file_status(&DiceFileOps(&ctx), cell_resolver, io, &path, result).await
            })
            .await?;
        }
        if result.bad != 0 {
            Err(buck2_error::buck2_error!(
                buck2_error::ErrorTag::Tier0,
                "Failed with {} mismatches",
                result.bad
            ))
        } else {
            writeln!(
                &mut stderr,
                "No mismatches detected ({} entries checked)",
                result.checked
            )?;
            Ok(buck2_cli_proto::GenericResponse {})
        }
    }

    fn is_success(&self, _response: &Self::Response) -> bool {
        // No response if we failed.
        true
    }
}

#[async_recursion]
async fn check_file_status(
    file_ops: &dyn FileOps,
    cell_resolver: &CellResolver,
    io: &dyn IoProvider,
    path: &ProjectRelativePath,
    result: &mut FileStatusResult,
) -> buck2_error::Result<()> {
    result.checking();

    let cell_path = cell_resolver.get_cell_path(path)?;
    if file_ops.is_ignored(cell_path.as_ref()).await?.is_ignored() {
        return Ok(());
    }

    let fs_metadata = io.read_path_metadata_if_exists(path.to_owned()).await?;

    let dice_metadata = file_ops
        .read_path_metadata_if_exists(cell_path.as_ref())
        .await?;

    let (fs_metadata, dice_metadata) = match (&fs_metadata, &dice_metadata) {
        (Some(fs), Some(dice)) => (fs, dice),
        (fs, dice) => {
            result.report("existence", path, &fs.is_some(), &dice.is_some())?;
            return Ok(());
        }
    };

    result.report(
        "entry kind",
        path,
        fs_metadata.variant_name(),
        dice_metadata.variant_name(),
    )?;

    match (fs_metadata, dice_metadata) {
        (
            RawPathMetadata::Symlink {
                at: fs_at,
                to: fs_to,
            },
            RawPathMetadata::Symlink {
                at: dice_at,
                to: dice_to,
            },
        ) => {
            let dice_at = cell_resolver.resolve_path(dice_at.as_ref().as_ref())?;
            result.report("symlink component location", path, fs_at, &dice_at)?;

            result.report(
                "symlink destination kind",
                path,
                fs_to.variant_name(),
                dice_to.variant_name(),
            )?;

            match (fs_to, dice_to) {
                (RawSymlink::Relative(fs_rel), RawSymlink::Relative(dice_rel)) => {
                    let dice_rel = cell_resolver.resolve_path(dice_rel.as_ref().as_ref())?;
                    result.report("relative symlink destination", path, fs_rel, &dice_rel)?;
                }
                (RawSymlink::External(fs_ext), RawSymlink::External(dice_ext)) => {
                    result.report("external symlink destination", path, fs_ext, dice_ext)?;
                }
                _ => {
                    // Reported earlier
                }
            };
        }
        (RawPathMetadata::File(fs_file), RawPathMetadata::File(dice_file)) => {
            result.report("file metadata", path, fs_file, dice_file)?;
        }
        (RawPathMetadata::Directory, RawPathMetadata::Directory) => {
            let fs_read_dir = io.read_dir(path.to_owned()).await?;
            let dice_read_dir = file_ops.read_dir(cell_path.as_ref()).await?;

            // No point checking file types here, we'll do that when we inspect them.
            let mut fs_names = fs_read_dir
                .iter()
                .map(|f| buck2_error::Ok(FileName::new(&f.file_name)?.to_owned()))
                .collect::<Result<Vec<_>, _>>()?;

            let mut dice_names = dice_read_dir
                .included
                .iter()
                .map(|e| e.file_name.clone())
                .collect::<Vec<_>>();

            fs_names.sort();
            dice_names.sort();

            let fs_names = DirList(fs_names);
            let dice_names = DirList(dice_names);

            result.report("directory contents", path, &fs_names, &dice_names)?;

            for file_name in &fs_names.0 {
                check_file_status(file_ops, cell_resolver, io, &path.join(file_name), result)
                    .await?;
            }
        }
        (_, _) => {
            // Reported earlier
        }
    }

    Ok(())
}
