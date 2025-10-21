/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use buck2_core::cells::cell_path::CellPathRef;
use dice::DiceComputations;
use dupe::Dupe;
use futures::FutureExt;
use futures::future::BoxFuture;

use crate::dice::cells::HasCellResolver;
use crate::file_ops::dice::DiceFileComputations;
use crate::file_ops::metadata::RawPathMetadata;
use crate::ignores::file_ignores::FileIgnoreResult;
use crate::io::DirectoryDoesNotExistSuggestion;
use crate::io::ReadDirError;

#[derive(Debug, buck2_error::Error)]
pub(crate) enum FileOpsError {
    #[error("File not found: {0}")]
    // File not found errors are not inherently always user errors; however, we only use these
    // methods with source files, and in that case this is correct
    #[buck2(input)]
    #[buck2(tag = IoNotFound)]
    FileNotFound(String),
}

pub enum FileReadError {
    NotFound(String),
    Buck(buck2_error::Error),
}

impl FileReadError {
    pub fn with_package_context_information(self, package_path: String) -> buck2_error::Error {
        match self {
            FileReadError::NotFound(path) => FileOpsError::FileNotFound(format!(
                "`{path}`.\n     Included in `{package_path}` but does not exist"
            ))
            .into(),
            FileReadError::Buck(err) => err.dupe(),
        }
    }

    pub fn without_package_context_information(self) -> buck2_error::Error {
        match self {
            FileReadError::NotFound(path) => FileOpsError::FileNotFound(path).into(),
            FileReadError::Buck(err) => err.dupe(),
        }
    }
}

pub trait FileReadErrorContext<T> {
    fn with_package_context_information(self, package_path: String) -> buck2_error::Result<T>;
    fn without_package_context_information(self) -> buck2_error::Result<T>;
}

impl<T> FileReadErrorContext<T> for std::result::Result<T, FileReadError> {
    fn with_package_context_information(self, package_path: String) -> buck2_error::Result<T> {
        self.map_err(|e| e.with_package_context_information(package_path))
    }

    fn without_package_context_information(self) -> buck2_error::Result<T> {
        self.map_err(|e| e.without_package_context_information())
    }
}

fn did_you_mean<'a>(value: &str, variants: impl IntoIterator<Item = &'a str>) -> Option<&'a str> {
    if value.is_empty() {
        return None;
    }

    const MAX_LEVENSHTEIN_DISTANCE: usize = 2;

    variants
        .into_iter()
        .map(|v| (v, strsim::levenshtein(value, v)))
        .filter(|(_, dist)| *dist <= MAX_LEVENSHTEIN_DISTANCE)
        .min_by_key(|(_v, sim)| *sim)
        .map(|(v, _)| v)
}

pub(super) fn extended_ignore_error<'a>(
    ctx: &'a mut DiceComputations<'_>,
    path: CellPathRef<'a>,
) -> BoxFuture<'a, Option<ReadDirError>> {
    async move {
        match path.parent() {
            Some(parent) => match DiceFileComputations::read_dir_ext(ctx, parent).await {
                Ok(v) => {
                    // the parent can be read fine, check if this path is ignored first (if it's ignored it won't appear in the read_dir results).
                    if let Ok(FileIgnoreResult::Ignored(reason)) =
                        DiceFileComputations::is_ignored(ctx, path).await
                    {
                        return Some(ReadDirError::DirectoryIsIgnored(path.to_owned(), reason));
                    }

                    match path.path().file_name() {
                        Some(file_name) if !v.contains(file_name) => {
                            let mut cell_suggestion = vec![];
                            if let Ok(cell_resolver) = ctx.get_cell_resolver().await {
                                for (cell_name, _) in cell_resolver.cells() {
                                    let cell_path = CellPathRef::new(cell_name, path.path());

                                    if DiceFileComputations::read_path_metadata_if_exists(
                                        ctx, cell_path,
                                    )
                                    .await
                                    .is_ok_and(|result| result.is_some())
                                    {
                                        cell_suggestion.push(cell_path.to_string());
                                    }
                                }
                            }

                            if !cell_suggestion.is_empty() {
                                return Some(ReadDirError::DirectoryDoesNotExist {
                                    path: path.to_owned(),
                                    suggestion: DirectoryDoesNotExistSuggestion::Cell(
                                        cell_suggestion,
                                    ),
                                });
                            } else if let Some(suggestion) = did_you_mean(
                                file_name.as_str(),
                                v.included.iter().map(|x| x.file_name.as_str()),
                            ) {
                                return Some(ReadDirError::DirectoryDoesNotExist {
                                    path: path.to_owned(),
                                    suggestion: DirectoryDoesNotExistSuggestion::Typo(
                                        match parent.path().join_normalized(suggestion) {
                                            Ok(p) => p.as_str().to_owned(),
                                            Err(_) => suggestion.to_owned()
                                        }
                                    ),
                                });
                            }

                            return Some(ReadDirError::DirectoryDoesNotExist {
                                path: path.to_owned(),
                                suggestion: DirectoryDoesNotExistSuggestion::NoSuggestion,
                            });
                        }
                        _ => {}
                    }

                    match DiceFileComputations::read_path_metadata(ctx, path).await {
                        Ok(RawPathMetadata::Directory) => {}
                        Ok(RawPathMetadata::Symlink { .. }) => {
                            // not sure how we should handle symlink here, if it's pointing to a dir is it potentially correct?
                        }
                        Err(_) => {
                            // we ignore this, we don't know what the error is and so we can't be sure that
                            // it's not missing important data that the original error would have.
                        }
                        Ok(RawPathMetadata::File(..)) => {
                            return Some(ReadDirError::NotADirectory(
                                path.to_owned(),
                                "file".to_owned(),
                            ));
                        }
                    }

                    None
                }
                Err(e) => Some(e),
            },
            None => None,
        }
    }
    .boxed()
}
