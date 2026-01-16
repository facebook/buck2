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

use allocative::Allocative;
use async_trait::async_trait;
use buck2_artifact::artifact::build_artifact::BuildArtifact;
use buck2_build_api::actions::Action;
use buck2_build_api::actions::ActionExecutionCtx;
use buck2_build_api::actions::UnregisteredAction;
use buck2_build_api::actions::box_slice_set::BoxSliceSet;
use buck2_build_api::actions::execute::action_executor::ActionExecutionKind;
use buck2_build_api::actions::execute::action_executor::ActionExecutionMetadata;
use buck2_build_api::actions::execute::action_executor::ActionOutputs;
use buck2_build_api::actions::execute::error::ExecuteError;
use buck2_build_api::artifact_groups::ArtifactGroup;
use buck2_build_api::interpreter::rule_defs::artifact::associated::AssociatedArtifacts;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_artifact_like::ValueAsInputArtifactLike;
use buck2_build_signals::env::WaitingData;
use buck2_core::category::CategoryRef;
use buck2_core::content_hash::ContentBasedPathHash;
use buck2_error::BuckErrorContext;
use buck2_execute::artifact::artifact_dyn::ArtifactDyn;
use buck2_execute::artifact_utils::ArtifactValueBuilder;
use buck2_execute::execute::command_executor::ActionExecutionTimingData;
use buck2_execute::materialize::materializer::CopiedArtifact;
use buck2_fs::paths::forward_rel_path::ForwardRelativePath;
use buck2_fs::paths::forward_rel_path::ForwardRelativePathBuf;
use dupe::Dupe;
use gazebo::prelude::*;
use indexmap::IndexSet;
use itertools::Itertools;
use starlark::values::OwnedFrozenValue;
use starlark::values::ValueError;
use starlark::values::dict::UnpackDictEntries;
use starlark_map::small_set::SmallSet;

use crate::actions::impls::copy::CopyMode;

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
enum SymlinkedDirError {
    #[error("Paths to symlink_dir must be non-overlapping, but got `{0}` and `{1}`")]
    OverlappingPaths(Box<ForwardRelativePath>, Box<ForwardRelativePath>),
    #[error("Paths to symlink_dir must not be empty")]
    EmptyPath,
    #[error("Only artifact inputs are supported in symlink_dir actions, got {0}")]
    UnsupportedInput(ArtifactGroup),
}

#[derive(Allocative)]
pub(crate) struct UnregisteredSymlinkedDirAction {
    copy: CopyMode,
    args: Vec<(ArtifactGroup, Box<ForwardRelativePath>)>,
    // All associated artifacts of inputs unioned together
    unioned_associated_artifacts: AssociatedArtifacts,
}

impl UnregisteredSymlinkedDirAction {
    /// Validate that no output path is duplicated or overlapping.
    /// Duplicates are easy - overlapping only happens with symlinks to directories (a bad idea),
    /// and would look like `a` and `a/b` both being given.
    fn validate_args(
        args: &mut [(ArtifactGroup, Box<ForwardRelativePath>)],
    ) -> buck2_error::Result<()> {
        // We sort the inputs. They are morally a set, so it shouldn't matter too much,
        // and this lets us implement the overlap check more easily.
        args.sort_by(|x, y| x.1.cmp(&y.1));

        for ((_, x), (_, y)) in args.iter().zip(args.iter().skip(1)) {
            if y.starts_with(x) {
                return Err(SymlinkedDirError::OverlappingPaths(x.clone(), y.clone()).into());
            }
        }
        if args.len() == 1 && args[0].1.is_empty() {
            return Err(SymlinkedDirError::EmptyPath.into());
        }

        for (g, _) in args.iter() {
            // TODO: Exclude other variants once they become available here. For now, this is a
            // noop.
            match g {
                ArtifactGroup::Artifact(..) | ArtifactGroup::Promise(..) => {}
                other => return Err(SymlinkedDirError::UnsupportedInput(other.dupe()).into()),
            };
        }

        Ok(())
    }

    // Map each artifact into an optional tuple of (artifact, path) and associated_artifacts, then collect
    // them into an optional tuple of vector and an index set respectively
    fn unpack_args<'v>(
        srcs: UnpackDictEntries<&'v str, ValueAsInputArtifactLike<'v>>,
    ) -> buck2_error::Result<(
        Vec<(ArtifactGroup, Box<ForwardRelativePath>)>,
        SmallSet<ArtifactGroup>,
    )> {
        // This assignment doesn't look like it should be necessary, but we get an error if we
        // don't do it.
        let len = srcs.entries.len();
        srcs.entries
            .into_iter()
            .map(|(k, as_artifact)| {
                let associates = as_artifact.0.get_associated_artifacts();
                buck2_error::Ok((
                    (
                        as_artifact.0.get_artifact_group()?,
                        ForwardRelativePathBuf::try_from(k.to_owned())
                            .buck_error_context("dict key must be a forward relative path")?
                            .into_box(),
                    ),
                    associates,
                ))
            })
            .fold_ok(
                (Vec::with_capacity(len), SmallSet::new()),
                |(mut aps, mut assocs), (ap, assoc)| {
                    aps.push(ap);
                    assoc.iter().flat_map(|v| v.iter()).for_each(|a| {
                        assocs.insert(a.dupe());
                    });
                    (aps, assocs)
                },
            )
    }

    pub(crate) fn new<'v>(
        copy: CopyMode,
        srcs: UnpackDictEntries<&'v str, ValueAsInputArtifactLike<'v>>,
    ) -> buck2_error::Result<Self> {
        let (mut args, unioned_associated_artifacts) = Self::unpack_args(srcs)
            // FIXME: This warning is talking about the Starlark-level argument name `srcs`.
            //        Once we use a proper Value parser this should all get cleaned up.
            .buck_error_context(
                ValueError::IncorrectParameterTypeNamed("srcs".to_owned()).to_string(),
            )?;
        // Overlapping check make sense for non-copy mode only.
        // When directories are copied into the same destination, the ordering defines how files are overwritten.
        match copy {
            CopyMode::Symlink => Self::validate_args(&mut args)?,
            CopyMode::Copy { .. } => (),
        };
        Ok(Self {
            copy,
            args,
            unioned_associated_artifacts: AssociatedArtifacts::from(unioned_associated_artifacts),
        })
    }

    pub(crate) fn unioned_associated_artifacts(&self) -> AssociatedArtifacts {
        self.unioned_associated_artifacts.dupe()
    }
}

impl UnregisteredAction for UnregisteredSymlinkedDirAction {
    fn register(
        self: Box<Self>,
        outputs: IndexSet<BuildArtifact>,
        _starlark_data: Option<OwnedFrozenValue>,
        _error_handler: Option<OwnedFrozenValue>,
    ) -> buck2_error::Result<Box<dyn Action>> {
        Ok(Box::new(SymlinkedDirAction {
            copy: self.copy,
            args: self.args,
            outputs: BoxSliceSet::from(outputs),
        }))
    }
}

#[derive(Debug, Allocative)]
struct SymlinkedDirAction {
    copy: CopyMode,
    args: Vec<(ArtifactGroup, Box<ForwardRelativePath>)>,
    outputs: BoxSliceSet<BuildArtifact>,
}

impl SymlinkedDirAction {
    fn output(&self) -> &BuildArtifact {
        self.outputs
            .iter()
            .next()
            .expect("a single artifact by construction")
    }
}

#[async_trait]
impl Action for SymlinkedDirAction {
    fn kind(&self) -> buck2_data::ActionKind {
        buck2_data::ActionKind::SymlinkedDir
    }

    fn inputs(&self) -> buck2_error::Result<Cow<'_, [ArtifactGroup]>> {
        Ok(Cow::Owned(self.args.iter().map(|x| x.0.dupe()).collect()))
    }

    fn outputs(&self) -> Cow<'_, [BuildArtifact]> {
        Cow::Borrowed(self.outputs.as_slice())
    }

    fn first_output(&self) -> &BuildArtifact {
        self.output()
    }

    fn category(&self) -> CategoryRef<'_> {
        CategoryRef::unchecked_new("symlinked_dir")
    }

    fn identifier(&self) -> Option<&str> {
        Some(self.output().get_path().path().as_str())
    }

    async fn execute(
        &self,
        ctx: &mut dyn ActionExecutionCtx,
    ) -> Result<(ActionOutputs, ActionExecutionMetadata), ExecuteError> {
        let fs = ctx.fs().fs();
        let temp_output = ctx.fs().resolve_build(
            self.output().get_path(),
            Some(&ContentBasedPathHash::for_output_artifact()),
        )?;
        let mut builder = ArtifactValueBuilder::new(fs, ctx.digest_config());
        let mut srcs = Vec::new();

        for (group, relative_dest) in &self.args {
            let (src_artifact, value) = ctx
                .artifact_values(group)
                .iter()
                .into_singleton()
                .buck_error_context("Input did not dereference to exactly one artifact")?;

            let src = src_artifact.resolve_path(
                ctx.fs(),
                if src_artifact.has_content_based_path() {
                    Some(value.content_based_path_hash())
                } else {
                    None
                }
                .as_ref(),
            )?;
            let temp_dest = temp_output.join(relative_dest);

            match self.copy {
                CopyMode::Copy {
                    executable_bit_override,
                } => {
                    let dest_entry = builder.add_copied(
                        value,
                        src.as_ref(),
                        temp_dest.as_ref(),
                        executable_bit_override,
                    )?;
                    srcs.push((src, relative_dest, dest_entry.map_dir(|d| d.as_immutable())));
                }
                CopyMode::Symlink => {
                    builder.add_symlinked(value, src, temp_dest.as_ref())?;
                }
            };
        }

        let value = builder.build(&temp_output.as_ref())?;
        let actual_output = ctx.fs().resolve_build(
            self.output().get_path(),
            if self.output().get_path().is_content_based_path() {
                Some(value.content_based_path_hash())
            } else {
                None
            }
            .as_ref(),
        )?;
        let srcs = srcs
            .into_iter()
            .map(|(src, relative_dest, dest_entry)| {
                CopiedArtifact::new(
                    src,
                    actual_output.join(relative_dest),
                    dest_entry,
                    match self.copy {
                        CopyMode::Copy {
                            executable_bit_override,
                        } => executable_bit_override,
                        CopyMode::Symlink => None,
                    },
                )
            })
            .collect_vec();

        ctx.materializer()
            .declare_copy(actual_output, value.dupe(), srcs)
            .await?;
        Ok((
            ActionOutputs::from_single(self.output().get_path().dupe(), value),
            ActionExecutionMetadata {
                execution_kind: ActionExecutionKind::Simple,
                timing: ActionExecutionTimingData::default(),
                input_files_bytes: None,
                waiting_data: WaitingData::new(),
            },
        ))
    }
}

#[cfg(test)]
mod tests {
    use buck2_artifact::artifact::artifact_type::Artifact;
    use buck2_artifact::artifact::source_artifact::SourceArtifact;
    use buck2_core::package::source_path::SourcePath;

    use super::*;

    fn mk_artifact() -> Artifact {
        let buck_path = SourcePath::testing_new("cell//pkg", "");
        Artifact::from(SourceArtifact::new(buck_path))
    }

    // TODO: This needs proper tests, but right now it's kind of a pain to get the
    //       action framework up and running to test actions
    #[test]
    fn symlinked_dir_test() {}

    #[test]
    fn test_symlinked_dir_validation() {
        fn validate(paths: &[&str]) -> buck2_error::Result<()> {
            let a = ArtifactGroup::Artifact(mk_artifact());
            let mut xs = paths.map(|x| {
                (
                    a.dupe(),
                    ForwardRelativePath::new(x).unwrap().to_buf().into_box(),
                )
            });
            UnregisteredSymlinkedDirAction::validate_args(&mut xs)
        }

        // Check that error conditions are detected
        assert!(validate(&["test", "other"]).is_ok());
        assert!(validate(&["test", "test"]).is_err());
        assert!(validate(&["test", "other", "test"]).is_err());
        assert!(validate(&["test", "test/child"]).is_err());
    }
}
