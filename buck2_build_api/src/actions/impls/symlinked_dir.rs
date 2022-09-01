/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Cow;
use std::path::PathBuf;

use anyhow::Context as _;
use async_trait::async_trait;
use buck2_core::category::Category;
use buck2_core::fs::paths::ForwardRelativePath;
use buck2_execute::artifact_utils::ArtifactValueBuilder;
use buck2_execute::execute::command_executor::ActionExecutionTimingData;
use buck2_execute::materialize::materializer::CopiedArtifact;
use gazebo::prelude::*;
use indexmap::IndexSet;
use once_cell::sync::Lazy;
use starlark::values::dict::Dict;
use starlark::values::OwnedFrozenValue;
use starlark::values::Value;
use starlark::values::ValueError;
use thiserror::Error;

use crate::actions::artifact::build_artifact::BuildArtifact;
use crate::actions::Action;
use crate::actions::ActionExecutable;
use crate::actions::ActionExecutionCtx;
use crate::actions::PristineActionExecutable;
use crate::actions::UnregisteredAction;
use crate::artifact_groups::ArtifactGroup;
use crate::execute::ActionExecutionKind;
use crate::execute::ActionExecutionMetadata;
use crate::execute::ActionOutputs;
use crate::interpreter::rule_defs::artifact::ValueAsArtifactLike;

#[derive(Debug, Error)]
enum SymlinkedDirError {
    #[error("Paths to symlink_dir must be non-overlapping, but got {0:?} and {1:?}")]
    OverlappingPaths(PathBuf, PathBuf),
    #[error("Paths to symlink_dir must not be empty")]
    EmptyPath,
    #[error("Only artifact inputs are supported in symlink_dir actions, got {0}")]
    UnsupportedInput(ArtifactGroup),
}

pub(crate) struct UnregisteredSymlinkedDirAction {
    copy: bool,
    args: Vec<(ArtifactGroup, PathBuf)>,
}

impl UnregisteredSymlinkedDirAction {
    /// Validate that no output path is duplicated or overlapping.
    /// Duplicates are easy - overlapping only happens with symlinks to directories (a bad idea),
    /// and would look like `a` and `a/b` both being given.
    fn validate_args(args: &mut [(ArtifactGroup, PathBuf)]) -> anyhow::Result<()> {
        // We sort the inputs. They are morally a set, so it shouldn't matter too much,
        // and this lets us implement the overlap check more easily.
        args.sort_by(|x, y| x.1.cmp(&y.1));

        for ((_, x), (_, y)) in args.iter().zip(args.iter().skip(1)) {
            if y.starts_with(x) {
                return Err(SymlinkedDirError::OverlappingPaths(x.clone(), y.clone()).into());
            }
        }
        if args.len() == 1 && args[0].1.as_os_str().is_empty() {
            return Err(SymlinkedDirError::EmptyPath.into());
        }

        for (g, _) in args.iter() {
            // TODO: Exclude other variants once they become available here. For now, this is a
            // noop.
            match g {
                ArtifactGroup::Artifact(..) => {}
                other => return Err(SymlinkedDirError::UnsupportedInput(other.dupe()).into()),
            };
        }

        Ok(())
    }

    fn unpack_args(srcs: Value) -> Option<Vec<(ArtifactGroup, PathBuf)>> {
        Dict::from_value(srcs)?
            .iter()
            .map(|(k, v)| {
                Some((
                    ArtifactGroup::Artifact(v.as_artifact()?.get_bound_deprecated().ok()?),
                    PathBuf::from(k.unpack_str()?),
                ))
            })
            .collect::<Option<_>>()
    }

    pub fn new(copy: bool, srcs: Value) -> anyhow::Result<Self> {
        let mut args = Self::unpack_args(srcs)
            // FIXME: This warning is talking about the Starlark-level argument name `srcs`.
            //        Once we use a proper Value parser this should all get cleaned up.
            .ok_or_else(|| ValueError::IncorrectParameterTypeNamed("srcs".to_owned()))?;
        // Overlapping check make sense for non-copy mode only.
        // When directories are copied into the same destination, the ordering defines how files are overwritten.
        if !copy {
            Self::validate_args(&mut args)?;
        }
        Ok(Self { copy, args })
    }

    pub fn inputs(&self) -> IndexSet<ArtifactGroup> {
        self.args.iter().map(|x| x.0.dupe()).collect()
    }
}

impl UnregisteredAction for UnregisteredSymlinkedDirAction {
    fn register(
        self: Box<Self>,
        inputs: IndexSet<ArtifactGroup>,
        outputs: IndexSet<BuildArtifact>,
        _starlark_data: Option<OwnedFrozenValue>,
    ) -> anyhow::Result<Box<dyn Action>> {
        Ok(box SymlinkedDirAction {
            copy: self.copy,
            args: self.args,
            inputs,
            outputs,
        })
    }
}

#[derive(Debug)]
struct SymlinkedDirAction {
    copy: bool,
    args: Vec<(ArtifactGroup, PathBuf)>,
    inputs: IndexSet<ArtifactGroup>,
    outputs: IndexSet<BuildArtifact>,
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

    fn inputs(&self) -> anyhow::Result<Cow<'_, IndexSet<ArtifactGroup>>> {
        Ok(Cow::Borrowed(&self.inputs))
    }

    fn outputs(&self) -> anyhow::Result<Cow<'_, IndexSet<BuildArtifact>>> {
        Ok(Cow::Borrowed(&self.outputs))
    }

    fn as_executable(&self) -> ActionExecutable<'_> {
        ActionExecutable::Pristine(self)
    }

    fn category(&self) -> &Category {
        static SYMLINKED_DIR_CATEGORY: Lazy<Category> =
            Lazy::new(|| Category::try_from("symlinked_dir").unwrap());

        &SYMLINKED_DIR_CATEGORY
    }

    fn identifier(&self) -> Option<&str> {
        Some(self.output().get_path().short_path().as_str())
    }
}

#[async_trait]
impl PristineActionExecutable for SymlinkedDirAction {
    async fn execute(
        &self,
        ctx: &mut dyn ActionExecutionCtx,
    ) -> anyhow::Result<(ActionOutputs, ActionExecutionMetadata)> {
        let fs = ctx.fs().fs();
        let output = ctx.fs().resolve_build(self.output().get_path());
        let mut builder = ArtifactValueBuilder::new(fs);
        let mut srcs = Vec::new();

        for (group, dest) in &self.args {
            let (src_artifact, value) = ctx
                .artifact_values(group)
                .iter()
                .into_singleton()
                .context("Input did not dereference to exactly one artifact")?;

            let src = ctx.fs().resolve(src_artifact.get_path())?;
            let dest = output.join(ForwardRelativePath::new(dest)?);

            if self.copy {
                let dest_entry = builder.add_copied(value, src.as_ref(), dest.as_ref())?;
                srcs.push(CopiedArtifact::new(
                    src,
                    dest,
                    dest_entry.map_dir(|d| d.as_immutable()),
                ));
            } else {
                builder.add_symlinked(value, src.as_ref(), dest.as_ref())?;
            }
        }

        let value = builder.build(output.as_ref())?;
        ctx.materializer()
            .declare_copy(output, value.dupe(), srcs)
            .await?;
        Ok((
            ActionOutputs::from_single(self.output().get_path().dupe(), value),
            ActionExecutionMetadata {
                execution_kind: ActionExecutionKind::Simple,
                timing: ActionExecutionTimingData::default(),
            },
        ))
    }
}

#[cfg(test)]
mod tests {
    use buck2_core::buck_path::BuckPath;
    use buck2_core::package::package_relative_path::PackageRelativePathBuf;
    use buck2_core::package::testing::PackageExt;
    use buck2_core::package::Package;
    use buck2_execute::artifact::source_artifact::SourceArtifact;

    use super::*;
    use crate::actions::artifact::Artifact;
    use crate::actions::ArtifactGroup;

    fn mk_artifact() -> Artifact {
        let pkg = Package::testing_new("cell", "pkg");
        let path = PackageRelativePathBuf::unchecked_new("".to_owned());
        let buck_path = BuckPath::new(pkg, path);
        Artifact::from(SourceArtifact::new(buck_path))
    }

    // TODO: This needs proper tests, but right now it's kind of a pain to get the
    //       action framework up and running to test actions
    #[test]
    fn symlinked_dir_test() {}

    #[test]
    fn test_symlinked_dir_validation() {
        fn validate(paths: &[&str]) -> anyhow::Result<()> {
            let a = ArtifactGroup::Artifact(mk_artifact());
            let mut xs = paths.map(|x| (a.dupe(), PathBuf::from(x)));
            UnregisteredSymlinkedDirAction::validate_args(&mut xs)
        }

        // Check that error conditions are detected
        assert!(validate(&["test", "other"]).is_ok());
        assert!(validate(&["test", "test"]).is_err());
        assert!(validate(&["test", "other", "test"]).is_err());
        assert!(validate(&["test", "test/child"]).is_err());
    }
}
