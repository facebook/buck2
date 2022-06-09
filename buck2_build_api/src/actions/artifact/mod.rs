/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! An 'Artifact' represents a File to be used as part of the build. It can be either a file in the
//! source tree, or a file to be generated as part of the build.
//!
//! The existence of an Artifact does not mean the actual file exists. Artifacts needs to be
//! 'made available' before its existence on the filesystem is guaranteed.
//!
//! An 'Artifact' is first "declared" by rule implementation as a 'DeclaredArtifact'. The artifact
//! will need to be "bound" to an 'Action' through being used as an 'OutputArtifact'. Once bound,
//! it becomes a 'BuildArtifact' that can be available.
//!

use std::{
    cell::{Ref, RefCell},
    cmp::Ordering,
    hash::{Hash, Hasher},
    ops::Deref,
    rc::Rc,
};

use anyhow::anyhow;
use buck2_core::fs::paths::ForwardRelativePath;
use derive_more::Display;
use gazebo::{hash::Hashed, prelude::*};
use thiserror::Error;

use crate::{actions::ActionKey, deferred::BaseDeferredKey, path::BuckOutPath};

mod artifact_value;
pub use artifact_value::ArtifactValue;

mod build_artifact;
pub use build_artifact::BuildArtifact;

mod source_artifact;
pub use source_artifact::SourceArtifact;

mod fs;
pub use fs::ArtifactFs;

/// An 'Artifact' that can be materialized at its path.
#[derive(Clone, Debug, Display, Dupe, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Artifact(pub Hashed<ArtifactKind>);

impl Artifact {
    pub fn is_source(&self) -> bool {
        match self.0.as_ref() {
            ArtifactKind::Source(_) => true,
            ArtifactKind::Build(_) => false,
        }
    }

    /// The callsite that declared this artifact, any.
    pub fn owner(&self) -> Option<&BaseDeferredKey> {
        match self.0.as_ref() {
            ArtifactKind::Source(_) => None,
            ArtifactKind::Build(b) => Some(b.get_path().owner()),
        }
    }

    /// The action that would produce this artifact, if any.
    pub fn action_key(&self) -> Option<&ActionKey> {
        match self.0.as_ref() {
            ArtifactKind::Source(_) => None,
            ArtifactKind::Build(b) => Some(b.key()),
        }
    }

    pub fn path(&self) -> &ForwardRelativePath {
        match self.0.as_ref() {
            ArtifactKind::Source(s) => s.get_path().path().as_ref(),
            ArtifactKind::Build(b) => b.get_path().path(),
        }
    }

    pub fn short_path(&self) -> &ForwardRelativePath {
        match self.0.as_ref() {
            ArtifactKind::Source(s) => s.get_path().path().as_ref(),
            ArtifactKind::Build(b) => b.get_path().short_path(),
        }
    }
}

#[derive(Clone, Debug, Display, Dupe, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ArtifactKind {
    Source(SourceArtifact),
    Build(BuildArtifact),
}

impl From<SourceArtifact> for Artifact {
    fn from(a: SourceArtifact) -> Self {
        Artifact(Hashed::new(ArtifactKind::Source(a)))
    }
}

impl From<BuildArtifact> for Artifact {
    fn from(a: BuildArtifact) -> Self {
        Artifact(Hashed::new(ArtifactKind::Build(a)))
    }
}

/// An artifact that is "declared" by a rule implementation, which will be an artifact that can be
/// made available by running some action created by that rule implementation.
///
/// This type should only exist within the concept of a rule implementation.
///
/// The 'DeclaredArtifact' can be either already "bound" if there is already an action attached,
/// in which case it is underneath a 'BuildArtifact', or be unbound if there is no action attached
/// to it yet, in which case it is a 'UnboundArtifact' underneath.
///
/// All 'DeclaredArtifact's are forced to be bound at the end of the analysis phase.
#[derive(Clone, Debug, Display, Dupe)]
#[display(fmt = "{}", "_0.borrow()")]
pub struct DeclaredArtifact(Rc<RefCell<DeclaredArtifactImpl>>);

impl DeclaredArtifact {
    pub(super) fn new(path: BuckOutPath) -> DeclaredArtifact {
        DeclaredArtifact(Rc::new(RefCell::new(DeclaredArtifactImpl::Unbound(
            UnboundArtifact(path),
        ))))
    }

    pub fn as_output(&self) -> OutputArtifact {
        OutputArtifact(self.dupe())
    }

    pub fn get_path(&self) -> Ref<'_, BuckOutPath> {
        Ref::map(self.0.borrow(), |a| match a {
            DeclaredArtifactImpl::Bound(a) => a.get_path(),
            DeclaredArtifactImpl::Unbound(a) => &a.0,
        })
    }

    /// Ensure that the artifact is bound.
    ///
    /// This is called before we freeze the artifacts by the artifact registry.
    /// When we freeze `DeclaredArtifact`, we then just call `get_bound()`, and `expect`
    /// it to be valid. We have the `ensure_bound` method to make sure that we can return
    /// a friendlier message to users because `freeze()` does not return error messages
    pub(crate) fn ensure_bound(self) -> anyhow::Result<BuildArtifact> {
        match &*self.0.borrow() {
            DeclaredArtifactImpl::Bound(built) => Ok(built.dupe()),
            DeclaredArtifactImpl::Unbound(unbound) => {
                Err(anyhow!(ArtifactErrors::UnboundArtifact(unbound.clone())))
            }
        }
    }

    pub fn owner(&self) -> Option<BaseDeferredKey> {
        match &*self.0.borrow() {
            DeclaredArtifactImpl::Bound(b) => Some(b.get_path().owner().dupe()),
            DeclaredArtifactImpl::Unbound(_) => None,
        }
    }
}

impl Hash for DeclaredArtifact {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.get_path().hash(state)
    }
}

impl PartialEq for DeclaredArtifact {
    fn eq(&self, other: &Self) -> bool {
        *self.get_path() == *other.get_path()
    }
}

impl Eq for DeclaredArtifact {}

impl PartialOrd for DeclaredArtifact {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.get_path().partial_cmp(&*other.get_path())
    }
}

impl Ord for DeclaredArtifact {
    fn cmp(&self, other: &Self) -> Ordering {
        self.get_path().cmp(&*other.get_path())
    }
}

/// A 'DeclaredArtifact' can be either "bound" to an 'Action', or "unbound"
#[derive(Clone, Debug, Display)]
enum DeclaredArtifactImpl {
    Bound(BuildArtifact),
    Unbound(UnboundArtifact),
}

#[derive(Error, Debug)]
pub enum ArtifactErrors {
    #[error("artifact `{0}` was already bound, but attempted to bind to action id `{1}`")]
    DuplicateBind(BuildArtifact, ActionKey),
    #[error(
        "artifact `{0}` should be bound by now. If you are intending to use this artifact as the output of `run`, are you missing an `.as_output()` call?"
    )]
    UnboundArtifact(UnboundArtifact),
}

/// An artifact that is marked as the output of a particular 'Action'.
#[derive(Clone, Debug, Display, Dupe, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct OutputArtifact(DeclaredArtifact);

impl From<BuildArtifact> for OutputArtifact {
    fn from(artifact: BuildArtifact) -> Self {
        Self(DeclaredArtifact(Rc::new(RefCell::new(
            DeclaredArtifactImpl::Bound(artifact),
        ))))
    }
}

impl OutputArtifact {
    pub(crate) fn bind(&self, key: ActionKey) -> anyhow::Result<BuildArtifact> {
        match &mut *self.0.0.borrow_mut() {
            DeclaredArtifactImpl::Bound(a) => {
                return Err(anyhow!(ArtifactErrors::DuplicateBind(a.dupe(), key)));
            }
            a => take_mut::take(a, |artifact| match artifact {
                DeclaredArtifactImpl::Unbound(unbound) => {
                    DeclaredArtifactImpl::Bound(unbound.bind(key))
                }
                DeclaredArtifactImpl::Bound(_) => {
                    unreachable!("should already be verified to be unbound")
                }
            }),
        };

        Ok(match &*self.0.0.borrow() {
            DeclaredArtifactImpl::Bound(b) => b.dupe(),
            _ => unreachable!("should already be bound"),
        })
    }
}

impl Deref for OutputArtifact {
    type Target = DeclaredArtifact;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Clone, Debug, Display)]
pub struct UnboundArtifact(BuckOutPath);

impl UnboundArtifact {
    fn bind(self, key: ActionKey) -> BuildArtifact {
        BuildArtifact::new(self.0, key)
    }
}

pub mod testing {
    use buck2_core::{fs::paths::ForwardRelativePathBuf, target::ConfiguredTargetLabel};
    use gazebo::prelude::*;

    use crate::{
        actions::{
            artifact::{BuildArtifact, DeclaredArtifact, DeclaredArtifactImpl},
            ActionKey,
        },
        deferred::{
            testing::DeferredDataExt, BaseDeferredKey, DeferredData, DeferredId, DeferredKey,
        },
        path::BuckOutPath,
    };

    pub trait ArtifactTestingExt {
        fn testing_is_bound(&self) -> bool;

        fn testing_action_key(&self) -> Option<ActionKey>;
    }

    impl ArtifactTestingExt for DeclaredArtifact {
        fn testing_is_bound(&self) -> bool {
            match &*self.0.borrow() {
                DeclaredArtifactImpl::Bound(_) => true,
                DeclaredArtifactImpl::Unbound(_) => false,
            }
        }

        fn testing_action_key(&self) -> Option<ActionKey> {
            match &*self.0.borrow() {
                DeclaredArtifactImpl::Bound(built) => Some(built.0.key.dupe()),
                DeclaredArtifactImpl::Unbound(_) => None,
            }
        }
    }

    impl ArtifactTestingExt for BuildArtifact {
        fn testing_is_bound(&self) -> bool {
            true
        }

        fn testing_action_key(&self) -> Option<ActionKey> {
            Some(self.0.key.dupe())
        }
    }

    pub trait BuildArtifactTestingExt {
        fn testing_new(
            target: ConfiguredTargetLabel,
            path: ForwardRelativePathBuf,
            id: DeferredId,
        ) -> BuildArtifact;
    }

    impl BuildArtifactTestingExt for BuildArtifact {
        fn testing_new(
            target: ConfiguredTargetLabel,
            path: ForwardRelativePathBuf,
            id: DeferredId,
        ) -> BuildArtifact {
            BuildArtifact::new(
                BuckOutPath::new(BaseDeferredKey::TargetLabel(target.dupe()), path),
                DeferredData::testing_new(DeferredKey::Base(
                    BaseDeferredKey::TargetLabel(target),
                    id,
                )),
            )
        }
    }
}

#[cfg(test)]
mod tests {
    use std::convert::TryFrom;

    use buck2_core::{
        cells::{testing::CellResolverExt, CellName, CellResolver},
        configuration::Configuration,
        fs::{
            paths::{AbsPathBuf, ForwardRelativePathBuf},
            project::{ProjectFilesystem, ProjectRelativePath, ProjectRelativePathBuf},
        },
        package::{testing::PackageExt, Package, PackageRelativePathBuf},
        target::{testing::ConfiguredTargetLabelExt, ConfiguredTargetLabel, TargetName},
    };
    use gazebo::prelude::*;

    use crate::{
        actions::{
            artifact::{
                testing::BuildArtifactTestingExt, ArtifactFs, BuildArtifact, DeclaredArtifact,
                DeclaredArtifactImpl, SourceArtifact,
            },
            ActionKey,
        },
        deferred::{
            testing::{DeferredDataExt, DeferredIdExt},
            BaseDeferredKey, DeferredId, DeferredKey,
        },
        path::{BuckOutPath, BuckOutPathResolver, BuckPath, BuckPathResolver},
    };

    #[test]
    pub fn artifact_binding() -> anyhow::Result<()> {
        let target = ConfiguredTargetLabel::testing_new(
            Package::testing_new("cell", "pkg"),
            TargetName::unchecked_new("foo"),
            Configuration::testing_new(),
        );
        let declared = DeclaredArtifact::new(BuckOutPath::new(
            BaseDeferredKey::TargetLabel(target.dupe()),
            ForwardRelativePathBuf::unchecked_new("bar.out".into()),
        ));
        let key = ActionKey::testing_new(DeferredKey::Base(
            BaseDeferredKey::TargetLabel(target),
            DeferredId::testing_new(0),
        ));

        let out = declared.as_output();
        let bound = out.bind(key.dupe())?;

        assert_eq!(bound.0.key, key);
        assert_eq!(bound.get_path(), &*declared.get_path());

        match &*declared.0.borrow() {
            DeclaredArtifactImpl::Bound(b) => {
                assert_eq!(b, &bound);
            }
            _ => panic!("should be bound"),
        };

        // binding again should fail
        if out.bind(key).is_ok() {
            panic!("should error due to binding multiple times")
        }

        Ok(())
    }

    #[test]
    fn resolve_artifact() -> anyhow::Result<()> {
        let source = SourceArtifact::new(BuckPath::new(
            Package::testing_new("cell", "pkg"),
            PackageRelativePathBuf::unchecked_new("src.cpp".into()),
        ));

        let project_fs =
            ProjectFilesystem::new(AbsPathBuf::try_from(std::env::current_dir().unwrap()).unwrap());
        let fs = ArtifactFs::new(
            BuckPathResolver::new(CellResolver::of_names_and_paths(&[(
                CellName::unchecked_new("cell".into()),
                ProjectRelativePathBuf::unchecked_new("cell_path".into()),
            )])),
            BuckOutPathResolver::new(ProjectRelativePathBuf::unchecked_new("buck_out".into())),
            project_fs,
        );

        assert_eq!(
            fs.resolve(&source.into())?,
            ProjectRelativePath::unchecked_new("cell_path/pkg/src.cpp")
        );

        Ok(())
    }

    #[test]
    fn writes_files() -> anyhow::Result<()> {
        let tempdir = tempfile::tempdir()?;
        let project_fs = ProjectFilesystem::new(AbsPathBuf::try_from(tempdir.into_path()).unwrap());

        let target = ConfiguredTargetLabel::testing_new(
            Package::testing_new("cell", "pkg"),
            TargetName::unchecked_new("foo"),
            Configuration::testing_new(),
        );

        let artifact1 = BuildArtifact::testing_new(
            target.dupe(),
            ForwardRelativePathBuf::unchecked_new("foo/bar.cpp".to_owned()),
            DeferredId::testing_new(0),
        );
        let artifact2 = BuildArtifact::testing_new(
            target.dupe(),
            ForwardRelativePathBuf::unchecked_new("foo/bar.h".to_owned()),
            DeferredId::testing_new(0),
        );
        let artifact3 = BuildArtifact::testing_new(
            target,
            ForwardRelativePathBuf::unchecked_new("foo/bar.cpp/invalid_file.txt".to_owned()),
            DeferredId::testing_new(1),
        );

        let fs = ArtifactFs::new(
            BuckPathResolver::new(CellResolver::of_names_and_paths(&[(
                CellName::unchecked_new("cell".into()),
                ProjectRelativePathBuf::unchecked_new("cell_path".into()),
            )])),
            BuckOutPathResolver::new(ProjectRelativePathBuf::unchecked_new("buck_out".into())),
            project_fs.clone(),
        );
        let expected_path1 = project_fs.resolve(&fs.resolve_build(&artifact1));
        let expected_path2 = project_fs.resolve(&fs.resolve_build(&artifact2));

        fs.write_file(&artifact1, "artifact1", false)?;

        assert_eq!("artifact1", std::fs::read_to_string(&expected_path1)?);

        fs.write_file(&artifact2, "artifact2", true)?;

        assert_eq!("artifact2", std::fs::read_to_string(&expected_path2)?);

        fs.write_file(&artifact3, "artifact3", false)
            .expect_err("should fail because bar.cpp is a file");

        #[cfg(unix)]
        {
            // Check executable bit on unix
            use std::os::unix::fs::PermissionsExt;
            // Unix permission bits
            let artifact1_executable =
                std::fs::metadata(expected_path1)?.permissions().mode() & 0o100 != 0;
            let artifact2_executable =
                std::fs::metadata(expected_path2)?.permissions().mode() & 0o100 != 0;

            assert_eq!(false, artifact1_executable);
            assert_eq!(true, artifact2_executable);
        }

        Ok(())
    }
}
