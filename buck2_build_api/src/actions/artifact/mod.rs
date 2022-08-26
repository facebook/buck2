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

use std::cell::Ref;
use std::cell::RefCell;
use std::cmp::Ordering;
use std::hash::Hash;
use std::hash::Hasher;
use std::ops::Deref;
use std::rc::Rc;
use std::sync::Arc;

use buck2_core::fs::paths::ForwardRelativePath;
use buck2_core::fs::paths::ForwardRelativePathBuf;
use derive_more::Display;
use derive_more::From;
use either::Either;
use gazebo::cell::ARef;
use gazebo::hash::Hashed;
use gazebo::prelude::*;
use thiserror::Error;

use crate::actions::ActionKey;
use crate::deferred::types::BaseDeferredKey;
use crate::path::BuckOutPath;

mod artifact_value;
pub use artifact_value::ArtifactValue;

mod build_artifact;
pub use build_artifact::BuildArtifact;

mod source_artifact;
pub use source_artifact::SourceArtifact;

mod projected_artifact;
pub use projected_artifact::ProjectedArtifact;

use crate::actions::artifact::path::ArtifactPath;

pub mod fs;
pub(crate) mod path;

/// An 'Artifact' that can be materialized at its path.
#[derive(Clone, Debug, Display, Dupe, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Artifact(pub Hashed<ArtifactKind>);

impl Artifact {
    pub fn new(
        artifact: impl Into<BaseArtifactKind>,
        projected_path: Option<Arc<ForwardRelativePathBuf>>,
    ) -> Self {
        let artifact = match projected_path {
            Some(path) => ArtifactKind::Projected(ProjectedArtifact::new(artifact.into(), path)),
            None => ArtifactKind::Base(artifact.into()),
        };

        Self(Hashed::new(artifact))
    }

    pub fn project(self, path: &ForwardRelativePath) -> Self {
        if path.is_empty() {
            return self;
        }

        match self.0.into() {
            ArtifactKind::Base(a) => Self::new(a, Some(Arc::new(path.to_owned()))),
            ArtifactKind::Projected(a) => {
                Self::new(a.base().dupe(), Some(Arc::new(a.path().join(path))))
            }
        }
    }

    pub fn is_source(&self) -> bool {
        match self.as_parts().0 {
            BaseArtifactKind::Source(_) => true,
            BaseArtifactKind::Build(_) => false,
        }
    }

    pub fn get_source(&self) -> Option<SourceArtifact> {
        match self.as_parts().0 {
            BaseArtifactKind::Source(x) => Some(x.dupe()),
            BaseArtifactKind::Build(_) => None,
        }
    }

    /// The callsite that declared this artifact, any.
    pub fn owner(&self) -> Option<&BaseDeferredKey> {
        match self.as_parts().0 {
            BaseArtifactKind::Source(_) => None,
            BaseArtifactKind::Build(b) => Some(b.get_path().owner()),
        }
    }

    /// The action that would produce this artifact, if any.
    pub fn action_key(&self) -> Option<&ActionKey> {
        match self.as_parts().0 {
            BaseArtifactKind::Source(_) => None,
            BaseArtifactKind::Build(b) => Some(b.key()),
        }
    }

    pub fn as_parts(&self) -> (&BaseArtifactKind, Option<&ForwardRelativePath>) {
        match self.0.as_ref() {
            ArtifactKind::Base(a) => (a, None),
            ArtifactKind::Projected(a) => (a.base(), Some(a.path())),
        }
    }

    pub fn get_path(&self) -> ArtifactPath<'_> {
        let (base, projected_path) = self.as_parts();

        let base_path = match base {
            BaseArtifactKind::Build(b) => Either::Left(ARef::new_ptr(b.get_path())),
            BaseArtifactKind::Source(s) => Either::Right(s.get_path()),
        };

        ArtifactPath {
            base_path,
            projected_path,
        }
    }
}

#[derive(
    Clone, Debug, Display, Dupe, PartialEq, Eq, PartialOrd, Ord, Hash, From
)]
pub enum BaseArtifactKind {
    Source(SourceArtifact),
    Build(BuildArtifact),
}

#[derive(Clone, Debug, Display, Dupe, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ArtifactKind {
    Base(BaseArtifactKind),
    Projected(ProjectedArtifact),
}

impl From<SourceArtifact> for Artifact {
    fn from(a: SourceArtifact) -> Self {
        Artifact(Hashed::new(ArtifactKind::Base(BaseArtifactKind::Source(a))))
    }
}

impl From<BuildArtifact> for Artifact {
    fn from(a: BuildArtifact) -> Self {
        Artifact(Hashed::new(ArtifactKind::Base(BaseArtifactKind::Build(a))))
    }
}

/// An intermediate struct to respond to calls to `ensure_bound`.
#[derive(Clone, Dupe, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Display)]
#[display(fmt = "{}", "self.get_path()")]
pub struct BoundBuildArtifact {
    artifact: BuildArtifact,
    projected_path: Option<Arc<ForwardRelativePathBuf>>,
}

impl BoundBuildArtifact {
    pub fn into_artifact(self) -> Artifact {
        Artifact::new(self.artifact, self.projected_path)
    }

    pub fn into_declared_artifact(self) -> DeclaredArtifact {
        DeclaredArtifact {
            artifact: Rc::new(RefCell::new(DeclaredArtifactKind::Bound(self.artifact))),
            projected_path: self.projected_path,
        }
    }

    pub fn as_base_artifact(&self) -> &BuildArtifact {
        &self.artifact
    }

    pub fn action_key(&self) -> &ActionKey {
        self.artifact.key()
    }

    pub fn get_path(&self) -> ArtifactPath<'_> {
        ArtifactPath {
            base_path: Either::Left(ARef::new_ptr(self.artifact.get_path())),
            projected_path: self
                .projected_path
                .as_ref()
                .map(|p| AsRef::<ForwardRelativePath>::as_ref(&**p)),
        }
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
#[derive(Clone, Debug, Dupe, Display)]
#[display(fmt = "{}", "self.get_path()")]
pub struct DeclaredArtifact {
    artifact: Rc<RefCell<DeclaredArtifactKind>>,
    projected_path: Option<Arc<ForwardRelativePathBuf>>,
}

impl DeclaredArtifact {
    pub(super) fn new(path: BuckOutPath) -> DeclaredArtifact {
        DeclaredArtifact {
            artifact: Rc::new(RefCell::new(DeclaredArtifactKind::Unbound(
                UnboundArtifact(path),
            ))),
            projected_path: None,
        }
    }

    pub fn project(&self, path: &ForwardRelativePath) -> Self {
        if path.is_empty() {
            return self.dupe();
        }

        Self {
            artifact: self.artifact.dupe(),
            projected_path: Some(Arc::new(match self.projected_path.as_ref() {
                Some(existing_path) => existing_path.join(path),
                None => path.to_owned(),
            })),
        }
    }

    pub fn as_output(&self) -> OutputArtifact {
        OutputArtifact(self.dupe())
    }

    pub fn get_path(&self) -> ArtifactPath<'_> {
        let borrow = self.artifact.borrow();

        let projected_path = self
            .projected_path
            .as_ref()
            .map(|p| AsRef::<ForwardRelativePath>::as_ref(&**p));

        let base_path = Ref::map(borrow, |a| match &a {
            DeclaredArtifactKind::Bound(a) => a.get_path(),
            DeclaredArtifactKind::Unbound(a) => &a.0,
        });

        ArtifactPath {
            base_path: Either::Left(ARef::new_ref(base_path)),
            projected_path,
        }
    }

    /// Ensure that the artifact is bound.
    ///
    /// This is called before we freeze the artifacts by the artifact registry.
    /// When we freeze `DeclaredArtifact`, we then just call `get_bound_deprecated()`, and `expect`
    /// it to be valid. We have the `ensure_bound` method to make sure that we can return
    /// a friendlier message to users because `freeze()` does not return error messages
    pub(crate) fn ensure_bound(self) -> anyhow::Result<BoundBuildArtifact> {
        let borrow = self.artifact.borrow();

        let artifact = match &*borrow {
            DeclaredArtifactKind::Bound(built) => built.dupe(),
            DeclaredArtifactKind::Unbound(unbound) => {
                return Err(anyhow::anyhow!(ArtifactErrors::UnboundArtifact(
                    unbound.dupe()
                )));
            }
        };

        Ok(BoundBuildArtifact {
            artifact,
            projected_path: self.projected_path,
        })
    }

    pub fn owner(&self) -> Option<BaseDeferredKey> {
        match &*self.artifact.borrow() {
            DeclaredArtifactKind::Bound(b) => Some(b.get_path().owner().dupe()),
            DeclaredArtifactKind::Unbound(_) => None,
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
        self.get_path() == other.get_path()
    }
}

impl Eq for DeclaredArtifact {}

impl PartialOrd for DeclaredArtifact {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.get_path().partial_cmp(&other.get_path())
    }
}

impl Ord for DeclaredArtifact {
    fn cmp(&self, other: &Self) -> Ordering {
        self.get_path().cmp(&other.get_path())
    }
}

/// A 'DeclaredArtifact' can be either "bound" to an 'Action', or "unbound"
#[derive(Clone, Dupe, Debug, Display)]
enum DeclaredArtifactKind {
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

impl From<DeclaredArtifact> for OutputArtifact {
    fn from(artifact: DeclaredArtifact) -> Self {
        Self(artifact)
    }
}

impl OutputArtifact {
    pub(crate) fn bind(&self, key: ActionKey) -> anyhow::Result<BoundBuildArtifact> {
        match &mut *self.0.artifact.borrow_mut() {
            DeclaredArtifactKind::Bound(a) => {
                // NOTE: If the artifact was already bound to the same action, we leave it alone.
                // This can happen when we have projected artifacts used in a command: we'll visit
                // the projected artifacts and then try to bind each of them, but the same
                // underlying artifact is the one that gets bound.
                if *a.key() != key {
                    return Err(anyhow::anyhow!(ArtifactErrors::DuplicateBind(
                        a.dupe(),
                        key
                    )));
                }
            }
            a => take_mut::take(a, |artifact| match artifact {
                DeclaredArtifactKind::Unbound(unbound) => {
                    DeclaredArtifactKind::Bound(unbound.bind(key))
                }
                DeclaredArtifactKind::Bound(_) => {
                    unreachable!("should already be verified to be unbound")
                }
            }),
        };

        let artifact = match &*self.0.artifact.borrow() {
            DeclaredArtifactKind::Bound(b) => b.dupe(),
            _ => unreachable!("should already be bound"),
        };

        Ok(BoundBuildArtifact {
            artifact,
            projected_path: self.0.projected_path.dupe(),
        })
    }
}

impl Deref for OutputArtifact {
    type Target = DeclaredArtifact;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Clone, Dupe, Debug, Display)]
pub struct UnboundArtifact(BuckOutPath);

impl UnboundArtifact {
    fn bind(self, key: ActionKey) -> BuildArtifact {
        BuildArtifact::new(self.0, key)
    }
}

pub mod testing {
    use buck2_core::fs::paths::ForwardRelativePathBuf;
    use buck2_core::target::ConfiguredTargetLabel;
    use gazebo::prelude::*;

    use crate::actions::artifact::BuildArtifact;
    use crate::actions::artifact::DeclaredArtifact;
    use crate::actions::artifact::DeclaredArtifactKind;
    use crate::actions::ActionKey;
    use crate::deferred::types::testing::DeferredDataExt;
    use crate::deferred::types::BaseDeferredKey;
    use crate::deferred::types::DeferredData;
    use crate::deferred::types::DeferredId;
    use crate::deferred::types::DeferredKey;
    use crate::path::BuckOutPath;

    pub trait ArtifactTestingExt {
        fn testing_is_bound(&self) -> bool;

        fn testing_action_key(&self) -> Option<ActionKey>;
    }

    impl ArtifactTestingExt for DeclaredArtifact {
        fn testing_is_bound(&self) -> bool {
            match &*self.artifact.borrow() {
                DeclaredArtifactKind::Bound(_) => true,
                DeclaredArtifactKind::Unbound(_) => false,
            }
        }

        fn testing_action_key(&self) -> Option<ActionKey> {
            match &*self.artifact.borrow() {
                DeclaredArtifactKind::Bound(built) => Some(built.0.key.dupe()),
                DeclaredArtifactKind::Unbound(_) => None,
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
    use assert_matches::assert_matches;
    use buck2_core::buck_path::BuckPath;
    use buck2_core::cells::cell_root_path::CellRootPathBuf;
    use buck2_core::cells::testing::CellResolverExt;
    use buck2_core::cells::CellName;
    use buck2_core::cells::CellResolver;
    use buck2_core::configuration::Configuration;
    use buck2_core::fs::paths::AbsPathBuf;
    use buck2_core::fs::paths::ForwardRelativePathBuf;
    use buck2_core::fs::project::ProjectRelativePath;
    use buck2_core::fs::project::ProjectRelativePathBuf;
    use buck2_core::fs::project::ProjectRoot;
    use buck2_core::package::package_relative_path::PackageRelativePathBuf;
    use buck2_core::package::testing::PackageExt;
    use buck2_core::package::Package;
    use buck2_core::target::testing::ConfiguredTargetLabelExt;
    use buck2_core::target::ConfiguredTargetLabel;
    use buck2_core::target::TargetName;
    use gazebo::prelude::*;

    use crate::actions::artifact::fs::ArtifactFs;
    use crate::actions::artifact::testing::BuildArtifactTestingExt;
    use crate::actions::artifact::Artifact;
    use crate::actions::artifact::BuildArtifact;
    use crate::actions::artifact::DeclaredArtifact;
    use crate::actions::artifact::DeclaredArtifactKind;
    use crate::actions::artifact::SourceArtifact;
    use crate::actions::ActionKey;
    use crate::deferred::types::testing::DeferredDataExt;
    use crate::deferred::types::testing::DeferredIdExt;
    use crate::deferred::types::BaseDeferredKey;
    use crate::deferred::types::DeferredId;
    use crate::deferred::types::DeferredKey;
    use crate::path::BuckOutPath;
    use crate::path::BuckOutPathResolver;
    use crate::path::BuckPathResolver;

    #[test]
    fn artifact_binding() -> anyhow::Result<()> {
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
            BaseDeferredKey::TargetLabel(target.dupe()),
            DeferredId::testing_new(0),
        ));

        let out = declared.as_output();
        let bound = out.bind(key.dupe())?;

        assert_eq!(*bound.as_base_artifact().key(), key);
        assert_eq!(bound.get_path(), declared.get_path());

        match &*declared.artifact.borrow() {
            DeclaredArtifactKind::Bound(b) => {
                assert_eq!(b, bound.as_base_artifact());
            }
            _ => panic!("should be bound"),
        };

        // Binding again to the same key should succeed
        out.bind(key)?;

        // Binding again to a different key should fail
        let other_key = ActionKey::testing_new(DeferredKey::Base(
            BaseDeferredKey::TargetLabel(target),
            DeferredId::testing_new(1),
        ));

        assert_matches!(out.bind(other_key), Err(..));

        Ok(())
    }

    #[test]
    fn resolve_artifact() -> anyhow::Result<()> {
        let source = SourceArtifact::new(BuckPath::new(
            Package::testing_new("cell", "pkg"),
            PackageRelativePathBuf::unchecked_new("src.cpp".into()),
        ));

        let project_fs =
            ProjectRoot::new(AbsPathBuf::try_from(std::env::current_dir().unwrap()).unwrap());
        let fs = ArtifactFs::new(
            BuckPathResolver::new(CellResolver::of_names_and_paths(&[(
                CellName::unchecked_new("cell".into()),
                CellRootPathBuf::new(ProjectRelativePathBuf::unchecked_new("cell_path".into())),
            )])),
            BuckOutPathResolver::new(ProjectRelativePathBuf::unchecked_new("buck_out".into())),
            project_fs,
        );

        assert_eq!(
            fs.resolve(Artifact::from(source).get_path())?,
            ProjectRelativePath::unchecked_new("cell_path/pkg/src.cpp")
        );

        Ok(())
    }

    #[test]
    fn writes_files() -> anyhow::Result<()> {
        let tempdir = tempfile::tempdir()?;
        let project_fs = ProjectRoot::new(AbsPathBuf::try_from(tempdir.into_path()).unwrap());

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
                CellRootPathBuf::new(ProjectRelativePathBuf::unchecked_new("cell_path".into())),
            )])),
            BuckOutPathResolver::new(ProjectRelativePathBuf::unchecked_new("buck_out".into())),
            project_fs.dupe(),
        );
        let expected_path1 = project_fs.resolve(&fs.resolve_build(artifact1.get_path()));
        let expected_path2 = project_fs.resolve(&fs.resolve_build(artifact2.get_path()));

        fs.write_file(artifact1.get_path(), "artifact1", false)?;

        assert_eq!("artifact1", std::fs::read_to_string(&expected_path1)?);

        fs.write_file(artifact2.get_path(), "artifact2", true)?;

        assert_eq!("artifact2", std::fs::read_to_string(&expected_path2)?);

        fs.write_file(artifact3.get_path(), "artifact3", false)
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
