/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{collections::HashSet, hash::Hash, sync::Arc};

use anyhow::Context as _;
use buck2_core::directory::Directory;
use gazebo::prelude::*;
use smallvec::{smallvec, SmallVec};

use crate::actions::{
    artifact::{Artifact, ArtifactFs, ArtifactValue},
    directory::{insert_artifact, ActionDirectoryBuilder, ActionSharedDirectory, INTERNER},
};

/// The [`ArtifactValue`]s for an [`crate::artifact_groups::ArtifactGroup`].
#[derive(Clone, Dupe)]
pub struct ArtifactGroupValues(pub(super) Arc<ArtifactGroupValuesData>);

impl ArtifactGroupValues {
    /// Create a new instance of ArtifactGroupValues for a TransitiveSetProjection. This expects
    /// that all the children *will* have a Directory.
    pub fn new(
        values: SmallVec<[(Artifact, ArtifactValue); 1]>,
        children: Vec<Self>,
        artifact_fs: &ArtifactFs,
    ) -> anyhow::Result<Self> {
        let mut builder = ActionDirectoryBuilder::empty();

        for (artifact, value) in values.iter() {
            let path = artifact_fs.resolve(artifact).context("Invalid artifact")?;
            insert_artifact(&mut builder, path.as_ref(), value)?;
        }

        for child in children.iter() {
            // NOTE: Technically, we could fall back to iterating the artifacts in the
            // ArtifactGroupValues here, but we *do* rely on the fact that TransitiveSetProjections
            // produce intermediate directories, so if they don't, it is preferable to report it.
            let child_dir = child
                .0
                .directory
                .as_ref()
                .context("TransitiveSetProjection was missing directory!")?;

            builder
                .merge(child_dir.to_builder())
                .context("Merge failed")?;
        }

        let directory = builder.fingerprint().shared(&*INTERNER);

        Ok(Self(Arc::new(ArtifactGroupValuesData {
            values,
            children,
            directory: Some(directory),
        })))
    }

    pub fn from_artifact(artifact: Artifact, value: ArtifactValue) -> Self {
        Self(Arc::new(ArtifactGroupValuesData {
            values: smallvec![(artifact, value)],
            children: Vec::new(),
            directory: None,
        }))
    }

    pub fn add_to_directory(
        &self,
        builder: &mut ActionDirectoryBuilder,
        artifact_fs: &ArtifactFs,
    ) -> anyhow::Result<()> {
        if let Some(d) = self.0.directory.as_ref() {
            builder.merge(d.to_builder())?;
            return Ok(());
        }

        for (artifact, value) in self.iter() {
            let projrel_path = artifact_fs.resolve(artifact)?;
            insert_artifact(builder, projrel_path.as_ref(), value)?;
        }

        Ok(())
    }

    pub fn iter(&self) -> impl Iterator<Item = &(Artifact, ArtifactValue)> {
        TransitiveSetIterator::new(self)
    }

    pub fn shallow_equals(&self, other: &Self) -> bool {
        let this = &self.0;
        let other = &other.0;

        this.values == other.values
            && this
                .children
                .iter()
                .eq_by(&other.children, |x, y| Arc::ptr_eq(&x.0, &y.0))
    }
}

pub struct ArtifactGroupValuesData {
    pub(super) values: SmallVec<[(Artifact, ArtifactValue); 1]>,
    pub(super) children: Vec<ArtifactGroupValues>,
    /// If set, a precomputed directory represented the union of all values in this
    /// ArtifactGroupValuesData.
    pub(super) directory: Option<ActionSharedDirectory>,
}

/// An opaque identifier for the identity of a ArtifactGroupValue. There is no operation on this
/// that makes sense except for comparison.
#[derive(Hash, Eq, PartialEq)]
pub struct ArtifactValueIdentity(usize);

impl TransitiveSetContainer for ArtifactGroupValues {
    type Value = (Artifact, ArtifactValue);
    type Identity = ArtifactValueIdentity;

    fn values(&self) -> &[Self::Value] {
        &self.0.values
    }

    fn children(&self) -> &[Self] {
        &self.0.children
    }

    fn identity(&self) -> Self::Identity {
        ArtifactValueIdentity(Arc::as_ptr(&self.0) as usize)
    }
}

pub trait TransitiveSetContainer: Sized {
    type Value: Sized;
    type Identity: Hash + Eq + PartialEq;

    fn values(&self) -> &[Self::Value];

    fn children(&self) -> &[Self];

    fn identity(&self) -> Self::Identity;
}

pub struct TransitiveSetIterator<'a, C, V, I> {
    values: &'a [V],
    queue: Vec<&'a C>,
    seen: HashSet<I>,
}

impl<'a, C>
    TransitiveSetIterator<
        'a,
        C,
        <C as TransitiveSetContainer>::Value,
        <C as TransitiveSetContainer>::Identity,
    >
where
    C: TransitiveSetContainer,
{
    pub fn new(container: &'a C) -> Self {
        let mut ret = Self {
            values: container.values(),
            queue: Vec::new(),
            seen: HashSet::new(),
        };
        ret.enqueue_children(container.children());
        ret
    }

    pub fn enqueue_children(&mut self, transitive: &'a [C]) {
        for t in transitive.iter().rev() {
            if self.seen.insert(t.identity()) {
                self.queue.push(t);
            }
        }
    }
}

impl<'a, C> Iterator
    for TransitiveSetIterator<
        'a,
        C,
        <C as TransitiveSetContainer>::Value,
        <C as TransitiveSetContainer>::Identity,
    >
where
    C: TransitiveSetContainer,
{
    type Item = &'a <C as TransitiveSetContainer>::Value;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some((v, rest)) = self.values.split_first() {
                self.values = rest;
                return Some(v);
            }

            let next = self.queue.pop()?;
            self.values = next.values();
            self.enqueue_children(next.children());
        }
    }
}

#[cfg(test)]
mod tests {
    use buck2_core::{
        configuration::Configuration,
        fs::paths::ForwardRelativePathBuf,
        package::{testing::PackageExt, Package},
        target::{testing::ConfiguredTargetLabelExt, ConfiguredTargetLabel, TargetName},
    };

    use super::*;
    use crate::{
        actions::artifact::{testing::BuildArtifactTestingExt, ArtifactValue, BuildArtifact},
        deferred::{testing::DeferredIdExt, DeferredId},
    };

    fn artifact(name: &str) -> (Artifact, ArtifactValue) {
        let target = ConfiguredTargetLabel::testing_new(
            Package::testing_new("cell", "pkg"),
            TargetName::unchecked_new("foo"),
            Configuration::testing_new(),
        );

        let artifact = BuildArtifact::testing_new(
            target.dupe(),
            ForwardRelativePathBuf::unchecked_new(name.to_owned()),
            DeferredId::testing_new(0),
        );

        let value = ArtifactValue::empty_file();

        (Artifact::from(artifact), value)
    }

    impl ArtifactGroupValuesData {
        fn value(mut self, v: &(Artifact, ArtifactValue)) -> Self {
            self.values.push((v.0.dupe(), v.1.dupe()));
            self
        }

        fn chain(mut self, child: &ArtifactGroupValues) -> Self {
            self.children.push(child.dupe());
            self
        }

        fn build(self) -> ArtifactGroupValues {
            ArtifactGroupValues(Arc::new(self))
        }
    }

    fn builder() -> ArtifactGroupValuesData {
        ArtifactGroupValuesData {
            values: Default::default(),
            children: Default::default(),
            directory: None,
        }
    }

    #[test]
    fn test_iter() {
        let a1 = artifact("a1");
        let a2 = artifact("a1");
        let a3 = artifact("a1");

        let v2 = builder().value(&a2).build();
        let v3 = builder().value(&a3).build();
        let values = builder().value(&a1).chain(&v2).chain(&v3).build();

        let mut iter = values.iter();
        assert_eq!(iter.next(), Some(&a1));
        assert_eq!(iter.next(), Some(&a2));
        assert_eq!(iter.next(), Some(&a3));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn test_shallow_eq() {
        let a1 = artifact("a1");
        let a2 = artifact("a1");
        let a3 = artifact("a1");

        let v2 = builder().value(&a2).build();
        let v3 = builder().value(&a3).build();

        {
            let s1 = builder().value(&a1).chain(&v2).chain(&v3).build();
            let s2 = builder().value(&a1).chain(&v2).chain(&v3).build();
            assert!(s1.shallow_equals(&s2));
        }

        {
            // Different artifacts
            let s1 = builder().value(&a1).chain(&v2).chain(&v3).build();
            let s2 = builder().chain(&v2).chain(&v3).build();
            assert!(!s1.shallow_equals(&s2));
        }

        {
            // Different children
            let s1 = builder().value(&a1).chain(&v2).chain(&v3).build();
            let s2 = builder().value(&a1).chain(&v2).build();
            assert!(!s1.shallow_equals(&s2));
        }
    }
}
