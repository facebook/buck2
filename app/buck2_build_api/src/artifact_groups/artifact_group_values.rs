/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::collections::HashSet;
use std::hash::Hash;
use std::sync::Arc;

use allocative::Allocative;
use buck2_artifact::artifact::artifact_type::Artifact;
use buck2_core::content_hash::ContentBasedPathHash;
use buck2_core::fs::artifact_path_resolver::ArtifactFs;
use buck2_directory::directory::directory::Directory;
use buck2_error::BuckErrorContext;
use buck2_error::internal_error;
use buck2_execute::artifact::artifact_dyn::ArtifactDyn;
use buck2_execute::artifact::group::artifact_group_values_dyn::ArtifactGroupValuesDyn;
use buck2_execute::artifact_value::ArtifactValue;
use buck2_execute::digest_config::DigestConfig;
use buck2_execute::directory::ActionDirectoryBuilder;
use buck2_execute::directory::ActionSharedDirectory;
use buck2_execute::directory::INTERNER;
use buck2_execute::directory::insert_artifact;
use dupe::Dupe;
use smallvec::SmallVec;
use smallvec::smallvec;

/// The [`ArtifactValue`]s for an [`crate::artifact_groups::ArtifactGroup`].
#[derive(Clone, Dupe, Allocative)]
pub struct ArtifactGroupValues(pub(super) Arc<ArtifactGroupValuesData>);

impl ArtifactGroupValues {
    /// Create a new instance of ArtifactGroupValues for a TransitiveSetProjection. This expects
    /// that all the children *will* have a Directory.
    pub fn new(
        values: SmallVec<[(Artifact, ArtifactValue); 1]>,
        children: Vec<Self>,
        artifact_fs: &ArtifactFs,
        digest_config: DigestConfig,
    ) -> buck2_error::Result<Self> {
        let mut content_based_builder = ActionDirectoryBuilder::empty();
        let mut non_content_based_builder = ActionDirectoryBuilder::empty();
        let mut dep_files_builder = ActionDirectoryBuilder::empty();

        for (artifact, value) in values.iter() {
            if artifact.has_content_based_path() {
                let path = artifact
                    .resolve_path(artifact_fs, Some(&value.content_based_path_hash()))
                    .buck_error_context("Invalid artifact")?;
                insert_artifact(&mut content_based_builder, path.as_ref(), value)?;

                let dep_files_path = artifact
                    .resolve_path(
                        artifact_fs,
                        Some(&ContentBasedPathHash::DepFilesPlaceholder),
                    )
                    .buck_error_context("Invalid artifact")?;
                insert_artifact(&mut dep_files_builder, dep_files_path.as_ref(), value)?;
            } else {
                let path = artifact
                    .resolve_path(artifact_fs, None)
                    .buck_error_context("Invalid artifact")?;
                insert_artifact(&mut non_content_based_builder, path.as_ref(), value)?;
            }
        }

        for child in children.iter() {
            // NOTE: Technically, we could fall back to iterating the artifacts in the
            // ArtifactGroupValues here, but we *do* rely on the fact that TransitiveSetProjections
            // produce intermediate directories, so if they don't, it is preferable to report it.
            let non_content_based_child_dir = child
                .0
                .non_content_based_directory
                .as_ref()
                .buck_error_context(
                    "TransitiveSetProjection was missing non_content_based_directory!",
                )?;

            non_content_based_builder
                .merge_with_compatible_leaves(non_content_based_child_dir.to_builder())
                .buck_error_context("Merge failed")?;

            let content_based_child_dir = child
                .0
                .content_based_directory
                .as_ref()
                .buck_error_context(
                    "TransitiveSetProjection was missing content_based_directory!",
                )?;

            content_based_builder
                .merge_with_compatible_leaves(content_based_child_dir.to_builder())
                .buck_error_context("Merge failed")?;

            let dep_files_child_dir =
                child.0.dep_files_directory.as_ref().buck_error_context(
                    "TransitiveSetProjection was missing dep_files_directory!",
                )?;

            dep_files_builder
                .merge_with_compatible_leaves(dep_files_child_dir.to_builder())
                .buck_error_context("Merge failed")?;
        }

        let non_content_based_directory = non_content_based_builder
            .fingerprint(digest_config.as_directory_serializer())
            .shared(&*INTERNER);

        let content_based_directory = content_based_builder
            .fingerprint(digest_config.as_directory_serializer())
            .shared(&*INTERNER);

        let dep_files_directory = dep_files_builder
            .fingerprint(digest_config.as_directory_serializer())
            .shared(&*INTERNER);

        Ok(Self(Arc::new(ArtifactGroupValuesData {
            values,
            children,
            non_content_based_directory: Some(non_content_based_directory),
            content_based_directory: Some(content_based_directory),
            dep_files_directory: Some(dep_files_directory),
        })))
    }

    pub fn from_artifact(artifact: Artifact, value: ArtifactValue) -> Self {
        Self(Arc::new(ArtifactGroupValuesData {
            values: smallvec![(artifact, value)],
            children: Vec::new(),
            non_content_based_directory: None,
            content_based_directory: None,
            dep_files_directory: None,
        }))
    }

    pub fn add_to_directory(
        &self,
        builder: &mut ActionDirectoryBuilder,
        artifact_fs: &ArtifactFs,
    ) -> buck2_error::Result<()> {
        match (
            self.0.non_content_based_directory.as_ref(),
            self.0.content_based_directory.as_ref(),
        ) {
            (Some(non_content_based_directory), Some(content_based_directory)) => {
                builder.merge(non_content_based_directory.to_builder())?;
                builder.merge(content_based_directory.to_builder())?;
                return Ok(());
            }
            (Some(_), None) | (None, Some(_)) => {
                return Err(internal_error!(
                    "Expected both or neither of content_based_directory and non_content_based_directory to be set"
                ));
            }
            (None, None) => {}
        }

        for (artifact, value) in self.iter() {
            let projrel_path = artifact.resolve_path(
                artifact_fs,
                if artifact.has_content_based_path() {
                    Some(value.content_based_path_hash())
                } else {
                    None
                }
                .as_ref(),
            )?;
            insert_artifact(builder, projrel_path.as_ref(), value)?;
        }

        Ok(())
    }

    pub fn add_to_directory_for_dep_files(
        &self,
        builder: &mut ActionDirectoryBuilder,
        artifact_fs: &ArtifactFs,
    ) -> buck2_error::Result<()> {
        match (
            self.0.non_content_based_directory.as_ref(),
            self.0.dep_files_directory.as_ref(),
        ) {
            (Some(d), Some(dep_files_dir)) => {
                builder.merge(d.to_builder())?;
                builder.merge(dep_files_dir.to_builder())?;
                return Ok(());
            }
            (None, None) => {}
            (Some(_), None) | (None, Some(_)) => {
                return Err(internal_error!(
                    "Expected both or neither of dep_files_directory and non_content_based_directory to be set"
                ));
            }
        }

        for (artifact, value) in self.iter() {
            let projrel_path = artifact.resolve_path(
                artifact_fs,
                if artifact.has_content_based_path() {
                    Some(ContentBasedPathHash::DepFilesPlaceholder)
                } else {
                    None
                }
                .as_ref(),
            )?;
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

#[derive(Allocative)]
pub struct ArtifactGroupValuesData {
    pub(super) values: SmallVec<[(Artifact, ArtifactValue); 1]>,
    pub(super) children: Vec<ArtifactGroupValues>,
    /// If set, a precomputed directory represented the union of all non-content-based values in
    /// this ArtifactGroupValuesData.
    pub(super) non_content_based_directory: Option<ActionSharedDirectory>,
    /// If set, a precomputed directory represented the union of all content-based values in
    /// this ArtifactGroupValuesData.
    pub(super) content_based_directory: Option<ActionSharedDirectory>,
    pub(super) dep_files_directory: Option<ActionSharedDirectory>,
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

impl ArtifactGroupValuesDyn for ArtifactGroupValues {
    fn iter(&self) -> Box<dyn Iterator<Item = (&dyn ArtifactDyn, &ArtifactValue)> + '_> {
        Box::new(
            self.iter()
                .map(|(artifact, value)| (artifact as &dyn ArtifactDyn, value)),
        )
    }

    fn add_to_directory(
        &self,
        builder: &mut ActionDirectoryBuilder,
        artifact_fs: &ArtifactFs,
    ) -> buck2_error::Result<()> {
        self.add_to_directory(builder, artifact_fs)
    }
}

#[cfg(test)]
mod tests {
    use buck2_artifact::actions::key::ActionIndex;
    use buck2_artifact::artifact::artifact_type::testing::BuildArtifactTestingExt;
    use buck2_artifact::artifact::build_artifact::BuildArtifact;
    use buck2_core::configuration::data::ConfigurationData;
    use buck2_core::target::configured_target_label::ConfiguredTargetLabel;

    use super::*;

    fn artifact(name: &str) -> (Artifact, ArtifactValue) {
        let target =
            ConfiguredTargetLabel::testing_parse("cell//pkg:foo", ConfigurationData::testing_new());

        let artifact = BuildArtifact::testing_new(target.dupe(), name, ActionIndex::new(0));

        let value = ArtifactValue::file(DigestConfig::testing_default().empty_file());

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
            non_content_based_directory: None,
            content_based_directory: None,
            dep_files_directory: None,
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
