/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::hash_map::DefaultHasher;
use std::hash::Hash;
use std::hash::Hasher;
use std::sync::Arc;

use allocative::Allocative;
use derive_more::Display;
use dupe::Dupe;
use itertools::Itertools;

use crate::category::CategoryRef;
use crate::cells::external::ExternalCellOrigin;
use crate::cells::paths::CellRelativePath;
use crate::deferred::base_deferred_key::BaseDeferredKey;
use crate::deferred::key::DeferredHolderKey;
use crate::fs::dynamic_actions_action_key::DynamicActionsActionKey;
use crate::fs::paths::forward_rel_path::ForwardRelativePath;
use crate::fs::paths::forward_rel_path::ForwardRelativePathBuf;
use crate::fs::project_rel_path::ProjectRelativePath;
use crate::fs::project_rel_path::ProjectRelativePathBuf;
use crate::provider::label::ConfiguredProvidersLabel;
use crate::provider::label::NonDefaultProvidersName;
use crate::provider::label::ProvidersName;

#[derive(
    Clone,
    Debug,
    Display,
    Allocative,
    Hash,
    Eq,
    PartialEq,
    strong_hash::StrongHash
)]
#[display("({})/{}", owner, path.as_str())]
struct BuildArtifactPathData {
    /// The owner responsible for creating this path.
    owner: DeferredHolderKey,
    /// The path relative to that target.
    path: Box<ForwardRelativePath>,
}

/// Represents a resolvable path corresponding to outputs of rules that are part
/// of a `Package`. The `BuckOutPath` refers to only the outputs of rules,
/// not the repo sources.
///
/// This structure contains a target label for generating the base of the path (base
/// path), and a `ForwardRelativePath` that represents the specific output
/// location relative to the 'base path'.
#[derive(
    Clone,
    Dupe,
    Debug,
    Display,
    Hash,
    PartialEq,
    Eq,
    Allocative,
    strong_hash::StrongHash
)]
pub struct BuildArtifactPath(Arc<BuildArtifactPathData>);

impl BuildArtifactPath {
    pub fn new(owner: BaseDeferredKey, path: ForwardRelativePathBuf) -> Self {
        Self::with_dynamic_actions_action_key(DeferredHolderKey::Base(owner), path)
    }

    pub fn with_dynamic_actions_action_key(
        owner: DeferredHolderKey,
        path: ForwardRelativePathBuf,
    ) -> Self {
        BuildArtifactPath(Arc::new(BuildArtifactPathData {
            owner,
            path: path.into_box(),
        }))
    }

    pub fn owner(&self) -> &DeferredHolderKey {
        &self.0.owner
    }

    pub fn dynamic_actions_action_key(&self) -> Option<DynamicActionsActionKey> {
        self.0.owner.action_key()
    }

    pub fn path(&self) -> &ForwardRelativePath {
        &self.0.path
    }
}

#[derive(Clone, Debug, Display, Eq, PartialEq)]
#[display("tmp/({})/{}", owner, path.as_str())]
pub struct BuckOutScratchPath {
    /// The deferred responsible for creating this path.
    owner: BaseDeferredKey,
    /// The path relative to that target.
    path: ForwardRelativePathBuf,
    /// The unique identifier for this action
    action_key: ForwardRelativePathBuf,
}

impl BuckOutScratchPath {
    /// Returning an Err from this function is considered an internal error - we try
    /// really hard to normalise anything the user supplies.
    pub fn new(
        owner: BaseDeferredKey,
        category: CategoryRef,
        identifier: Option<&str>,
        action_key: ForwardRelativePathBuf,
    ) -> buck2_error::Result<Self> {
        const MAKE_SENSIBLE_PREFIX: &str = "_buck_";
        // Windows has MAX_PATH limit (260 chars).
        const LENGTH_THRESHOLD: usize = 50;

        /// A path is sensible if it's going to produce a ForwardRelativePath that works and isn't too long.
        /// These are heuristics to make it more likely that paths that are not sensible are replaced, not guarantees.
        fn is_sensible(x: &str) -> Option<&ForwardRelativePath> {
            // We need a way to make something sensible, so disallow this prefix
            if x.starts_with(MAKE_SENSIBLE_PREFIX) {
                return None;
            }
            // If things are too long they don't make good paths
            if x.len() > LENGTH_THRESHOLD {
                return None;
            }

            // If things have weird characters in they won't make a good path.
            // Mostly based off Windows banned characters, with some additions.
            // We do allow `/` and `\` as they can just be part of the path.
            // We exclude spaces because they cause issues with some command line tools.
            const BAD_CHARS: &str = "<>;:\"'\n\r\t?* ";
            for c in BAD_CHARS.as_bytes() {
                if x.as_bytes().contains(c) {
                    return None;
                }
            }
            ForwardRelativePath::new(x).ok()
        }

        let path = ForwardRelativePath::new(category.as_str())?;
        let path = match identifier {
            Some(v) => {
                if let Some(v) = is_sensible(v) {
                    path.join_normalized(v)?
                } else {
                    // FIXME: Should this be a crypto hasher?
                    let mut hasher = DefaultHasher::new();
                    v.hash(&mut hasher);
                    let output_hash = format!("{}{:016x}", MAKE_SENSIBLE_PREFIX, hasher.finish());
                    path.join_normalized(ForwardRelativePath::new(&output_hash)?)?
                }
            }
            _ => path.to_buf(),
        };

        Ok(Self {
            owner,
            path,
            action_key,
        })
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Allocative)]
pub struct BuckOutTestPath {
    /// A base path. This is primarily useful when e.g. set of tests should all be in the same
    /// path.
    base: ForwardRelativePathBuf,

    /// A path relative to the base path.
    path: ForwardRelativePathBuf,
}

impl BuckOutTestPath {
    pub fn new(base: ForwardRelativePathBuf, path: ForwardRelativePathBuf) -> Self {
        BuckOutTestPath { base, path }
    }

    pub fn into_path(self) -> ForwardRelativePathBuf {
        self.path
    }
}

#[derive(Clone, Allocative)]
pub struct BuckOutPathResolver {
    buck_out_v2: ProjectRelativePathBuf,
}

impl BuckOutPathResolver {
    /// creates a 'BuckOutPathResolver' that will resolve outputs to the provided buck-out root.
    /// If not set, buck_out defaults to "buck-out/v2"
    pub fn new(buck_out_v2: ProjectRelativePathBuf) -> Self {
        BuckOutPathResolver { buck_out_v2 }
    }

    /// Returns the buck-out root.
    pub fn root(&self) -> &ProjectRelativePath {
        &self.buck_out_v2
    }

    /// Resolves a 'BuckOutPath' into a 'ProjectRelativePath' based on the base
    /// directory, target and cell.
    pub fn resolve_gen(
        &self,
        path: &BuildArtifactPath,
    ) -> buck2_error::Result<ProjectRelativePathBuf> {
        self.prefixed_path_for_owner(
            ForwardRelativePath::unchecked_new("gen"),
            path.owner().owner(),
            path.dynamic_actions_action_key()
                .as_ref()
                .map(|x| x.as_str()),
            path.path(),
            false,
        )
    }

    pub fn resolve_offline_cache(
        &self,
        path: &BuildArtifactPath,
    ) -> buck2_error::Result<ProjectRelativePathBuf> {
        self.prefixed_path_for_owner(
            ForwardRelativePath::unchecked_new("offline-cache"),
            path.owner().owner(),
            path.dynamic_actions_action_key()
                .as_ref()
                .map(|x| x.as_str()),
            path.path(),
            false,
        )
    }

    pub fn resolve_external_cell_source(
        &self,
        path: &CellRelativePath,
        origin: ExternalCellOrigin,
    ) -> ProjectRelativePathBuf {
        ProjectRelativePathBuf::from(ForwardRelativePathBuf::concat([
            self.buck_out_v2.as_forward_relative_path(),
            ForwardRelativePath::new("external_cells").unwrap(),
            match origin {
                ExternalCellOrigin::Bundled(_) => ForwardRelativePath::new("bundled").unwrap(),
                ExternalCellOrigin::Git(_) => ForwardRelativePath::new("git").unwrap(),
            },
            match &origin {
                ExternalCellOrigin::Bundled(cell) => {
                    ForwardRelativePath::new(cell.as_str()).unwrap()
                }
                ExternalCellOrigin::Git(setup) => {
                    ForwardRelativePath::new(setup.commit.as_ref()).unwrap()
                }
            },
            path.as_ref(),
        ]))
    }

    pub fn resolve_scratch(
        &self,
        path: &BuckOutScratchPath,
    ) -> buck2_error::Result<ProjectRelativePathBuf> {
        self.prefixed_path_for_owner(
            ForwardRelativePath::unchecked_new("tmp"),
            &path.owner,
            Some(&path.action_key.as_str()),
            &path.path,
            // Fully hash scratch path as it can be very long and cause path too long issue on Windows.
            true,
        )
    }

    /// Resolve a test path
    pub fn resolve_test(&self, path: &BuckOutTestPath) -> ProjectRelativePathBuf {
        ProjectRelativePathBuf::from(ForwardRelativePathBuf::concat([
            self.buck_out_v2.as_forward_relative_path(),
            ForwardRelativePath::new("test").unwrap(),
            &path.base,
            &path.path,
        ]))
    }

    /// Resolve a test path for test discovery
    pub fn resolve_test_discovery(
        &self,
        label: &ConfiguredProvidersLabel,
    ) -> buck2_error::Result<ProjectRelativePathBuf> {
        let path = match label.name() {
            ProvidersName::Default => "default".into(),
            ProvidersName::NonDefault(nd) => match nd.as_ref() {
                NonDefaultProvidersName::Named(names) => names
                    .iter()
                    // Replacing / with + to avoid the path clash for ["foo/bar"] and ["foo", "bar"]
                    .map(|name| name.as_str().replace("/", "+"))
                    .join("/")
                    .into(),
                NonDefaultProvidersName::UnrecognizedFlavor(s) => s.dupe(),
            },
        };
        let path = ForwardRelativePath::unchecked_new(&path);
        self.prefixed_path_for_owner(
            ForwardRelativePath::unchecked_new("test_discovery"),
            &BaseDeferredKey::TargetLabel(label.target().dupe()),
            None,
            &path,
            true,
        )
    }

    fn prefixed_path_for_owner(
        &self,
        prefix: &ForwardRelativePath,
        owner: &BaseDeferredKey,
        action_key: Option<&str>,
        path: &ForwardRelativePath,
        fully_hash_path: bool,
    ) -> buck2_error::Result<ProjectRelativePathBuf> {
        owner.make_hashed_path(&self.buck_out_v2, prefix, action_key, path, fully_hash_path)
    }

    /// This function returns the exact location of the symlink of a given target.
    /// Note that it (deliberately) ignores the configuration and takes no action_key information.
    /// A `None` implies there is no unhashed location.
    pub fn unhashed_gen(&self, path: &BuildArtifactPath) -> Option<ProjectRelativePathBuf> {
        Some(ProjectRelativePathBuf::from(
            ForwardRelativePathBuf::concat([
                self.buck_out_v2.as_ref(),
                ForwardRelativePath::unchecked_new("gen"),
                &path.0.owner.owner().make_unhashed_path()?,
                path.path(),
            ]),
        ))
    }
}

#[cfg(test)]
mod tests {

    use std::path::Path;
    use std::sync::Arc;

    use dupe::Dupe;
    use regex::Regex;

    use crate::category::CategoryRef;
    use crate::cells::CellResolver;
    use crate::cells::cell_root_path::CellRootPathBuf;
    use crate::cells::name::CellName;
    use crate::cells::paths::CellRelativePath;
    use crate::configuration::data::ConfigurationData;
    use crate::deferred::base_deferred_key::BaseDeferredKey;
    use crate::deferred::dynamic::DynamicLambdaIndex;
    use crate::deferred::dynamic::DynamicLambdaResultsKey;
    use crate::deferred::key::DeferredHolderKey;
    use crate::fs::artifact_path_resolver::ArtifactFs;
    use crate::fs::buck_out_path::BuckOutPathResolver;
    use crate::fs::buck_out_path::BuckOutScratchPath;
    use crate::fs::buck_out_path::BuildArtifactPath;
    use crate::fs::paths::abs_norm_path::AbsNormPathBuf;
    use crate::fs::paths::forward_rel_path::ForwardRelativePathBuf;
    use crate::fs::project::ProjectRoot;
    use crate::fs::project_rel_path::ProjectRelativePathBuf;
    use crate::package::PackageLabel;
    use crate::package::source_path::SourcePath;
    use crate::provider::label::ConfiguredProvidersLabel;
    use crate::provider::label::ProviderName;
    use crate::provider::label::ProvidersName;
    use crate::target::label::label::TargetLabel;
    use crate::target::name::TargetNameRef;

    #[test]
    fn buck_path_resolves() -> buck2_error::Result<()> {
        let cell_resolver = CellResolver::testing_with_name_and_path(
            CellName::testing_new("foo"),
            CellRootPathBuf::new(ProjectRelativePathBuf::unchecked_new("bar-cell".into())),
        );
        let buck_out_path_resolver = BuckOutPathResolver::new(
            ProjectRelativePathBuf::unchecked_new("base/buck-out/v2".into()),
        );
        let artifact_fs = ArtifactFs::new(
            cell_resolver,
            buck_out_path_resolver,
            ProjectRoot::new_unchecked(
                AbsNormPathBuf::new(
                    Path::new(if cfg!(windows) {
                        "C:\\project"
                    } else {
                        "/project"
                    })
                    .to_owned(),
                )
                .unwrap(),
            ),
        );

        let resolved = artifact_fs
            .resolve_source(SourcePath::testing_new("foo//baz-package", "faz.file").as_ref())?;

        assert_eq!(
            ProjectRelativePathBuf::unchecked_new("bar-cell/baz-package/faz.file".into()),
            resolved
        );

        assert_eq!(
            artifact_fs
                .resolve_source(SourcePath::testing_new("none_existent//baz", "fazx").as_ref())
                .is_err(),
            true
        );

        Ok(())
    }

    #[test]
    fn buck_output_path_resolves() -> buck2_error::Result<()> {
        let path_resolver = BuckOutPathResolver::new(ProjectRelativePathBuf::unchecked_new(
            "base/buck-out/v2".into(),
        ));

        let pkg = PackageLabel::new(
            CellName::testing_new("foo"),
            CellRelativePath::unchecked_new("baz-package"),
        );
        let target = TargetLabel::new(pkg, TargetNameRef::unchecked_new("target-name"));
        let cfg_target = target.configure(ConfigurationData::testing_new());
        let owner = BaseDeferredKey::TargetLabel(cfg_target);

        let resolved_gen_path = path_resolver.resolve_gen(&BuildArtifactPath::new(
            owner.dupe(),
            ForwardRelativePathBuf::unchecked_new("faz.file".into()),
        ))?;

        let expected_gen_path = Regex::new(
            "base/buck-out/v2/gen/foo/[0-9a-f]{16}/baz-package/__target-name__/faz.file",
        )?;
        assert!(
            expected_gen_path.is_match(resolved_gen_path.as_str()),
            "{}.is_match({})",
            expected_gen_path,
            resolved_gen_path
        );

        let resolved_scratch_path = path_resolver.resolve_scratch(
            &BuckOutScratchPath::new(
                owner,
                CategoryRef::new("category").unwrap(),
                Some(&String::from("blah.file")),
                ForwardRelativePathBuf::new("1_2".to_owned()).unwrap(),
            )
            .unwrap(),
        )?;

        let expected_scratch_path =
            Regex::new("base/buck-out/v2/tmp/foo/[0-9a-f]{16}/category/blah.file")?;
        assert!(
            expected_scratch_path.is_match(resolved_scratch_path.as_str()),
            "{}.is_match({})",
            expected_scratch_path,
            resolved_scratch_path
        );
        Ok(())
    }

    #[test]
    fn buck_target_output_path_resolves() -> buck2_error::Result<()> {
        let path_resolver =
            BuckOutPathResolver::new(ProjectRelativePathBuf::unchecked_new("buck-out".into()));

        let pkg = PackageLabel::new(
            CellName::testing_new("foo"),
            CellRelativePath::unchecked_new("baz-package"),
        );
        let target = TargetLabel::new(pkg, TargetNameRef::unchecked_new("target-name"));
        let cfg_target = target.configure(ConfigurationData::testing_new());
        let owner = BaseDeferredKey::TargetLabel(cfg_target);

        let resolved_gen_path = path_resolver.resolve_gen(&BuildArtifactPath::new(
            owner.dupe(),
            ForwardRelativePathBuf::unchecked_new("quux".to_owned()),
        ))?;

        let expected_gen_path: Regex =
            Regex::new("buck-out/gen/foo/[0-9a-f]{16}/baz-package/__target-name__/quux")?;
        assert!(
            expected_gen_path.is_match(resolved_gen_path.as_str()),
            "{}.is_match({})",
            expected_gen_path,
            resolved_gen_path
        );

        let path = BuildArtifactPath::with_dynamic_actions_action_key(
            DeferredHolderKey::DynamicLambda(Arc::new(DynamicLambdaResultsKey::new(
                DeferredHolderKey::Base(owner.dupe()),
                DynamicLambdaIndex::new(17),
            ))),
            ForwardRelativePathBuf::unchecked_new("quux".to_owned()),
        );
        let resolved_gen_path = path_resolver.resolve_gen(&path)?;

        let expected_gen_path = Regex::new(
            "buck-out/gen/foo/[0-9a-f]{16}/baz-package/__target-name__/__action___17__/quux",
        )?;
        assert!(
            expected_gen_path.is_match(resolved_gen_path.as_str()),
            "{}.is_match({})",
            expected_gen_path,
            resolved_gen_path
        );

        let resolved_scratch_path = path_resolver.resolve_scratch(
            &BuckOutScratchPath::new(
                owner,
                CategoryRef::new("category").unwrap(),
                Some(&String::from(
                    "xxx_some_crazy_long_file_name_that_causes_it_to_be_hashed_xxx.txt",
                )),
                ForwardRelativePathBuf::new(
                    "xxx_some_long_action_key_but_it_doesnt_matter_xxx".to_owned(),
                )
                .unwrap(),
            )
            .unwrap(),
        )?;

        let expected_scratch_path =
            Regex::new("buck-out/tmp/foo/[0-9a-f]{16}/category/_buck_[0-9a-f]{16}")?;
        assert!(
            expected_scratch_path.is_match(resolved_scratch_path.as_str()),
            "{}.is_match({})",
            expected_scratch_path,
            resolved_scratch_path
        );

        Ok(())
    }

    #[test]
    fn test_scratch_path_is_sensible() {
        let pkg = PackageLabel::new(
            CellName::testing_new("foo"),
            CellRelativePath::unchecked_new("baz-package"),
        );
        let target = TargetLabel::new(pkg, TargetNameRef::unchecked_new("target-name"));
        let cfg_target = target.configure(ConfigurationData::testing_new());
        let category = CategoryRef::new("category").unwrap();

        // We expect these all to be valid paths, avoiding weird things we throw in
        BuckOutScratchPath::new(
            BaseDeferredKey::TargetLabel(cfg_target.dupe()),
            category,
            None,
            ForwardRelativePathBuf::new("1_2".to_owned()).unwrap(),
        )
        .unwrap();

        let mk = move |s| {
            BuckOutScratchPath::new(
                BaseDeferredKey::TargetLabel(cfg_target.dupe()),
                category,
                Some(s),
                ForwardRelativePathBuf::new("3_4".to_owned()).unwrap(),
            )
            .unwrap()
            .path
            .as_str()
            .to_owned()
        };

        // We want to preserve reasonable strings
        assert!(mk("hello").ends_with("/hello"));
        assert!(mk("hello/slash").ends_with("/hello/slash"));
        assert!(mk("hello_underscore").ends_with("/hello_underscore"));
        // But hide silly ones
        assert!(!mk("<>weird").contains("<>"));
        assert!(!mk("a space").contains(' '));
        let long_string = str::repeat("q", 10000);
        assert!(!mk(&long_string).contains(&long_string));
        assert!(!mk("foo/../bar").contains(".."));
        // But that we do create different versions
        assert_eq!(mk("normal"), mk("normal"));
        assert_eq!(mk("weird <>"), mk("weird <>"));
        assert_ne!(mk("weird <>"), mk("weird ><"))
    }

    #[test]
    fn test_scratch_path_is_unique() {
        let path_resolver = BuckOutPathResolver::new(ProjectRelativePathBuf::unchecked_new(
            "base/buck-out/v2".into(),
        ));
        let pkg = PackageLabel::new(
            CellName::testing_new("foo"),
            CellRelativePath::unchecked_new("baz-package"),
        );
        let target = TargetLabel::new(pkg, TargetNameRef::unchecked_new("target-name"));
        let cfg_target = target.configure(ConfigurationData::testing_new());

        let mk = move |s: &str, id: &str| {
            path_resolver
                .resolve_scratch(
                    &BuckOutScratchPath::new(
                        BaseDeferredKey::TargetLabel(cfg_target.dupe()),
                        CategoryRef::new("category").unwrap(),
                        Some(id),
                        ForwardRelativePathBuf::new(s.to_owned()).unwrap(),
                    )
                    .unwrap(),
                )
                .unwrap()
                .as_str()
                .to_owned()
        };

        // Same action_key, same identifier are equal
        assert_eq!(mk("same_key", "same_id"), mk("same_key", "same_id"));
        assert_eq!(mk("same_key", "_buck_same"), mk("same_key", "_buck_same"));

        // Same action_key, different identifier are not equal
        assert_ne!(mk("same_key", "diff_id1"), mk("same_key", "diff_id2"));
        assert_ne!(mk("same_key", "_buck_1"), mk("same_key", "_buck_2"));

        // Different action_key, same identifier are not equal
        assert_ne!(mk("diff_key1", "same_id"), mk("diff_key2", "same_id"));
        assert_ne!(mk("diff_key1", "_buck_same"), mk("diff_key2", "_buck_same"));

        // Different action_key, different identifier are not equal
        assert_ne!(mk("diff_key1", "diff_id1"), mk("diff_key2", "diff_id2"));
        assert_ne!(mk("diff_key1", "_buck_1"), mk("diff_key2", "_buck_2"));
    }

    #[test]
    fn test_resolve_test_discovery() -> buck2_error::Result<()> {
        let path_resolver =
            BuckOutPathResolver::new(ProjectRelativePathBuf::unchecked_new("buck-out".into()));

        let pkg = PackageLabel::new(
            CellName::testing_new("foo"),
            CellRelativePath::unchecked_new("baz-package"),
        );
        let target = TargetLabel::new(pkg, TargetNameRef::unchecked_new("target-name"));
        let cfg_target = target.configure(ConfigurationData::testing_new());
        let providers = ProvidersName::Default.push(ProviderName::new_unchecked("bar/baz".into()));
        let providers_label = ConfiguredProvidersLabel::new(cfg_target, providers);
        let result = path_resolver.resolve_test_discovery(&providers_label)?;
        let expected_result = Regex::new("buck-out/test_discovery/foo/[0-9a-f]{16}/bar\\+baz")?;
        assert!(expected_result.is_match(result.as_str()));
        Ok(())
    }
}
