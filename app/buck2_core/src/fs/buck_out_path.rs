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

use crate::base_deferred_key_dyn::BaseDeferredKeyDyn;
use crate::category::Category;
use crate::fs::paths::forward_rel_path::ForwardRelativePath;
use crate::fs::paths::forward_rel_path::ForwardRelativePathBuf;
use crate::fs::project_rel_path::ProjectRelativePath;
use crate::fs::project_rel_path::ProjectRelativePathBuf;

#[derive(Clone, Debug, Display, Allocative, Hash, Eq, PartialEq)]
#[display(fmt = "({})/{}", owner, "path.as_str()")]
struct BuckOutPathData {
    /// The owner responsible for creating this path.
    owner: BaseDeferredKeyDyn,
    /// The unique identifier for this action (only set for outputs inside dynamic actions)
    action_key: Option<Arc<str>>,
    /// The path relative to that target.
    path: ForwardRelativePathBuf,
}

/// Represents a resolvable path corresponding to outputs of rules that are part
/// of a `Package`. The `BuckOutPath` refers to only the outputs of rules,
/// not the repo sources.
///
/// This structure contains a target label for generating the base of the path (base
/// path), and a `ForwardRelativePath` that represents the specific output
/// location relative to the 'base path'.
///
/// For `Eq`/`Hash` we want the equality to be based on the path on disk,
/// regardless of how the path looks to the user. Therefore we ignore the `hidden` field.
#[derive(Clone, Dupe, Debug, Display, Hash, PartialEq, Eq, Allocative)]
pub struct BuckOutPath(Arc<BuckOutPathData>);

impl BuckOutPath {
    pub fn new(owner: BaseDeferredKeyDyn, path: ForwardRelativePathBuf) -> Self {
        Self::with_action_key(owner, path, None)
    }

    pub fn with_action_key(
        owner: BaseDeferredKeyDyn,
        path: ForwardRelativePathBuf,
        action_key: Option<Arc<str>>,
    ) -> Self {
        BuckOutPath(Arc::new(BuckOutPathData {
            owner,
            action_key,
            path,
        }))
    }

    pub fn owner(&self) -> &BaseDeferredKeyDyn {
        &self.0.owner
    }

    pub fn action_key(&self) -> Option<&str> {
        self.0.action_key.as_deref()
    }

    pub fn path(&self) -> &ForwardRelativePath {
        &self.0.path
    }
}

#[derive(Clone, Debug, Display, Eq, PartialEq)]
#[display(fmt = "tmp/({})/{}", owner, "path.as_str()")]
pub struct BuckOutScratchPath {
    /// The deferred responsible for creating this path.
    owner: BaseDeferredKeyDyn,
    /// The path relative to that target.
    path: ForwardRelativePathBuf,
}

impl BuckOutScratchPath {
    /// Returning an Err from this function is considered an internal error - we try
    /// really hard to normalise anything the user supplies.
    pub fn new(
        owner: BaseDeferredKeyDyn,
        category: &Category,
        identifier: Option<&str>,
    ) -> anyhow::Result<Self> {
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
                    let output_hash = format!("{}{:x}", MAKE_SENSIBLE_PREFIX, hasher.finish());
                    path.join_normalized(ForwardRelativePath::new(&output_hash)?)?
                }
            }
            _ => path.to_buf(),
        };

        Ok(Self { owner, path })
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
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
pub struct BuckOutPathResolver(ProjectRelativePathBuf);

impl BuckOutPathResolver {
    /// creates a 'BuckOutPathResolver' that will resolve outputs to the provided buck-out root.
    /// If not set, buck_out defaults to "buck-out/v2"
    pub fn new(buck_out: ProjectRelativePathBuf) -> Self {
        BuckOutPathResolver(buck_out)
    }

    /// Returns the buck-out root.
    pub fn root(&self) -> &ProjectRelativePath {
        &self.0
    }

    /// Resolves a 'BuckOutPath' into a 'ProjectRelativePath' based on the base
    /// directory, target and cell.
    pub fn resolve_gen(&self, path: &BuckOutPath) -> ProjectRelativePathBuf {
        self.prefixed_path_for_owner(
            ForwardRelativePath::unchecked_new("gen"),
            path.owner(),
            path.action_key(),
            path.path(),
        )
    }

    pub fn resolve_scratch(&self, path: &BuckOutScratchPath) -> ProjectRelativePathBuf {
        self.prefixed_path_for_owner(
            ForwardRelativePath::unchecked_new("tmp"),
            &path.owner,
            None,
            &path.path,
        )
    }

    /// Resolve a test path
    pub fn resolve_test(&self, path: &BuckOutTestPath) -> ProjectRelativePathBuf {
        ProjectRelativePathBuf::from(ForwardRelativePathBuf::concat([
            self.0.as_forward_relative_path(),
            ForwardRelativePath::new("test").unwrap(),
            &path.base,
            &path.path,
        ]))
    }

    fn prefixed_path_for_owner(
        &self,
        prefix: &ForwardRelativePath,
        owner: &BaseDeferredKeyDyn,
        action_key: Option<&str>,
        path: &ForwardRelativePath,
    ) -> ProjectRelativePathBuf {
        owner.make_hashed_path(&self.0, prefix, action_key, path)
    }

    /// This function returns the exact location of the symlink of a given target.
    /// Note that it (deliberately) ignores the configuration and takes no action_key information.
    /// A `None` implies there is no unhashed location.
    pub fn unhashed_gen(&self, path: &BuckOutPath) -> Option<ProjectRelativePathBuf> {
        Some(ProjectRelativePathBuf::from(
            ForwardRelativePathBuf::concat([
                self.0.as_ref(),
                ForwardRelativePath::unchecked_new("gen"),
                &path.0.owner.make_unhashed_path()?,
                path.path(),
            ]),
        ))
    }
}

#[cfg(test)]
mod tests {

    use std::sync::Arc;

    use dupe::Dupe;
    use regex::Regex;

    use crate::base_deferred_key_dyn::BaseDeferredKeyDyn;
    use crate::buck_path::path::BuckPath;
    use crate::buck_path::resolver::BuckPathResolver;
    use crate::category::Category;
    use crate::cells::cell_root_path::CellRootPathBuf;
    use crate::cells::name::CellName;
    use crate::cells::paths::CellRelativePath;
    use crate::cells::CellResolver;
    use crate::configuration::data::ConfigurationData;
    use crate::fs::buck_out_path::BuckOutPath;
    use crate::fs::buck_out_path::BuckOutPathResolver;
    use crate::fs::buck_out_path::BuckOutScratchPath;
    use crate::fs::paths::forward_rel_path::ForwardRelativePathBuf;
    use crate::fs::project_rel_path::ProjectRelativePathBuf;
    use crate::package::package_relative_path::PackageRelativePathBuf;
    use crate::package::PackageLabel;
    use crate::target::label::TargetLabel;
    use crate::target::name::TargetNameRef;

    #[test]
    fn buck_path_resolves() -> anyhow::Result<()> {
        let cell_resolver = CellResolver::of_names_and_paths(
            CellName::testing_new("foo"),
            CellRootPathBuf::new(ProjectRelativePathBuf::unchecked_new("bar-cell".into())),
        );
        let path_resolver = BuckPathResolver::new(cell_resolver);

        let resolved = path_resolver.resolve(
            BuckPath::testing_new(
                PackageLabel::new(
                    CellName::testing_new("foo"),
                    CellRelativePath::unchecked_new("baz-package"),
                ),
                PackageRelativePathBuf::unchecked_new("faz.file".into()),
            )
            .as_ref(),
        )?;

        assert_eq!(
            ProjectRelativePathBuf::unchecked_new("bar-cell/baz-package/faz.file".into()),
            resolved
        );

        assert_eq!(
            path_resolver
                .resolve(
                    BuckPath::testing_new(
                        PackageLabel::new(
                            CellName::testing_new("none_existent"),
                            CellRelativePath::unchecked_new("baz")
                        ),
                        PackageRelativePathBuf::unchecked_new("fazx".into())
                    )
                    .as_ref()
                )
                .is_err(),
            true
        );

        Ok(())
    }

    #[test]
    fn buck_output_path_resolves() -> anyhow::Result<()> {
        let path_resolver = BuckOutPathResolver::new(ProjectRelativePathBuf::unchecked_new(
            "base/buck-out/v2".into(),
        ));

        let pkg = PackageLabel::new(
            CellName::testing_new("foo"),
            CellRelativePath::unchecked_new("baz-package"),
        );
        let target = TargetLabel::new(pkg, TargetNameRef::unchecked_new("target-name"));
        let cfg_target = target.configure(ConfigurationData::testing_new());

        let resolved = path_resolver.resolve_gen(&BuckOutPath::new(
            BaseDeferredKeyDyn::TargetLabel(cfg_target),
            ForwardRelativePathBuf::unchecked_new("faz.file".into()),
        ));

        let re =
            Regex::new("base/buck-out/v2/gen/foo/[0-9a-z]+/baz-package/__target-name__/faz.file")?;
        assert!(
            re.is_match(resolved.as_str()),
            "{}.is_match({})",
            re,
            resolved
        );
        Ok(())
    }

    #[test]
    fn buck_target_output_path_resolves() -> anyhow::Result<()> {
        let path_resolver =
            BuckOutPathResolver::new(ProjectRelativePathBuf::unchecked_new("buck-out".into()));

        let pkg = PackageLabel::new(
            CellName::testing_new("foo"),
            CellRelativePath::unchecked_new("baz-package"),
        );
        let target = TargetLabel::new(pkg, TargetNameRef::unchecked_new("target-name"));
        let cfg_target = target.configure(ConfigurationData::testing_new());

        let resolved = path_resolver.resolve_gen(&BuckOutPath::new(
            BaseDeferredKeyDyn::TargetLabel(cfg_target.dupe()),
            ForwardRelativePathBuf::unchecked_new("quux".to_owned()),
        ));

        let re = Regex::new("buck-out/gen/foo/[0-9a-z]+/baz-package/__target-name__/quux")?;
        assert!(
            re.is_match(resolved.as_str()),
            "{}.is_match({})",
            re,
            resolved
        );

        let path = BuckOutPath::with_action_key(
            BaseDeferredKeyDyn::TargetLabel(cfg_target),
            ForwardRelativePathBuf::unchecked_new("quux".to_owned()),
            Some(Arc::from("xxx")),
        );
        let resolved = path_resolver.resolve_gen(&path);

        let re = Regex::new(
            "buck-out/gen/foo/[0-9a-z]+/baz-package/__target-name__/__action__xxx__/quux",
        )?;
        assert!(
            re.is_match(resolved.as_str()),
            "{}.is_match({})",
            re,
            resolved
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
        let category = Category::try_from("category").unwrap();

        // We expect these all to be valid paths, avoiding weird things we throw in
        BuckOutScratchPath::new(
            BaseDeferredKeyDyn::TargetLabel(cfg_target.dupe()),
            &category,
            None,
        )
        .unwrap();

        let mk = move |s| {
            BuckOutScratchPath::new(
                BaseDeferredKeyDyn::TargetLabel(cfg_target.dupe()),
                &category,
                Some(s),
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
}
