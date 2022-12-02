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
use buck2_core::buck_path::BuckPathRef;
use buck2_core::category::Category;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::CellResolver;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePath;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePathBuf;
use buck2_core::fs::project::ProjectRelativePath;
use buck2_core::fs::project::ProjectRelativePathBuf;
use derivative::Derivative;
use derive_more::Display;
use gazebo::prelude::*;

use crate::base_deferred_key::BaseDeferredKey;

#[derive(Clone, Debug, Display, Derivative, Allocative)]
#[derivative(Hash, Eq, PartialEq)]
#[display(fmt = "({})/{}", owner, "path.as_str()")]
struct BuckOutPathData {
    /// The owner responsible for creating this path.
    owner: BaseDeferredKey,
    /// The unique identifier for this action (only set for outputs inside dynamic actions)
    action_key: Option<Arc<str>>,
    /// The path relative to that target.
    path: ForwardRelativePathBuf,
    /// The number of components at the prefix of that path that are
    /// internal details to the rule, not returned by `.short_path`.
    #[derivative(Hash = "ignore", PartialEq = "ignore")]
    hidden_components_count: usize,
}

/// Represents a resolvable path corresponding to outputs of rules that are part
/// of a `Package`. The `BuckOutPath` refers to only the outputs of rules,
/// not the repo sources.
///
/// This structure contains a target label for generating the base of the path (base
/// path), and a `ForwardRelativePath` that represents the specific output
/// location relative to the 'base path'. When a user asks for the `short_path`
/// of a `BuckOutPath`, some of the `path` prefix may be hidden, as requested
/// by `declare_output`.
///
/// For `Eq`/`Hash` we want the equality to be based on the path on disk,
/// regardless of how the path looks to the user. Therefore we ignore the `hidden` field.
#[derive(Clone, Dupe, Debug, Display, Hash, PartialEq, Eq, Allocative)]
pub struct BuckOutPath(Arc<BuckOutPathData>);

impl BuckOutPath {
    pub fn new(owner: BaseDeferredKey, path: ForwardRelativePathBuf) -> Self {
        Self::with_hidden(owner, path, 0)
    }

    /// Create a path where the short_path ignores a number hidden components.
    /// It _must_ be the case that path has at least that many components in it,
    /// or `short_path` will fail later.
    pub fn with_hidden(
        owner: BaseDeferredKey,
        path: ForwardRelativePathBuf,
        hidden_components_count: usize,
    ) -> Self {
        Self::with_hidden_and_action_key(owner, path, hidden_components_count, None)
    }

    pub fn with_hidden_and_action_key(
        owner: BaseDeferredKey,
        path: ForwardRelativePathBuf,
        hidden_components_count: usize,
        action_key: Option<Arc<str>>,
    ) -> Self {
        BuckOutPath(Arc::new(BuckOutPathData {
            owner,
            action_key,
            path,
            hidden_components_count,
        }))
    }

    pub fn owner(&self) -> &BaseDeferredKey {
        &self.0.owner
    }

    pub fn action_key(&self) -> Option<&str> {
        self.0.action_key.as_deref()
    }

    pub fn path(&self) -> &ForwardRelativePath {
        &self.0.path
    }

    // The suffix of `path` that is usually relevant to user rules.
    pub fn short_path(&self) -> &ForwardRelativePath {
        self.0.path
            .strip_prefix_components(self.0.hidden_components_count)
            .unwrap_or_else(|| panic!("Invalid BuckOutPath, `hidden_components_count` greater than the number of path components, {:?}", self))
    }
}

#[derive(Clone, Debug, Display, Hash, Eq, PartialEq)]
#[display(fmt = "tmp/({})/{}", owner, "path.as_str()")]
pub struct BuckOutScratchPath {
    /// The deferred responsible for creating this path.
    owner: BaseDeferredKey,
    /// The path relative to that target.
    path: ForwardRelativePathBuf,
}

impl BuckOutScratchPath {
    /// Returning an Err from this function is considered an internal error - we try
    /// really hard to normalise anything the user supplies.
    pub fn new(
        owner: BaseDeferredKey,
        category: &Category,
        identifier: Option<&str>,
    ) -> anyhow::Result<Self> {
        const MAKE_SENSIBLE_PREFIX: &str = "_buck_";

        /// A path is sensible if it's going to produce a ForwardRelativePath that works and isn't too long.
        /// These are heuristics to make it more likely that paths that are not sensible are replaced, not guarantees.
        fn is_sensible(x: &str) -> Option<&ForwardRelativePath> {
            // We need a way to make something sensible, so disallow this prefix
            if x.starts_with(MAKE_SENSIBLE_PREFIX) {
                return None;
            }
            // If things are too long they don't make good paths
            if x.len() > 200 {
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
    base: Option<ForwardRelativePathBuf>,

    /// A path relative to the base path.
    path: ForwardRelativePathBuf,
}

impl BuckOutTestPath {
    pub fn new(base: ForwardRelativePathBuf, path: ForwardRelativePathBuf) -> Self {
        Self {
            base: Some(base),
            path,
        }
    }

    pub fn into_path(self) -> ForwardRelativePathBuf {
        self.path
    }
}

#[derive(Clone, Dupe, Allocative)]
pub struct BuckPathResolver(CellResolver);

impl BuckPathResolver {
    pub fn new(cells: CellResolver) -> Self {
        BuckPathResolver(cells)
    }

    /// Resolves a 'BuckPath' into a 'ProjectRelativePath' based on the package
    /// and cell.
    pub fn resolve(&self, path: BuckPathRef) -> anyhow::Result<ProjectRelativePathBuf> {
        Ok(self.0.resolve_package(path.package())?.join(path.path()))
    }

    pub fn resolve_cell_path(&self, path: &CellPath) -> anyhow::Result<ProjectRelativePathBuf> {
        Ok(self.0.get(path.cell())?.path().join(path.path()))
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
        ProjectRelativePathBuf::unchecked_new(join(&[
            self.0.as_str(),
            "/",
            "test",
            "/",
            path.base.as_ref().map_or("", |b| b.as_str()),
            if path.base.is_some() { "/" } else { "" },
            path.path.as_str(),
        ]))
    }

    fn prefixed_path_for_owner(
        &self,
        prefix: &ForwardRelativePath,
        owner: &BaseDeferredKey,
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

fn join(parts: &[&str]) -> String {
    let len = parts.iter().map(|p| p.len()).sum();

    let mut ret = String::with_capacity(len);
    for part in parts {
        ret.push_str(part);
    }

    ret
}

impl BaseDeferredKey {
    fn make_unhashed_path(&self) -> Option<ForwardRelativePathBuf> {
        match self {
            BaseDeferredKey::TargetLabel(target) => Some(
                ForwardRelativePath::new(target.pkg().cell_name().as_str())
                    .unwrap()
                    .join(target.pkg().cell_relative_path()),
            ),
            _ => None,
        }
    }

    fn make_hashed_path(
        &self,
        base: &ProjectRelativePath,
        prefix: &ForwardRelativePath,
        action_key: Option<&str>,
        path: &ForwardRelativePath,
    ) -> ProjectRelativePathBuf {
        match self {
            BaseDeferredKey::TargetLabel(target) => {
                let cell_relative_path = target.pkg().cell_relative_path().as_str();

                // It is performance critical that we use slices and allocate via `join` instead of
                // repeated calls to `join` on the path object because `join` allocates on each call,
                // which has a significant impact.
                let parts = [
                    base.as_str(),
                    "/",
                    prefix.as_str(),
                    "/",
                    target.pkg().cell_name().as_str(),
                    "/",
                    target.cfg().output_hash(),
                    if target.exec_cfg().is_some() { "-" } else { "" },
                    target.exec_cfg().map_or("", |x| x.output_hash()),
                    "/",
                    cell_relative_path,
                    if cell_relative_path.is_empty() {
                        ""
                    } else {
                        "/"
                    },
                    "__",
                    target.name().as_ref(),
                    "__",
                    action_key.unwrap_or_default(),
                    if action_key.is_none() { "" } else { "__" },
                    "/",
                    path.as_str(),
                ];

                ProjectRelativePathBuf::unchecked_new(join(&parts))
            }
            BaseDeferredKey::BxlLabel(key) => {
                let label = key.label();
                let cell_relative_path = label.bxl_path.path().path().as_str();

                let output_hash = {
                    let mut hasher = DefaultHasher::new();
                    key.cli_args().hash(&mut hasher);
                    let output_hash = hasher.finish();
                    format!("{:x}", output_hash)
                };

                // It is performance critical that we use slices and allocate via `join` instead of
                // repeated calls to `join` on the path object because `join` allocates on each call,
                // which has a significant impact.
                let parts = [
                    base.as_str(),
                    "/",
                    prefix.as_str(),
                    "-bxl/",
                    label.bxl_path.cell().as_str(),
                    "/",
                    output_hash.as_str(),
                    "/",
                    cell_relative_path,
                    if cell_relative_path.is_empty() {
                        ""
                    } else {
                        "/"
                    },
                    "__",
                    key.label().name.as_str(),
                    "__",
                    action_key.unwrap_or_default(),
                    if action_key.is_none() { "" } else { "__" },
                    "/",
                    path.as_str(),
                ];

                ProjectRelativePathBuf::unchecked_new(join(&parts))
            }
            BaseDeferredKey::AnonTarget(target) => {
                let cell_relative_path = target.name().pkg().cell_relative_path().as_str();

                // It is performance critical that we use slices and allocate via `join` instead of
                // repeated calls to `join` on the path object because `join` allocates on each call,
                // which has a significant impact.
                let parts = [
                    base.as_str(),
                    "/",
                    prefix.as_str(),
                    "-anon/",
                    target.name().pkg().cell_name().as_str(),
                    "/",
                    target.exec_cfg().output_hash(),
                    cell_relative_path,
                    if cell_relative_path.is_empty() {
                        ""
                    } else {
                        "/"
                    },
                    target.rule_type_attrs_hash(),
                    "/__",
                    target.name().name().as_ref(),
                    "__",
                    action_key.unwrap_or_default(),
                    if action_key.is_none() { "" } else { "__" },
                    "/",
                    path.as_str(),
                ];

                ProjectRelativePathBuf::unchecked_new(join(&parts))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use buck2_core::buck_path::BuckPath;
    use buck2_core::category::Category;
    use buck2_core::cells::cell_root_path::CellRootPathBuf;
    use buck2_core::cells::paths::CellRelativePath;
    use buck2_core::cells::testing::CellResolverExt;
    use buck2_core::cells::CellName;
    use buck2_core::cells::CellResolver;
    use buck2_core::configuration::Configuration;
    use buck2_core::fs::paths::forward_rel_path::ForwardRelativePathBuf;
    use buck2_core::fs::project::ProjectRelativePathBuf;
    use buck2_core::package::package_relative_path::PackageRelativePathBuf;
    use buck2_core::package::Package;
    use buck2_core::target::TargetLabel;
    use buck2_core::target::TargetName;
    use gazebo::prelude::*;
    use regex::Regex;

    use crate::base_deferred_key::BaseDeferredKey;
    use crate::path::buck_out_path::BuckOutPath;
    use crate::path::buck_out_path::BuckOutPathResolver;
    use crate::path::buck_out_path::BuckOutScratchPath;
    use crate::path::buck_out_path::BuckPathResolver;

    #[test]
    fn buck_path_resolves() -> anyhow::Result<()> {
        let cell_resolver = CellResolver::of_names_and_paths(&[(
            CellName::unchecked_new("foo".into()),
            CellRootPathBuf::new(ProjectRelativePathBuf::unchecked_new("bar-cell".into())),
        )]);
        let path_resolver = BuckPathResolver::new(cell_resolver);

        let resolved = path_resolver.resolve(
            BuckPath::new(
                Package::new(
                    &CellName::unchecked_new("foo".into()),
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
                    BuckPath::new(
                        Package::new(
                            &CellName::unchecked_new("none_existant".into()),
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

        let pkg = Package::new(
            &CellName::unchecked_new("foo".into()),
            CellRelativePath::unchecked_new("baz-package"),
        );
        let target = TargetLabel::new(pkg, TargetName::unchecked_new("target-name"));
        let cfg_target = target.configure(Configuration::testing_new());

        let resolved = path_resolver.resolve_gen(&BuckOutPath::new(
            BaseDeferredKey::TargetLabel(cfg_target),
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

        let pkg = Package::new(
            &CellName::unchecked_new("foo".to_owned()),
            CellRelativePath::unchecked_new("baz-package"),
        );
        let target = TargetLabel::new(pkg, TargetName::unchecked_new("target-name"));
        let cfg_target = target.configure(Configuration::testing_new());

        let resolved = path_resolver.resolve_gen(&BuckOutPath::new(
            BaseDeferredKey::TargetLabel(cfg_target.dupe()),
            ForwardRelativePathBuf::unchecked_new("quux".to_owned()),
        ));

        let re = Regex::new("buck-out/gen/foo/[0-9a-z]+/baz-package/__target-name__/quux")?;
        assert!(
            re.is_match(resolved.as_str()),
            "{}.is_match({})",
            re,
            resolved
        );

        let path = BuckOutPath::with_hidden_and_action_key(
            BaseDeferredKey::TargetLabel(cfg_target),
            ForwardRelativePathBuf::unchecked_new("quux".to_owned()),
            0,
            Some(Arc::from("xxx")),
        );
        let resolved = path_resolver.resolve_gen(&path);

        let re = Regex::new("buck-out/gen/foo/[0-9a-z]+/baz-package/__target-name__xxx__/quux")?;
        assert!(
            re.is_match(resolved.as_str()),
            "{}.is_match({})",
            re,
            resolved
        );

        Ok(())
    }

    #[test]
    fn buck_out_path_eq() -> anyhow::Result<()> {
        let pkg = Package::new(
            &CellName::unchecked_new("foo".into()),
            CellRelativePath::unchecked_new("baz-package"),
        );
        let target = TargetLabel::new(pkg, TargetName::unchecked_new("target-name"));
        let cfg_target = target.configure(Configuration::testing_new());

        let full = BuckOutPath::new(
            BaseDeferredKey::TargetLabel(cfg_target.dupe()),
            ForwardRelativePathBuf::unchecked_new("foo/bar/baz".to_owned()),
        );
        let hidden = BuckOutPath::with_hidden(
            BaseDeferredKey::TargetLabel(cfg_target),
            ForwardRelativePathBuf::unchecked_new("foo/bar/baz".to_owned()),
            2,
        );
        assert_eq!(full.eq(&full), true);
        assert_eq!(full, hidden);
        assert_eq!(full.short_path().as_str(), "foo/bar/baz");
        assert_eq!(hidden.short_path().as_str(), "baz");

        Ok(())
    }

    #[test]
    fn test_scratch_path_is_sensible() {
        let pkg = Package::new(
            &CellName::unchecked_new("foo".into()),
            CellRelativePath::unchecked_new("baz-package"),
        );
        let target = TargetLabel::new(pkg, TargetName::unchecked_new("target-name"));
        let cfg_target = target.configure(Configuration::testing_new());
        let category = Category::try_from("category").unwrap();

        // We expect these all to be valid paths, avoiding weird things we throw in
        BuckOutScratchPath::new(
            BaseDeferredKey::TargetLabel(cfg_target.dupe()),
            &category,
            None,
        )
        .unwrap();

        let mk = move |s| {
            BuckOutScratchPath::new(
                BaseDeferredKey::TargetLabel(cfg_target.dupe()),
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
