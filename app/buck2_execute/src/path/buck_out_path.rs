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
use std::iter::Peekable;
use std::sync::Arc;

use allocative::Allocative;
use anyhow::Context;
use buck2_core::buck_path::BuckPathRef;
use buck2_core::category::Category;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::cell_path::CellPathRef;
use buck2_core::cells::name::CellName;
use buck2_core::cells::paths::CellRelativePath;
use buck2_core::cells::CellResolver;
use buck2_core::fs::paths::file_name::FileName;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePath;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePathBuf;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_core::package::PackageLabel;
use buck2_core::target::label::TargetLabel;
use buck2_core::target::name::TargetNameRef;
use buck2_interpreter::path::BxlFilePath;
use derive_more::Display;
use dupe::Dupe;
use itertools::Itertools;
use thiserror::Error;

use crate::base_deferred_key::BaseDeferredKey;
use crate::bxl::types::BxlFunctionLabel;

#[derive(Clone, Debug, Display, Allocative, Hash, Eq, PartialEq)]
#[display(fmt = "({})/{}", owner, "path.as_str()")]
struct BuckOutPathData {
    /// The owner responsible for creating this path.
    owner: BaseDeferredKey,
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
    pub fn new(owner: BaseDeferredKey, path: ForwardRelativePathBuf) -> Self {
        Self::with_action_key(owner, path, None)
    }

    pub fn with_action_key(
        owner: BaseDeferredKey,
        path: ForwardRelativePathBuf,
        action_key: Option<Arc<str>>,
    ) -> Self {
        BuckOutPath(Arc::new(BuckOutPathData {
            owner,
            action_key,
            path,
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

    pub fn resolve_cell_path(&self, path: CellPathRef) -> anyhow::Result<ProjectRelativePathBuf> {
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
                    target.exec_cfg().as_ref().map_or("", |x| x.output_hash()),
                    "/",
                    cell_relative_path,
                    if cell_relative_path.is_empty() {
                        ""
                    } else {
                        "/"
                    },
                    "__",
                    target.name().as_str(),
                    "__",
                    "/",
                    if action_key.is_none() {
                        ""
                    } else {
                        "__action__"
                    },
                    action_key.unwrap_or_default(),
                    if action_key.is_none() { "" } else { "__/" },
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
                    target.name().name().as_str(),
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

#[derive(Debug, Error)]
pub(crate) enum BuckOutPathParserError {
    #[error(
        "Malformed buck-out path. Expected format: `buck-out/<isolation_prefix>/<gen|tmp|test|gen-anon|gen-bxl>/<cell_name>/<cfg_hash>/<target_path?>/__<target_name>__/<__action__id__?>/<outputs>`. Actual path was: `{0}`"
    )]
    MalformedOutputPath(String),
}

/// The types of the `buck-out` path. Each type contains the configuration hash.
pub enum BuckOutPathType {
    BxlOutput {
        // `BxlFunctionLabel` contains the `CellPath` to the bxl function.
        bxl_function_label: BxlFunctionLabel,
        config_hash: String,
    },
    AnonOutput {
        path: CellPath,
        target_label: TargetLabel,
        // Rule attr hash is part of anonymous target buck-outs.
        attr_hash: String,
        config_hash: String,
    },
    RuleOutput {
        path: CellPath,
        target_label: TargetLabel,
        // This is the part of the buck-out after the isolation prefix. Ex: `gen/path/to/__target_name/artifact`
        path_after_isolation_prefix: String,
        config_hash: String,
    },
    TestOutput {
        path: CellPath,
        config_hash: String,
    },
    TmpOutput {
        path: CellPath,
        target_label: TargetLabel,
        config_hash: String,
    },
}

pub struct BuckOutPathParser<'v> {
    cell_resolver: &'v CellResolver,
}

fn validate_buck_out_and_isolation_prefix<'v>(
    iter: &mut Peekable<impl Iterator<Item = &'v FileName>>,
) -> anyhow::Result<()> {
    // Validate path starts with buck-out.
    match iter.next() {
        Some(buck_out) => {
            if buck_out != "buck-out" {
                return Err(anyhow::anyhow!("Path does not start with buck-out"));
            }
        }
        None => return Err(anyhow::anyhow!("Path does not start with buck-out")),
    }

    // Advance the iterator to isolation dir.
    match iter.next() {
        Some(_) => Ok(()),
        None => Err(anyhow::anyhow!("Path does not have an isolation dir")),
    }
}

fn get_cell_path<'v>(
    iter: &mut Peekable<impl Iterator<Item = &'v FileName>>,
    cell_resolver: &'v CellResolver,
    generated_prefix: &'v str,
) -> anyhow::Result<(CellPath, String, Option<String>)> {
    let is_anon = generated_prefix == "gen-anon";
    let is_test = generated_prefix == "test";
    // Get cell name and validate it exists
    match iter.next() {
        Some(cell_name) => {
            let cell_name = CellName::unchecked_new(cell_name.as_str());

            cell_resolver.get(cell_name)?;

            // Advance iterator to the config hash
            let config_hash = match iter.next() {
                Some(config_hash) => config_hash,
                None => {
                    return Err(anyhow::anyhow!(
                        "Path does not have a platform configuration"
                    ));
                }
            };

            // Get cell relative path and construct the cell path
            let mut cell_relative_path = CellRelativePath::unchecked_new("").to_owned();

            while let Some(part) = iter.next() {
                cell_relative_path = cell_relative_path.join(part).to_owned();

                // We make sure not to consume the target name part via the iterator.
                match iter.peek() {
                    Some(maybe_target_name) => {
                        let maybe_target_name = maybe_target_name.as_str();
                        // TODO(@wendyy) We assume that the first string that we find that starts and ends with "__"
                        // is the target name. There is a small risk of naming collisions (ex: if there's a directory
                        // name that follows this convention that contains a build file), but I will fix this at a
                        // later date.
                        if (*maybe_target_name).starts_with("__")
                            && (*maybe_target_name).ends_with("__")
                        {
                            // If it's an anonymous target, then the last part before the target name is actually the
                            // hash, and not part of the cell relative path.
                            let cell_path = if is_anon {
                                CellPath::new(
                                    cell_name,
                                    cell_relative_path
                                        .parent()
                                        .with_context(|| "Invalid path for anonymous target")?
                                        .to_buf(),
                                )
                            } else {
                                CellPath::new(cell_name, cell_relative_path.to_buf())
                            };

                            let anon_hash = if is_anon {
                                // Iterator is pointing to the part right before the target name, aka the attr
                                // hash for the anonymous target.
                                Some(part.to_string())
                            } else {
                                None
                            };

                            return Ok((cell_path, config_hash.to_string(), anon_hash));
                        }
                    }
                    None => (),
                }
            }

            if is_test {
                Ok((
                    CellPath::new(cell_name, cell_relative_path.to_buf()),
                    config_hash.to_string(),
                    None,
                ))
            } else {
                Err(anyhow::anyhow!("Invalid target name"))
            }
        }
        None => Err(anyhow::anyhow!("Invalid cell name")),
    }
}

fn get_target_name<'v>(
    iter: &mut Peekable<impl Iterator<Item = &'v FileName>>,
) -> anyhow::Result<&'v str> {
    // Get target name, which is prefixed and suffixed with "__"
    match iter.next() {
        Some(target_name_with_underscores) => {
            let target_name_with_underscores = target_name_with_underscores.as_str();
            let target_name =
                &target_name_with_underscores[2..(target_name_with_underscores.len() - 2)];
            Ok(target_name)
        }
        None => Err(anyhow::anyhow!("Invalid target name")),
    }
}

fn get_target_label<'v>(
    iter: &mut Peekable<impl Iterator<Item = &'v FileName>>,
    path: CellPath,
) -> anyhow::Result<TargetLabel> {
    let target_name = get_target_name(iter)?;
    let package = PackageLabel::new(path.cell(), path.path());
    let target = TargetNameRef::new(target_name)?;
    let target_label = TargetLabel::new(package.dupe(), target);
    Ok(target_label)
}

fn get_bxl_function_label<'v>(
    iter: &mut Peekable<impl Iterator<Item = &'v FileName>>,
    path: CellPath,
) -> anyhow::Result<BxlFunctionLabel> {
    let target_name = get_target_name(iter)?;
    let bxl_path = BxlFilePath::new(path)?;
    let bxl_function_label = BxlFunctionLabel {
        bxl_path,
        name: target_name.to_owned(),
    };

    Ok(bxl_function_label)
}

impl<'v> BuckOutPathParser<'v> {
    pub fn new(cell_resolver: &'v CellResolver) -> BuckOutPathParser {
        BuckOutPathParser { cell_resolver }
    }

    // Validates and parses the buck-out path, returning the `BuckOutPathType`. Assumes
    // that the inputted path is not a symlink.
    pub fn parse(&self, output_path: &str) -> anyhow::Result<BuckOutPathType> {
        self.parse_inner(output_path)
            .with_context(|| BuckOutPathParserError::MalformedOutputPath(output_path.to_owned()))
    }

    fn parse_inner(&self, output_path: &str) -> anyhow::Result<BuckOutPathType> {
        let path_as_forward_rel_path = ForwardRelativePathBuf::new(output_path.to_owned())?;
        let mut iter = path_as_forward_rel_path.iter().peekable();

        validate_buck_out_and_isolation_prefix(&mut iter)?;

        let path_after_isolation_prefix = iter.clone().join("/");

        // Advance the iterator to the prefix (tmp, test, gen, gen-anon, or gen-bxl)
        match iter.next() {
            Some(part) => {
                let result = match part.as_str() {
                    "tmp" => {
                        let (path, config_hash, _) =
                            get_cell_path(&mut iter, self.cell_resolver, "tmp")?;
                        let target_label = get_target_label(&mut iter, path.clone())?;

                        Ok(BuckOutPathType::TmpOutput {
                            path,
                            target_label,
                            config_hash,
                        })
                    }
                    "test" => {
                        let (path, config_hash, _) =
                            get_cell_path(&mut iter, self.cell_resolver, "test")?;

                        Ok(BuckOutPathType::TestOutput { path, config_hash })
                    }
                    "gen" => {
                        let (path, config_hash, _) =
                            get_cell_path(&mut iter, self.cell_resolver, "gen")?;
                        let target_label = get_target_label(&mut iter, path.clone())?;

                        Ok(BuckOutPathType::RuleOutput {
                            path,
                            target_label,
                            path_after_isolation_prefix,
                            config_hash,
                        })
                    }
                    "gen-anon" => {
                        let (path, config_hash, anon_hash) =
                            get_cell_path(&mut iter, self.cell_resolver, "gen-anon")?;
                        let target_label = get_target_label(&mut iter, path.clone())?;

                        Ok(BuckOutPathType::AnonOutput {
                            path,
                            target_label,
                            attr_hash: anon_hash
                                .expect("No hash found in anonymous artifact buck-out"),
                            config_hash,
                        })
                    }
                    "gen-bxl" => {
                        let (path, config_hash, _) =
                            get_cell_path(&mut iter, self.cell_resolver, "gen-bxl")?;
                        let bxl_function_label = get_bxl_function_label(&mut iter, path)?;

                        Ok(BuckOutPathType::BxlOutput {
                            bxl_function_label,
                            config_hash,
                        })
                    }
                    _ => Err(anyhow::anyhow!(
                        "Directory after isolation dir is invalid (should be gen, gen-bxl, gen-anon, tmp, or test)"
                    )),
                };

                // Validate for non-test outputs that the target name is not the last element in the path
                if part != "test" && iter.peek().is_none() {
                    Err(anyhow::anyhow!("No output artifacts found"))
                } else {
                    result
                }
            }
            None => Err(anyhow::anyhow!("Path is empty")),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;
    use std::collections::HashMap;
    use std::sync::Arc;

    use buck2_core::buck_path::BuckPath;
    use buck2_core::category::Category;
    use buck2_core::cells::cell_path::CellPath;
    use buck2_core::cells::cell_root_path::CellRootPath;
    use buck2_core::cells::cell_root_path::CellRootPathBuf;
    use buck2_core::cells::name::CellName;
    use buck2_core::cells::paths::CellRelativePath;
    use buck2_core::cells::testing::CellResolverExt;
    use buck2_core::cells::CellAlias;
    use buck2_core::cells::CellResolver;
    use buck2_core::configuration::ConfigurationData;
    use buck2_core::configuration::ConfigurationDataData;
    use buck2_core::fs::paths::forward_rel_path::ForwardRelativePathBuf;
    use buck2_core::fs::project_rel_path::ProjectRelativePath;
    use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
    use buck2_core::package::package_relative_path::PackageRelativePathBuf;
    use buck2_core::package::PackageLabel;
    use buck2_core::target::label::TargetLabel;
    use buck2_core::target::name::TargetNameRef;
    use buck2_interpreter::path::BxlFilePath;
    use dupe::Dupe;
    use regex::Regex;

    use crate::base_deferred_key::BaseDeferredKey;
    use crate::bxl::types::BxlFunctionLabel;
    use crate::path::buck_out_path::BuckOutPath;
    use crate::path::buck_out_path::BuckOutPathParser;
    use crate::path::buck_out_path::BuckOutPathResolver;
    use crate::path::buck_out_path::BuckOutPathType;
    use crate::path::buck_out_path::BuckOutScratchPath;
    use crate::path::buck_out_path::BuckPathResolver;

    #[test]
    fn buck_path_resolves() -> anyhow::Result<()> {
        let cell_resolver = CellResolver::of_names_and_paths(
            CellName::unchecked_new("root"),
            &[(
                CellName::unchecked_new("foo"),
                CellRootPathBuf::new(ProjectRelativePathBuf::unchecked_new("bar-cell".into())),
            )],
        );
        let path_resolver = BuckPathResolver::new(cell_resolver);

        let resolved = path_resolver.resolve(
            BuckPath::testing_new(
                PackageLabel::new(
                    CellName::unchecked_new("foo"),
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
                            CellName::unchecked_new("none_existant"),
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
            CellName::unchecked_new("foo"),
            CellRelativePath::unchecked_new("baz-package"),
        );
        let target = TargetLabel::new(pkg, TargetNameRef::unchecked_new("target-name"));
        let cfg_target = target.configure(ConfigurationData::testing_new());

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

        let pkg = PackageLabel::new(
            CellName::unchecked_new("foo"),
            CellRelativePath::unchecked_new("baz-package"),
        );
        let target = TargetLabel::new(pkg, TargetNameRef::unchecked_new("target-name"));
        let cfg_target = target.configure(ConfigurationData::testing_new());

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

        let path = BuckOutPath::with_action_key(
            BaseDeferredKey::TargetLabel(cfg_target),
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
            CellName::unchecked_new("foo"),
            CellRelativePath::unchecked_new("baz-package"),
        );
        let target = TargetLabel::new(pkg, TargetNameRef::unchecked_new("target-name"));
        let cfg_target = target.configure(ConfigurationData::testing_new());
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

    fn get_parse_test_cell_resolver() -> anyhow::Result<CellResolver> {
        let cell_path = CellRootPath::new(ProjectRelativePath::new("foo/bar")?);

        let mut cell_alias_map = HashMap::new();
        cell_alias_map.insert(
            CellAlias::new("bar".to_owned()),
            CellName::unchecked_new("bar"),
        );

        let cell_resolver = CellResolver::with_names_and_paths_with_alias(&[(
            CellName::unchecked_new("bar"),
            cell_path.to_buf(),
            cell_alias_map,
        )]);

        Ok(cell_resolver)
    }

    #[test]
    fn test_buck_path_parser_validation() -> anyhow::Result<()> {
        let configuration = ConfigurationData::from_platform(
            "cfg_for//:testing_exec".to_owned(),
            ConfigurationDataData {
                constraints: BTreeMap::new(),
                buckconfigs: BTreeMap::new(),
            },
        )
        .unwrap();
        let cell_resolver = get_parse_test_cell_resolver()?;
        let buck_out_parser = BuckOutPathParser::new(&cell_resolver);

        let malformed_path1 = "does/not/start/with/buck-out/blah/blah";
        let malformed_path2 = "buck-out/v2/invalid_buck_prefix/blah/blah/blah/blah";
        let malformed_path3 = "buck-out/v2/gen/bar/no/target/name/found";
        let malformed_path4 = "buck-out/v2/gen/bar/path/to/target/__but_no_artifacts__";

        let res = buck_out_parser.parse(malformed_path1);
        assert!(res.err().unwrap().to_string().contains("Malformed"));

        let res = buck_out_parser.parse(malformed_path2);
        assert!(res.err().unwrap().to_string().contains("Malformed"));

        let res = buck_out_parser.parse(malformed_path3);
        assert!(res.err().unwrap().to_string().contains("Malformed"));

        let res = buck_out_parser.parse(malformed_path4);
        assert!(res.err().unwrap().to_string().contains("Malformed"));

        let cell_does_not_exist =
            "buck-out/v2/gen/nonexistent_cell/cfg_hash/path/to/target/__target_name__/output";

        let res = buck_out_parser.parse(cell_does_not_exist);
        assert!(res.err().unwrap().to_string().contains("Malformed"));

        let config_hash = configuration.output_hash();
        let no_artifacts_after_target_name = &format!(
            "buck-out/v2/gen/bar/{}/path/to/target/__target_name__",
            config_hash
        );
        let res = buck_out_parser.parse(no_artifacts_after_target_name);
        assert!(res.err().unwrap().to_string().contains("Malformed"));

        Ok(())
    }

    #[test]
    fn test_buck_path_parser() -> anyhow::Result<()> {
        let configuration = ConfigurationData::from_platform(
            "cfg_for//:testing_exec".to_owned(),
            ConfigurationDataData {
                constraints: BTreeMap::new(),
                buckconfigs: BTreeMap::new(),
            },
        )
        .unwrap();
        let cell_resolver = get_parse_test_cell_resolver()?;
        let buck_out_parser = BuckOutPathParser::new(&cell_resolver);

        let pkg = PackageLabel::new(
            CellName::unchecked_new("bar"),
            CellRelativePath::unchecked_new("path/to/target"),
        );

        let expected_target_label = TargetLabel::new(pkg, TargetNameRef::new("target_name")?);

        let expected_cell_path = CellPath::new(
            CellName::unchecked_new("bar"),
            CellRelativePath::unchecked_new("path/to/target").to_owned(),
        );

        let expected_config_hash = configuration.output_hash();

        let rule_path = format!(
            "buck-out/v2/gen/bar/{}/path/to/target/__target_name__/output",
            expected_config_hash
        );

        let res = buck_out_parser.parse(&rule_path)?;

        match res {
            BuckOutPathType::RuleOutput {
                path,
                target_label,
                path_after_isolation_prefix,
                config_hash,
            } => {
                assert_eq!(
                    path_after_isolation_prefix,
                    format!(
                        "gen/bar/{}/path/to/target/__target_name__/output",
                        expected_config_hash
                    )
                );
                assert_eq!(target_label, expected_target_label,);
                assert_eq!(path, expected_cell_path,);
                assert_eq!(config_hash, expected_config_hash,);
            }
            _ => panic!("Should have parsed buck-out path successfully"),
        }

        let tmp_path = format!(
            "buck-out/v2/tmp/bar/{}/path/to/target/__target_name__/output",
            expected_config_hash
        );

        let res = buck_out_parser.parse(&tmp_path)?;

        match res {
            BuckOutPathType::TmpOutput {
                path,
                target_label,
                config_hash,
            } => {
                assert_eq!(path, expected_cell_path,);
                assert_eq!(config_hash, expected_config_hash,);
                assert_eq!(target_label, expected_target_label,);
            }
            _ => panic!("Should have parsed buck-out path successfully"),
        }

        let test_path = format!(
            "buck-out/v2/test/bar/{}/path/to/target/test/output",
            expected_config_hash
        );

        let expected_test_cell_path = CellPath::new(
            CellName::unchecked_new("bar"),
            CellRelativePath::unchecked_new("path/to/target/test/output").to_owned(),
        );

        let res = buck_out_parser.parse(&test_path)?;

        match res {
            BuckOutPathType::TestOutput { path, config_hash } => {
                assert_eq!(path, expected_test_cell_path,);
                assert_eq!(config_hash, expected_config_hash,);
            }
            _ => panic!("Should have parsed buck-out path successfully"),
        }

        let anon_path = format!(
            "buck-out/v2/gen-anon/bar/{}/path/to/target/anon_hash/__target_name__/output",
            expected_config_hash
        );

        let res = buck_out_parser.parse(&anon_path)?;

        match res {
            BuckOutPathType::AnonOutput {
                path,
                target_label,
                attr_hash,
                config_hash,
            } => {
                assert_eq!(target_label, expected_target_label,);
                assert_eq!(path, expected_cell_path,);
                assert_eq!(attr_hash, "anon_hash",);
                assert_eq!(config_hash, expected_config_hash,);
            }
            _ => panic!("Should have parsed buck-out path successfully"),
        }

        let path = format!(
            "buck-out/v2/gen-bxl/bar/{}/path/to/function.bxl/__function_name__/output",
            expected_config_hash
        );

        let res = buck_out_parser.parse(&path)?;

        match res {
            BuckOutPathType::BxlOutput {
                bxl_function_label,
                config_hash,
            } => {
                let path = CellPath::new(
                    CellName::unchecked_new("bar"),
                    CellRelativePath::unchecked_new("path/to/function.bxl").to_owned(),
                );

                let bxl_path = BxlFilePath::new(path)?;
                let expected_bxl_function_label = BxlFunctionLabel {
                    bxl_path,
                    name: "function_name".to_owned(),
                };

                assert_eq!(bxl_function_label, expected_bxl_function_label,);
                assert_eq!(config_hash, expected_config_hash,);
            }
            _ => panic!("Should have parsed buck-out path successfully"),
        }

        Ok(())
    }
}
