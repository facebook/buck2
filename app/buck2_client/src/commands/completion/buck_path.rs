/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::path::Path;

use buck2_common::invocation_roots::find_invocation_roots;
use buck2_common::legacy_configs::cells::BuckConfigBasedCells;
use buck2_core::cells::name::CellName;
use buck2_core::cells::paths::CellRelativePath;
use buck2_core::cells::CellResolver;
use buck2_core::fs::paths::abs_norm_path::AbsNormPath;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePath;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use relative_path::RelativePath;

#[derive(Debug, Clone)]
pub(crate) struct BuckPath {
    given: String,
    abs_path: AbsNormPathBuf,
    cell_name: CellName,
    // cell_path: CellRelativePathBuf,
}

impl BuckPath {
    pub(crate) async fn new(cwd: &Path, s: &str) -> anyhow::Result<Self> {
        let cwd_roots = find_invocation_roots(cwd)?;
        let cwd_abs_path = AbsNormPathBuf::new(cwd.to_path_buf())?;
        let cwd_relative = cwd_roots.project_root.relativize(&cwd_abs_path)?;
        let project_root = cwd_roots.project_root.root();
        let cell_configs = BuckConfigBasedCells::parse_with_config_args(
            &cwd_roots.project_root,
            &[],
            &cwd_relative,
        )
        .await?;
        let resolver = &cell_configs.cell_resolver;

        match s.split("//").collect::<Vec<_>>()[..] {
            [path_str] => {
                let path = Path::new(path_str);
                let abs_path = if path.is_absolute() {
                    AbsNormPathBuf::new(path.to_owned())?
                } else {
                    cwd_abs_path.join_normalized(RelativePath::from_path(&path)?)?
                };

                let cwd_cell_name =
                    Self::path_cell_name(&resolver, project_root, &cwd_roots.cell_root)?;
                let cell_name = Self::path_cell_name(&resolver, project_root, &abs_path)?;

                let cell_path = resolver
                    .get_cell_path_from_abs_path(&abs_path, &cwd_roots.project_root)?
                    .path()
                    .to_owned();
                if cell_name != cwd_cell_name || !Self::is_normalized_path_in_cell(s, &cell_path) {
                    let fixed_given = if s.ends_with('/') && cell_path.as_str() != "" {
                        format!("{}//{}/", cell_name, cell_path)
                    } else {
                        format!("{}//{}", cell_name, cell_path)
                    };
                    Ok(BuckPath {
                        given: fixed_given,
                        abs_path,
                        cell_name,
                        // cell_path,
                    })
                } else {
                    Ok(BuckPath {
                        given: s.to_owned(),
                        abs_path,
                        cell_name,
                        // cell_path,
                    })
                }
            }
            [given_cell_str, cell_path] => {
                let alias_resolver = cell_configs
                    .get_cell_alias_resolver_for_cwd_fast(
                        &cwd_roots.project_root,
                        &cwd_roots.project_root.relativize(&AbsNormPath::new(cwd)?)?,
                    )
                    .await?;
                let given_cell = if given_cell_str == "" {
                    resolver.find(&cwd_roots.project_root.relativize(&cwd_roots.cell_root)?)?
                } else {
                    alias_resolver.resolve(given_cell_str)?
                };
                let abs_path = project_root
                    .join_normalized(&resolver.get(given_cell)?.path().as_forward_relative_path())?
                    .join_normalized(cell_path)?;
                let actual_cell = Self::path_cell_name(&resolver, project_root, &abs_path)?;
                if given_cell == actual_cell {
                    Ok(BuckPath {
                        given: s.to_owned(),
                        abs_path,
                        cell_name: given_cell,
                        // cell_path: CellRelativePath::from_path(cell_path)?.to_owned(),
                    })
                } else {
                    // This is a bit ugly because it breaks expectations --
                    // the "given" path is malformed, so it is corrected to
                    // the closest equivalent path that actually works.
                    let corrected_cell_root = project_root.join_normalized(
                        resolver.get(actual_cell)?.path().as_forward_relative_path(),
                    )?;
                    let corrected_cell_path = abs_path.strip_prefix(corrected_cell_root)?;
                    Ok(BuckPath {
                        given: format!("{}//{}", actual_cell, corrected_cell_path),
                        abs_path,
                        cell_name: actual_cell,
                        // cell_path: corrected_cell_path,
                    })
                }
            }
            _ => Err(anyhow::Error::msg("Poorly formatted BuckPath string")),
        }
    }

    fn path_cell_name(
        resolver: &CellResolver,
        project_root: &AbsNormPath,
        path: &AbsNormPath,
    ) -> anyhow::Result<CellName> {
        let project_delta = path.strip_prefix(project_root)?;
        let project_relative = ProjectRelativePathBuf::from(project_delta.to_buf());
        resolver.find::<ProjectRelativePath>(&project_relative)
    }

    fn is_normalized_path_in_cell(s: &str, cell_path: &CellRelativePath) -> bool {
        let path = Path::new(s);
        match ForwardRelativePath::new(path) {
            Ok(forward_rel_path) => cell_path.ends_with(forward_rel_path),
            Err(_) => false,
        }
    }

    pub(crate) fn abs_path(&self) -> &AbsNormPath {
        &self.abs_path
    }

    // pub(crate) fn canonical(&self) -> String {
    //     format!("{}//{}", self.cell_name, self.cell_path())
    // }

    pub(crate) fn cell_name(&self) -> &CellName {
        &self.cell_name
    }

    // pub(crate) fn cell_path(&self) -> &CellRelativePath {
    //     &self.cell_path
    // }

    pub(crate) fn given(&self) -> &str {
        &self.given
    }

    pub(crate) fn is_full_dir(&self) -> bool {
        let is_root_dir = self.given == "";
        let is_slash_terminated_dir = self.abs_path.is_dir() && self.given.ends_with('/');
        is_root_dir || is_slash_terminated_dir
    }
}

impl std::fmt::Display for BuckPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        write!(f, "{}", self.given)
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use super::*;

    fn paths_to_test_data() -> &'static [&'static str] {
        &[
            "fbcode/buck2/app/buck2_client/test_data",
            "app/buck2_client/test_data",
            "test_data",
        ]
    }

    fn in_dir(d: &str) -> anyhow::Result<PathBuf> {
        let cwd = std::env::current_dir().unwrap();

        let mut candidate = PathBuf::new();
        for path in paths_to_test_data() {
            candidate = cwd.join(path).join(d);
            if candidate.exists() {
                break;
            }
        }

        assert!(candidate.exists(), "test_data directory not found");
        Ok(candidate)
    }

    fn in_root() -> PathBuf {
        let cwd = std::env::current_dir().unwrap();

        let mut candidate = PathBuf::new();
        for path in paths_to_test_data() {
            candidate = cwd.join(path);
            if candidate.exists() {
                break;
            }
        }

        assert!(candidate.exists(), "test_data directory not found");
        candidate
    }

    fn abs_path_from_relative(relative: &str) -> anyhow::Result<AbsNormPathBuf> {
        let root = AbsNormPathBuf::new(in_root())?;
        root.join_normalized(relative)
    }

    #[tokio::test]
    async fn test_can_create_from_a_canonical_path() -> anyhow::Result<()> {
        let cwd = in_root();
        BuckPath::new(&cwd, "root//baredir0/buckdir0a").await?;

        Ok(())
    }

    #[tokio::test]
    async fn test_exposes_abs_path_from_canonical_path() -> anyhow::Result<()> {
        let cwd = in_root();
        let uut = BuckPath::new(&cwd, "root//baredir0/buckdir0a").await?;

        assert_eq!(
            uut.abs_path(),
            abs_path_from_relative("baredir0/buckdir0a")?
        );

        Ok(())
    }

    // #[tokio::test]
    // async fn test_exposes_same_canonical_path_as_provided_in_construction() -> anyhow::Result<()> {
    //     let cwd = in_root();
    //     let uut = BuckPath::new(&cwd, "root//baredir0/buckdir0a").await?;

    //     assert_eq!(uut.canonical(), "root//baredir0/buckdir0a");

    //     Ok(())
    // }

    #[tokio::test]
    async fn test_exposes_cell_from_canonical_path() -> anyhow::Result<()> {
        let cwd = in_root();
        let uut = BuckPath::new(&cwd, "root//baredir0/buckdir0a").await?;

        assert_eq!(uut.cell_name(), &CellName::testing_new("root"));

        Ok(())
    }

    // #[tokio::test]
    // async fn test_exposes_cell_relative_path_from_canonical_path() -> anyhow::Result<()> {
    //     let cwd = in_root();
    //     let uut = BuckPath::new(&cwd, "root//baredir0/buckdir0a").await?;

    //     assert_eq!(
    //         uut.cell_path(),
    //         CellRelativePath::testing_new("baredir0/buckdir0a".into())
    //     );

    //     Ok(())
    // }

    #[tokio::test]
    async fn test_retains_given_user_input_for_later_access() -> anyhow::Result<()> {
        let cwd = in_root();
        let uut = BuckPath::new(&cwd, "root//baredir0/buckdir0a").await?;

        assert_eq!(uut.given(), "root//baredir0/buckdir0a");

        Ok(())
    }

    #[tokio::test]
    async fn test_displays_as_provided_canonical_path() -> anyhow::Result<()> {
        let cwd = in_root();
        let uut = BuckPath::new(&cwd, "root//baredir0/buckdir0a").await?;

        assert_eq!(format!("{}", uut), "root//baredir0/buckdir0a");

        Ok(())
    }

    #[tokio::test]
    async fn test_stringifies_as_provided_canonical_path() -> anyhow::Result<()> {
        let cwd = in_root();
        let uut = BuckPath::new(&cwd, "root//baredir0/buckdir0a").await?;

        assert_eq!(uut.to_string(), "root//baredir0/buckdir0a");

        Ok(())
    }

    #[tokio::test]
    async fn test_identifies_abs_path_for_anonymous_cell() -> anyhow::Result<()> {
        let cwd = in_root();
        let uut = BuckPath::new(&cwd, "//").await?;

        assert_eq!(uut.abs_path(), abs_path_from_relative("")?);

        Ok(())
    }

    // #[tokio::test]
    // async fn test_anonymous_cell_is_translated_to_canonical_form_with_named_cell() -> anyhow::Result<()> {
    //     let cwd = in_root();
    //     let uut = BuckPath::new(&cwd, "//").await?;

    //     assert_eq!(uut.canonical(), "root//");

    //     Ok(())
    // }

    #[tokio::test]
    async fn test_finds_correct_cell_for_anonymous_cell() -> anyhow::Result<()> {
        let cwd = in_root();
        let uut = BuckPath::new(&cwd, "//").await?;

        assert_eq!(uut.cell_name(), &CellName::testing_new("root"));

        Ok(())
    }

    // #[tokio::test]
    // async fn test_exposes_cell_relative_path_from_anonymous_cell() -> anyhow::Result<()> {
    //     let cwd = in_root();
    //     let uut = BuckPath::new(&cwd, "//").await?;

    //     assert_eq!(
    //         uut.cell_path(),
    //         CellRelativePath::testing_new("".into())
    //     );

    //     Ok(())
    // }

    #[tokio::test]
    async fn test_retains_anonymous_cell_for_given_view() -> anyhow::Result<()> {
        let cwd = in_root();
        let uut = BuckPath::new(&cwd, "//").await?;

        assert_eq!(uut.given(), "//");

        Ok(())
    }

    #[tokio::test]
    async fn test_displays_as_anonymous_root() -> anyhow::Result<()> {
        let cwd = in_root();
        let uut = BuckPath::new(&cwd, "//").await?;

        assert_eq!(format!("{}", uut), "//");

        Ok(())
    }

    #[tokio::test]
    async fn test_stringifies_as_anonymous_root() -> anyhow::Result<()> {
        let cwd = in_root();
        let uut = BuckPath::new(&cwd, "//").await?;

        assert_eq!(uut.to_string(), "//");

        Ok(())
    }

    #[tokio::test]
    async fn test_exposes_abs_path_from_canonical_path_in_nested_cell() -> anyhow::Result<()> {
        let cwd = in_root();
        let uut = BuckPath::new(&cwd, "cell1//buck2").await?;

        assert_eq!(uut.abs_path(), abs_path_from_relative("cell1/buck2")?);

        Ok(())
    }

    // #[tokio::test]
    // async fn test_exposes_same_canonical_path_as_nested_canonical_path_provided_in_construction()
    // -> anyhow::Result<()> {
    //     let cwd = in_root();
    //     let uut = BuckPath::new(&cwd, "cell1//buck2").await?;

    //     assert_eq!(uut.canonical(), "cell1//buck2");

    //     Ok(())
    // }

    #[tokio::test]
    async fn test_exposes_cell_from_canonical_path_in_nested_cell() -> anyhow::Result<()> {
        let cwd = in_root();
        let uut = BuckPath::new(&cwd, "cell1//buck2").await?;

        assert_eq!(uut.cell_name(), &CellName::testing_new("cell1"));

        Ok(())
    }

    // #[tokio::test]
    // async fn test_exposes_cell_relative_path_from_canonical_path_in_nested_cell() -> anyhow::Result<()> {
    //     let cwd = in_root();
    //     let uut = BuckPath::new(&cwd, "cell1//buck2").await?;

    //     assert_eq!(
    //         uut.cell_path(),
    //         CellRelativePath::testing_new("buck2".into())
    //     );

    //     Ok(())
    // }

    #[tokio::test]
    async fn test_displays_as_nested_canonical_path_provided_in_construction() -> anyhow::Result<()>
    {
        let cwd = in_root();
        let uut = BuckPath::new(&cwd, "cell1//buck2").await?;

        assert_eq!(format!("{}", uut), "cell1//buck2");

        Ok(())
    }

    #[tokio::test]
    async fn test_stringifies_as_nested_canonical_path_provided_in_construction()
    -> anyhow::Result<()> {
        let cwd = in_root();
        let uut = BuckPath::new(&cwd, "cell1//buck2").await?;

        assert_eq!(uut.to_string(), "cell1//buck2");

        Ok(())
    }

    #[tokio::test]
    async fn test_can_create_from_a_str_relative_path() -> anyhow::Result<()> {
        let cwd = in_root();
        BuckPath::new(&cwd, "baredir0/buckdir0a").await?;

        Ok(())
    }

    #[tokio::test]
    async fn test_exposes_abs_path_from_a_relative_path() -> anyhow::Result<()> {
        let cwd = in_root();
        let uut = BuckPath::new(&cwd, "baredir0/buckdir0a").await?;

        assert_eq!(
            uut.abs_path(),
            abs_path_from_relative("baredir0/buckdir0a")?
        );

        Ok(())
    }

    // #[tokio::test]
    // async fn test_exposes_correct_canonical_path_when_given_relative_from_root_cell() -> anyhow::Result<()>
    // {
    //     let cwd = in_root();
    //     let uut = BuckPath::new(&cwd, "baredir0/buckdir0a").await?;

    //     assert_eq!(uut.canonical(), "root//baredir0/buckdir0a");

    //     Ok(())
    // }

    #[tokio::test]
    async fn test_resolves_relative_path_into_cell() -> anyhow::Result<()> {
        let cwd = in_root();
        let uut = BuckPath::new(&cwd, "baredir0/buckdir0a").await?;

        assert_eq!(uut.cell_name(), &CellName::testing_new("root"));

        Ok(())
    }

    // #[tokio::test]
    // async fn test_resolves_relative_path_into_cell_path() -> anyhow::Result<()> {
    //     let cwd = in_root();
    //     let uut = BuckPath::new(&cwd, "baredir0/buckdir0a").await?;

    //     assert_eq!(
    //         uut.cell_path(),
    //         CellRelativePath::testing_new("baredir0/buckdir0a".into())
    //     );

    //     Ok(())
    // }

    #[tokio::test]
    async fn test_displays_as_provided_relative_path() -> anyhow::Result<()> {
        let cwd = in_root();
        let uut = BuckPath::new(&cwd, "baredir0/buckdir0a").await?;

        assert_eq!(format!("{}", uut), "baredir0/buckdir0a");

        Ok(())
    }

    #[tokio::test]
    async fn test_stringifies_as_provided_relative_path() -> anyhow::Result<()> {
        let cwd = in_root();
        let uut = BuckPath::new(&cwd, "baredir0/buckdir0a").await?;

        assert_eq!(uut.to_string(), "baredir0/buckdir0a");

        Ok(())
    }

    #[tokio::test]
    async fn test_exposes_abs_path_from_a_cross_cell_forward_path() -> anyhow::Result<()> {
        let cwd = in_root();
        let uut = BuckPath::new(&cwd, "cell1/buck2").await?;

        assert_eq!(uut.abs_path(), abs_path_from_relative("cell1/buck2")?);

        Ok(())
    }

    // #[tokio::test]
    // async fn test_exposes_correct_canonical_path_from_a_cross_cell_forward_path() -> anyhow::Result<()> {
    //     let cwd = in_root();
    //     let uut = BuckPath::new(&cwd, "cell1/buck2").await?;

    //     assert_eq!(uut.canonical(), "cell1//buck2");

    //     Ok(())
    // }

    #[tokio::test]
    async fn test_resolves_cell_from_cross_cell_forward_path_in_constructor() -> anyhow::Result<()>
    {
        let cwd = in_root();
        let uut = BuckPath::new(&cwd, "cell1/buck2").await?;

        assert_eq!(uut.cell_name(), &CellName::testing_new("cell1"));

        Ok(())
    }

    // #[tokio::test]
    // async fn test_resolves_cell_path_from_a_cross_cell_forward_path() -> anyhow::Result<()> {
    //     let cwd = in_root();
    //     let uut = BuckPath::new(&cwd, "cell1/buck2").await?;

    //     assert_eq!(
    //         uut.cell_path(),
    //         CellRelativePath::testing_new("buck2".into())
    //     );

    //     Ok(())
    // }

    #[tokio::test]
    async fn test_displays_canonical_path_from_a_cross_cell_forward_path() -> anyhow::Result<()> {
        let cwd = in_root();
        let uut = BuckPath::new(&cwd, "cell1//buck2").await?;

        assert_eq!(format!("{}", uut), "cell1//buck2");

        Ok(())
    }

    #[tokio::test]
    async fn test_stringifies_canonical_path_from_a_cross_cell_forward_path() -> anyhow::Result<()>
    {
        let cwd = in_root();
        let uut = BuckPath::new(&cwd, "cell1/buck2").await?;

        assert_eq!(uut.to_string(), "cell1//buck2");

        Ok(())
    }

    #[tokio::test]
    async fn test_exposes_abs_path_from_a_malformed_cross_cell_forward_path() -> anyhow::Result<()>
    {
        let cwd = in_root();
        let uut = BuckPath::new(&cwd, "root//cell1/buck2").await?;

        assert_eq!(uut.abs_path(), abs_path_from_relative("cell1/buck2")?);

        Ok(())
    }

    // #[tokio::test]
    // async fn test_exposes_correct_canonical_path_from_a_malformed_cross_cell_forward_path() -> anyhow::Result<()> {
    //     let cwd = in_root();
    //     let uut = BuckPath::new(&cwd, "root//cell1/buck2").await?;

    //     assert_eq!(uut.canonical(), "cell1//buck2");

    //     Ok(())
    // }

    #[tokio::test]
    async fn test_resolves_cell_from_malformed_cross_cell_forward_path_in_constructor()
    -> anyhow::Result<()> {
        let cwd = in_root();
        let uut = BuckPath::new(&cwd, "root//cell1/buck2").await?;

        assert_eq!(uut.cell_name(), &CellName::testing_new("cell1"));

        Ok(())
    }

    // #[tokio::test]
    // async fn test_resolves_cell_path_from_a_malformed_cross_cell_forward_path() -> anyhow::Result<()> {
    //     let cwd = in_root();
    //     let uut = BuckPath::new(&cwd, "root//cell1/buck2").await?;

    //     assert_eq!(
    //         uut.cell_path(),
    //         CellRelativePath::testing_new("buck2".into())
    //     );

    //     Ok(())
    // }

    #[tokio::test]
    async fn test_displays_canonical_path_from_a_malformed_cross_cell_forward_path()
    -> anyhow::Result<()> {
        let cwd = in_root();
        let uut = BuckPath::new(&cwd, "root//cell1/buck2").await?;

        assert_eq!(format!("{}", uut), "cell1//buck2");

        Ok(())
    }

    #[tokio::test]
    async fn test_stringifies_canonical_path_from_a_malformed_cross_cell_forward_path()
    -> anyhow::Result<()> {
        let cwd = in_root();
        let uut = BuckPath::new(&cwd, "root//cell1/buck2").await?;

        assert_eq!(uut.to_string(), "cell1//buck2");

        Ok(())
    }

    #[tokio::test]
    async fn test_root_dir_is_a_full_dir() -> anyhow::Result<()> {
        let cwd = in_root();
        let uut = BuckPath::new(&cwd, "").await?;

        assert!(uut.is_full_dir());

        Ok(())
    }

    #[tokio::test]
    async fn test_slash_terminated_dir_is_a_full_dir() -> anyhow::Result<()> {
        let cwd = in_root();
        let uut = BuckPath::new(&cwd, "baredir0/").await?;

        assert!(uut.is_full_dir());

        Ok(())
    }

    #[tokio::test]
    async fn test_partial_with_no_slash_is_not_a_full_dir() -> anyhow::Result<()> {
        let cwd = in_root();
        let uut = BuckPath::new(&cwd, "baredir0").await?;

        assert!(!uut.is_full_dir());

        Ok(())
    }

    #[tokio::test]
    async fn test_fully_qualified_cell_is_a_full_dir() -> anyhow::Result<()> {
        let cwd = in_root();
        let uut = BuckPath::new(&cwd, "cell1//").await?;

        assert!(uut.is_full_dir());

        Ok(())
    }

    #[tokio::test]
    async fn test_bails_on_nonexistent_cell() -> anyhow::Result<()> {
        let cwd = in_root();
        let uut = BuckPath::new(&cwd, "boguscell//").await;

        assert!(uut.is_err());

        Ok(())
    }

    #[tokio::test]
    async fn test_normalizes_absolute_paths_cell_name_in_root() -> anyhow::Result<()> {
        let cwd = in_root();
        let uut = BuckPath::new(&cwd, cwd.join("baredir0").to_str().unwrap()).await?;

        assert_eq!(uut.cell_name(), &CellName::testing_new("root"));

        Ok(())
    }

    #[tokio::test]
    async fn test_normalizes_absolute_paths_in_root() -> anyhow::Result<()> {
        let cwd = in_root();
        let uut = BuckPath::new(&cwd, cwd.join("baredir0").to_str().unwrap()).await?;

        assert_eq!(uut.to_string(), "root//baredir0");

        Ok(())
    }

    #[tokio::test]
    async fn test_normalizes_absolute_paths_cell_name_in_subcell() -> anyhow::Result<()> {
        let cwd = in_dir("cell1")?;
        let uut = BuckPath::new(&cwd, cwd.join("buck2").to_str().unwrap()).await?;

        assert_eq!(uut.cell_name(), &CellName::testing_new("cell1"));

        Ok(())
    }

    #[tokio::test]
    async fn test_normalizes_absolute_paths_in_subcell() -> anyhow::Result<()> {
        let cwd = in_dir("cell1")?;
        let uut = BuckPath::new(&cwd, cwd.join("buck2").to_str().unwrap()).await?;

        assert_eq!(uut.to_string(), "cell1//buck2");

        Ok(())
    }

    #[tokio::test]
    async fn test_can_create_from_a_cell_alias() -> anyhow::Result<()> {
        let cwd = in_dir("cell1/buck2/prelude")?;
        BuckPath::new(&cwd, "cell1_alias//buck2").await?;

        Ok(())
    }

    #[tokio::test]
    async fn test_aliased_object_provides_correct_abs_path() -> anyhow::Result<()> {
        let cwd = in_dir("cell1/buck2/prelude")?;
        let uut = BuckPath::new(&cwd, "cell1_alias//buck2").await?;

        assert_eq!(uut.abs_path(), abs_path_from_relative("cell1/buck2")?);

        Ok(())
    }

    // #[tokio::test]
    // async fn test_aliased_object_provides_non_aliased_canonical_path() -> anyhow::Result<()> {
    //     let cwd = in_dir("cell1/buck2/prelude")?;
    //     let uut = BuckPath::new(&cwd, "cell1_alias//buck2").await?;

    //     assert_eq!(uut.canonical(), "cell1//buck2");

    //     Ok(())
    // }

    #[tokio::test]
    async fn test_aliased_object_provides_non_aliased_cell_name() -> anyhow::Result<()> {
        let cwd = in_dir("cell1/buck2/prelude")?;
        let uut = BuckPath::new(&cwd, "cell1_alias//buck2").await?;

        assert_eq!(uut.cell_name(), &CellName::testing_new("cell1"));

        Ok(())
    }

    // #[tokio::test]
    // async fn test_exposes_cell_relative_path_from_canonical_path() -> anyhow::Result<()> {
    //     let cwd = in_dir("cell1/buck2/prelude")?;
    //     let uut = BuckPath::new(&cwd, "cell1_alias//buck2").await?;

    //     assert_eq!(
    //         uut.cell_path(),
    //         CellRelativePath::testing_new("cell1/buck2".into())
    //     );

    //     Ok(())
    // }

    #[tokio::test]
    async fn test_aliased_object_retains_aliased_cell_in_given() -> anyhow::Result<()> {
        let cwd = in_dir("cell1/buck2/prelude")?;
        let uut = BuckPath::new(&cwd, "cell1_alias//buck2").await?;

        assert_eq!(uut.given(), "cell1_alias//buck2");

        Ok(())
    }

    #[tokio::test]
    async fn test_aliased_object_displays_as_aliased_non_canonical_path() -> anyhow::Result<()> {
        let cwd = in_dir("cell1/buck2/prelude")?;
        let uut = BuckPath::new(&cwd, "cell1_alias//buck2").await?;

        assert_eq!(format!("{}", uut), "cell1_alias//buck2");

        Ok(())
    }

    #[tokio::test]
    async fn test_alaised_object_stringifies_as_aliased_non_canonical_path() -> anyhow::Result<()> {
        let cwd = in_dir("cell1/buck2/prelude")?;
        let uut = BuckPath::new(&cwd, "cell1_alias//buck2").await?;

        assert_eq!(uut.to_string(), "cell1_alias//buck2");

        Ok(())
    }

    #[tokio::test]
    async fn test_creation_returns_error_on_non_local_alias() -> anyhow::Result<()> {
        let cwd = in_dir("cell1/buck2")?;
        let uut = BuckPath::new(&cwd, "cell1_alias//buck2").await;

        assert!(uut.is_err());

        Ok(())
    }
}
