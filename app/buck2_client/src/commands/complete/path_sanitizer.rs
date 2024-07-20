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

#[derive(Debug, Clone)]
pub(crate) struct SanitizedPath {
    given: String,
    abs_path: AbsNormPathBuf,
    cell_name: CellName,
    // cell_path: CellRelativePathBuf,
}

impl SanitizedPath {
    pub(crate) async fn new(
        resolver: &CellResolver,
        cwd: &AbsNormPath,
        s: &str,
    ) -> anyhow::Result<Self> {
        let cwd_roots = find_invocation_roots(cwd)?;
        let project_root = cwd_roots.project_root.root();

        match s.split("//").collect::<Vec<_>>()[..] {
            [path_str] => {
                let path = Path::new(path_str);
                let abs_path = if path.is_absolute() {
                    AbsNormPathBuf::new(path.to_owned())?
                } else {
                    cwd.join_normalized(path_str)?
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
                    Ok(SanitizedPath {
                        given: fixed_given,
                        abs_path,
                        cell_name,
                        // cell_path,
                    })
                } else {
                    Ok(SanitizedPath {
                        given: s.to_owned(),
                        abs_path,
                        cell_name,
                        // cell_path,
                    })
                }
            }
            [given_cell_str, cell_path] => {
                let given_cell = if given_cell_str == "" {
                    resolver.find(&cwd_roots.project_root.relativize(&cwd_roots.cell_root)?)?
                } else {
                    let cell_configs = BuckConfigBasedCells::parse_with_config_args(
                        &cwd_roots.project_root,
                        &[],
                        ProjectRelativePath::empty(),
                    )
                    .await?;
                    let alias_resolver = cell_configs
                        .get_cell_alias_resolver_for_cwd_fast(
                            &cwd_roots.project_root,
                            &cwd_roots.project_root.relativize(&AbsNormPath::new(cwd)?)?,
                        )
                        .await?;
                    alias_resolver.resolve(given_cell_str)?
                };
                let abs_path = project_root
                    .join_normalized(&resolver.get(given_cell)?.path().as_forward_relative_path())?
                    .join_normalized(cell_path)?;
                let actual_cell = Self::path_cell_name(&resolver, project_root, &abs_path)?;
                if given_cell == actual_cell {
                    Ok(SanitizedPath {
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
                    Ok(SanitizedPath {
                        given: format!("{}//{}", actual_cell, corrected_cell_path),
                        abs_path,
                        cell_name: actual_cell,
                        // cell_path: corrected_cell_path,
                    })
                }
            }
            _ => Err(anyhow::Error::msg("Poorly formatted SanitizedPath string")),
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

impl std::fmt::Display for SanitizedPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        write!(f, "{}", self.given)
    }
}

#[cfg(test)]
mod tests {
    use buck2_common::legacy_configs::cells::BuckConfigBasedCells;
    use paste::paste;

    use super::*;

    fn paths_to_test_data() -> &'static [&'static str] {
        &[
            "fbcode/buck2/app/buck2_client/test_data",
            "app/buck2_client/test_data",
            "test_data",
        ]
    }

    fn in_dir(d: &str) -> anyhow::Result<AbsNormPathBuf> {
        let cwd = AbsNormPathBuf::new(std::env::current_dir().unwrap())?;

        for path in paths_to_test_data() {
            let candidate = cwd.join_normalized(path)?.join_normalized(d)?;
            if candidate.exists() {
                return Ok(candidate);
            }
        }

        Err(anyhow::anyhow!("test_data directory not found"))
    }

    fn in_root() -> anyhow::Result<AbsNormPathBuf> {
        let cwd = AbsNormPathBuf::new(std::env::current_dir().unwrap())?;

        for path in paths_to_test_data() {
            let candidate = cwd.join_normalized(path)?;
            if candidate.exists() {
                return Ok(candidate);
            }
        }

        Err(anyhow::anyhow!("test_data directory not found"))
    }

    fn abs_path_from_root(relative: &str) -> anyhow::Result<AbsNormPathBuf> {
        let root = in_root()?;
        root.join_normalized(relative)
    }

    fn abs_str_from_root(relative: &str) -> anyhow::Result<String> {
        let path = abs_path_from_root(relative)?;
        Ok(path.to_string())
    }

    fn cell_resolver(cwd: &Path) -> anyhow::Result<CellResolver> {
        let cwd_roots = find_invocation_roots(cwd)?;
        let configs = futures::executor::block_on(BuckConfigBasedCells::parse_with_config_args(
            &cwd_roots.project_root,
            &[],
            &cwd_roots.project_root.relativize(&AbsNormPath::new(cwd)?)?,
        ))?;
        Ok(configs.cell_resolver)
    }

    macro_rules! testy {
        ($test_name:ident($in_dir:expr, $partial:expr) -> {
            abs_path: from_root($abs_dir:literal),
            canonical: $canonical:literal,
            cell_name: $cell_name:literal,
            cell_path: $cell_path:literal,
            given: $given:literal,
            display: $display:literal,
            to_string: $to_string:literal,
        }) => {
            paste! {
                #[tokio::test]
                async fn [<test_ $test_name _verify_abs_path>]() -> anyhow::Result<()> {
                    let cwd = $in_dir;
                    let uut = SanitizedPath::new(&cell_resolver(&cwd)?, &cwd, $partial).await?;

                    assert_eq!(uut.abs_path(), abs_path_from_root($abs_dir)?);

                    Ok(())
                }

                // #[tokio::test]
                // fn [<test_ $test_name _verify_canonical>]() -> anyhow::Result<()> {
                //     let cwd = $in_dir;
                //     let uut = SanitizedPath::new(&cell_resolver(&cwd)?, &cwd, $partial).await?;

                //     assert_eq!(uut.canonical(), $canonical);

                //     Ok(())
                // }

                #[tokio::test]
                async fn [<test_ $test_name _verify_cell_name>]() -> anyhow::Result<()> {
                    let cwd = $in_dir;
                    let uut = SanitizedPath::new(&cell_resolver(&cwd)?, &cwd, $partial).await?;

                    assert_eq!(uut.cell_name(), &CellName::testing_new($cell_name));

                    Ok(())
                }

                // #[tokio::test]
                // async fn [<test_ $test_name _cell_path>]() -> anyhow::Result<()> {
                //     let cwd = $in_dir;
                //     let uut = SanitizedPath::new(&cell_resolver(&cwd)?, &cwd, $partial).await?;

                //     assert_eq!(uut.cell_path(), &CellRelativePath::testing_new($cell_path.into()));

                //     Ok(())
                // }

                #[tokio::test]
                async fn [<test_ $test_name _verify_given>]() -> anyhow::Result<()> {
                    let cwd = $in_dir;
                    let uut = SanitizedPath::new(&cell_resolver(&cwd)?, &cwd, $partial).await?;

                    assert_eq!(uut.given(), $given);

                    Ok(())
                }

                #[tokio::test]
                async fn [<test_ $test_name _verify_display>]() -> anyhow::Result<()> {
                    let cwd = $in_dir;
                    let uut = SanitizedPath::new(&cell_resolver(&cwd)?, &cwd, $partial).await?;

                    assert_eq!(format!("{}", uut), $display);

                    Ok(())
                }

                #[tokio::test]
                async fn [<test_ $test_name _verify_to_string>]() -> anyhow::Result<()> {
                    let cwd = $in_dir;
                    let uut = SanitizedPath::new(&cell_resolver(&cwd)?, &cwd, $partial).await?;

                    assert_eq!(uut.to_string(), $to_string);

                    Ok(())
                }
            }
        };
    }

    #[tokio::test]
    async fn test_can_create_from_a_canonical_path() -> anyhow::Result<()> {
        let cwd = in_root()?;
        SanitizedPath::new(&cell_resolver(&cwd)?, &cwd, "root//baredir0/buckdir0a").await?;

        Ok(())
    }

    #[tokio::test]
    async fn test_can_create_from_a_str_relative_path() -> anyhow::Result<()> {
        let cwd = in_root()?;
        SanitizedPath::new(&cell_resolver(&cwd)?, &cwd, "baredir0/buckdir0a").await?;

        Ok(())
    }

    testy!(canonical_path_in_root_from_root(in_root()?, "root//baredir0/buckdir0a") -> {
        abs_path: from_root("baredir0/buckdir0a"),
        canonical: "root//baredir0/buckdir0a",
        cell_name: "root",
        cell_path: "baredir0/buckdir0a",
        given: "root//baredir0/buckdir0a",
        display: "root//baredir0/buckdir0a",
        to_string: "root//baredir0/buckdir0a",
    });

    testy!(anonymous_cell_from_root(in_root()?, "//") -> {
        abs_path: from_root(""),
        canonical: "//",
        cell_name: "root",
        cell_path: "",
        given: "//",
        display: "//",
        to_string: "//",
    });

    testy!(canonical_cell_path_from_root(in_root()?, "cell1//buck2") -> {
        abs_path: from_root("cell1/buck2"),
        canonical: "cell1//buck2",
        cell_name: "cell1",
        cell_path: "buck2",
        given: "cell1//buck2",
        display: "cell1//buck2",
        to_string: "cell1//buck2",
    });

    testy!(relative_path_from_root(in_root()?, "baredir0/buckdir0a") -> {
        abs_path: from_root("baredir0/buckdir0a"),
        canonical: "root//baredir0/buckdir0a",
        cell_name: "root",
        cell_path: "baredir0/buckdir0a",
        given: "baredir0/buckdir0a",
        display: "baredir0/buckdir0a",
        to_string: "baredir0/buckdir0a",
    });

    testy!(cross_cell_forward_path_from_root(in_root()?, "cell1/buck2") -> {
        abs_path: from_root("cell1/buck2"),
        canonical: "cell1//buck2",
        cell_name: "cell1",
        cell_path: "buck2",
        given: "cell1//buck2", // SanitizedPath is documented as correcting this to cell1//buck2
        display: "cell1//buck2",
        to_string: "cell1//buck2",
    });

    testy!(corrects_malformed_cross_cell_forward_path_from_root(in_root()?, "root//cell1/buck2") -> {
        abs_path: from_root("cell1/buck2"),
        canonical: "cell1//buck2",
        cell_name: "cell1",
        cell_path: "buck2",
        given: "cell1//buck2",
        display: "cell1//buck2",
        to_string: "cell1//buck2",
    });

    #[tokio::test]
    async fn test_root_dir_as_empty_string_is_ready_for_subdirs() -> anyhow::Result<()> {
        let cwd = in_root()?;
        let uut = SanitizedPath::new(&cell_resolver(&cwd)?, &cwd, "").await?;

        assert!(uut.is_full_dir());

        Ok(())
    }

    #[tokio::test]
    async fn test_slash_terminated_dir_is_a_full_dir() -> anyhow::Result<()> {
        let cwd = in_root()?;
        let uut = SanitizedPath::new(&cell_resolver(&cwd)?, &cwd, "baredir0/").await?;

        assert!(uut.is_full_dir());

        Ok(())
    }

    #[tokio::test]
    async fn test_partial_with_no_slash_is_not_a_full_dir() -> anyhow::Result<()> {
        let cwd = in_root()?;
        let uut = SanitizedPath::new(&cell_resolver(&cwd)?, &cwd, "baredir0").await?;

        assert!(!uut.is_full_dir());

        Ok(())
    }

    #[tokio::test]
    async fn test_fully_qualified_cell_is_a_full_dir() -> anyhow::Result<()> {
        let cwd = in_root()?;
        let uut = SanitizedPath::new(&cell_resolver(&cwd)?, &cwd, "cell1//").await?;

        assert!(uut.is_full_dir());

        Ok(())
    }

    #[tokio::test]
    async fn test_bails_on_nonexistent_cell() -> anyhow::Result<()> {
        let cwd = in_root()?;
        let uut = SanitizedPath::new(&cell_resolver(&cwd)?, &cwd, "boguscell//").await;

        assert!(uut.is_err());

        Ok(())
    }

    testy!(absolute_path_from_root(in_root()?, &abs_str_from_root("baredir0")?) -> {
        abs_path: from_root("baredir0"),
        canonical: "root//baredir0",
        cell_name: "root",
        cell_path: "baredir0",
        given: "root//baredir0",
        display: "root//baredir0",
        to_string: "root//baredir0",
    });

    testy!(absolute_path_in_subcell(in_dir("cell1")?, &abs_str_from_root("cell1/buck2")?) -> {
        abs_path: from_root("cell1/buck2"),
        canonical: "cell1//buck2",
        cell_name: "cell1",
        cell_path: "buck2",
        given: "cell1//buck2",
        display: "cell1//buck2",
        to_string: "cell1//buck2",
    });

    testy!(aliased_cell(in_dir("cell1/buck2/prelude")?, "cell1_alias//buck2") -> {
        abs_path: from_root("cell1/buck2"),
        canonical: "cell1//buck2",
        cell_name: "cell1",
        cell_path: "buck2",
        given: "cell1_alias//buck2",
        display: "cell1_alias//buck2",
        to_string: "cell1_alias//buck2",
    });

    #[tokio::test]
    async fn test_creation_returns_error_on_non_local_alias() -> anyhow::Result<()> {
        let cwd = in_dir("cell1/buck2")?;
        let uut = SanitizedPath::new(&cell_resolver(&cwd)?, &cwd, "cell1_alias//buck2").await;

        assert!(uut.is_err());

        Ok(())
    }
}
