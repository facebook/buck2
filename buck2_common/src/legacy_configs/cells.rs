/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;

use anyhow::Context;
use buck2_core::cells::cell_root_path::CellRootPathBuf;
use buck2_core::cells::CellAlias;
use buck2_core::cells::CellResolver;
use buck2_core::cells::CellsAggregator;
use buck2_core::fs::paths::abs_norm_path::AbsNormPath;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::paths::file_name::FileNameBuf;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePath;
use buck2_core::fs::paths::RelativePath;
use buck2_core::fs::project::ProjectRelativePath;
use buck2_core::fs::project::ProjectRelativePathBuf;
use buck2_core::fs::project::ProjectRoot;
use once_cell::unsync::OnceCell;

use crate::legacy_configs::path::BuckConfigFile;
use crate::legacy_configs::path::DEFAULT_BUCK_CONFIG_FILES;
use crate::legacy_configs::push_all_files_from_a_directory;
use crate::legacy_configs::BuckConfigParseOptions;
use crate::legacy_configs::CellResolutionState;
use crate::legacy_configs::ConfigParserFileOps;
use crate::legacy_configs::DefaultConfigParserFileOps;
use crate::legacy_configs::LegacyBuckConfig;
use crate::legacy_configs::LegacyBuckConfigs;
use crate::legacy_configs::LegacyConfigCmdArg;
use crate::legacy_configs::MainConfigFile;

/// Used for creating a CellResolver in a buckv1-compatible way based on values
/// in .buckconfig in each cell.
///
/// We'll traverse the structure of the `[repositories]` sections starting from
/// the root .buckconfig. All aliases found in the root config will also be
/// available in all other cells (v1 provides that same behavior).
///
/// We don't (currently) enforce that all aliases appear in the root config, but
/// unlike v1, our cells implementation works just fine if that isn't the case.
pub struct BuckConfigBasedCells {
    pub configs_by_name: LegacyBuckConfigs,
    pub cell_resolver: CellResolver,
}

impl BuckConfigBasedCells {
    /// Performs a parse of the root `.buckconfig` for the cell _only_ without following includes
    /// and without parsing any configs for any referenced cells. This means this function might return
    /// an empty mapping if the root `.buckconfig` does not contain the cell definitions.
    pub fn parse_immediate_cell_mapping(project_fs: &ProjectRoot) -> anyhow::Result<CellResolver> {
        Self::parse_immediate_cell_mapping_with_file_ops(project_fs, &DefaultConfigParserFileOps {})
    }

    /// Private function with semantics of `parse_immediate_cell_mapping` but usable for testing.
    pub(crate) fn parse_immediate_cell_mapping_with_file_ops(
        project_fs: &ProjectRoot,
        file_ops: &dyn ConfigParserFileOps,
    ) -> anyhow::Result<CellResolver> {
        let opts = BuckConfigParseOptions {
            follow_includes: false,
            // Cells _still_ need to be parsed because we need to correctly resolved cell aliases
            // against the repository mapping for a particular cell. For example, imagine a cell
            // alias like `//some:path` in an argfle - it's relatively to the cell containing
            // the argfile, so we must have parsed all cells.
            parse_cells: true,
        };
        let cells = Self::parse_with_file_ops_and_options(
            project_fs,
            file_ops,
            &[],
            ProjectRelativePath::empty(),
            opts,
        )?;
        Ok(cells.cell_resolver)
    }

    pub fn parse(project_fs: &ProjectRoot) -> anyhow::Result<Self> {
        Self::parse_with_file_ops(
            project_fs,
            &DefaultConfigParserFileOps {},
            &[],
            ProjectRelativePath::empty(),
        )
    }

    pub fn parse_with_config_args(
        project_fs: &ProjectRoot,
        config_args: &[LegacyConfigCmdArg],
        cwd: &ProjectRelativePath,
    ) -> anyhow::Result<Self> {
        Self::parse_with_file_ops(project_fs, &DefaultConfigParserFileOps {}, config_args, cwd)
    }

    pub fn parse_with_file_ops(
        project_fs: &ProjectRoot,
        file_ops: &dyn ConfigParserFileOps,
        config_args: &[LegacyConfigCmdArg],
        cwd: &ProjectRelativePath,
    ) -> anyhow::Result<Self> {
        let opts = BuckConfigParseOptions {
            follow_includes: true,
            parse_cells: true,
        };
        Self::parse_with_file_ops_and_options(project_fs, file_ops, config_args, cwd, opts)
    }

    fn parse_with_file_ops_and_options(
        project_fs: &ProjectRoot,
        file_ops: &dyn ConfigParserFileOps,
        config_args: &[LegacyConfigCmdArg],
        cwd: &ProjectRelativePath,
        options: BuckConfigParseOptions,
    ) -> anyhow::Result<Self> {
        let mut buckconfigs = HashMap::new();
        let mut work = vec![CellRootPathBuf::new(ProjectRelativePathBuf::try_from(
            "".to_owned(),
        )?)];
        let mut cells_aggregator = CellsAggregator::new();
        let mut root_aliases = HashMap::new();

        // By definition, cell resolution should be happening against the cell mapping defined
        // by the .buckconfig of the project root.
        let cell_resolution = CellResolutionState {
            project_filesystem: project_fs,
            cell_resolver: OnceCell::new(),
            cwd: &project_fs.resolve(cwd),
        };
        // NOTE: This will _not_ perform IO unless it needs to.
        let processed_config_args =
            LegacyBuckConfig::process_config_args(config_args, Some(&cell_resolution), file_ops)?;

        while let Some(path) = work.pop() {
            if buckconfigs.contains_key(&path) {
                continue;
            }

            let mut buckconfig_paths: Vec<MainConfigFile> = Vec::new();

            for buckconfig in DEFAULT_BUCK_CONFIG_FILES {
                match buckconfig {
                    BuckConfigFile::ProjectRelativeFile(file) => {
                        let buckconfig_path = ForwardRelativePath::new(file)?;
                        buckconfig_paths.push(MainConfigFile {
                            path: project_fs
                                .resolve(&path.project_relative_path().join(buckconfig_path)),
                            owned_by_project: true,
                        });
                    }

                    BuckConfigFile::ProjectRelativeFolder(folder) => {
                        let buckconfig_folder_path = ForwardRelativePath::new(folder)?;
                        let buckconfig_folder_abs_path = project_fs
                            .resolve(&path.project_relative_path().join(buckconfig_folder_path));
                        push_all_files_from_a_directory(
                            &mut buckconfig_paths,
                            &buckconfig_folder_abs_path,
                            true,
                        )?;
                    }
                    BuckConfigFile::UserFile(file) => {
                        let home_dir = dirs::home_dir();
                        if let Some(home_dir_path) = home_dir {
                            let buckconfig_path = ForwardRelativePath::new(file)?;
                            buckconfig_paths.push(MainConfigFile {
                                path: AbsNormPath::new(&home_dir_path)?
                                    .join_normalized(buckconfig_path)?,
                                owned_by_project: false,
                            });
                        }
                    }
                    BuckConfigFile::UserFolder(folder) => {
                        let home_dir = dirs::home_dir();
                        if let Some(home_dir_path) = home_dir {
                            let buckconfig_path = ForwardRelativePath::new(folder)?;
                            let buckconfig_folder_abs_path = AbsNormPath::new(&home_dir_path)?
                                .join_normalized(buckconfig_path)?;
                            push_all_files_from_a_directory(
                                &mut buckconfig_paths,
                                &buckconfig_folder_abs_path,
                                false,
                            )?;
                        }
                    }
                    BuckConfigFile::GlobalFile(file) => {
                        buckconfig_paths.push(MainConfigFile {
                            path: AbsNormPathBuf::from(String::from(*file))?,
                            owned_by_project: false,
                        });
                    }
                    BuckConfigFile::GlobalFolder(folder) => {
                        let buckconfig_folder_abs_path =
                            AbsNormPathBuf::from(String::from(*folder))?;
                        push_all_files_from_a_directory(
                            &mut buckconfig_paths,
                            &buckconfig_folder_abs_path,
                            false,
                        )?;
                    }
                }
            }

            let existing_configs: Vec<MainConfigFile> = buckconfig_paths
                .into_iter()
                .filter(|main_config_file| file_ops.file_exists(&main_config_file.path))
                .collect();

            // Must contains a buckconfig owned by project, otherwise no cell can be found.
            // This also check if existing_configs is empty
            let has_project_owned_config = existing_configs
                .iter()
                .any(|main_config_file| main_config_file.owned_by_project);

            if !has_project_owned_config {
                buckconfigs.insert(path, LegacyBuckConfig::empty());
                continue;
            };

            let config = LegacyBuckConfig::parse_with_file_ops_with_includes(
                existing_configs.as_slice(),
                file_ops,
                &processed_config_args,
                options.follow_includes,
            )?;

            if let Some(repositories) = config.get_section("repositories") {
                for (alias, alias_path) in repositories.iter() {
                    let alias_path = alias_path.as_str();
                    let alias_path = path
                        .join_normalized(RelativePath::new(alias_path))
                        .with_context(|| {
                            format!(
                                "expected alias path to be a relative path, but found `{}` for `{}` in buckconfig `{}`",
                                alias_path,
                                alias,
                                path
                            )
                        })?;
                    let alias_path = CellRootPathBuf::new(alias_path);
                    let alias = CellAlias::new(alias.to_owned());
                    if path.as_str() == "" {
                        root_aliases.insert(alias.clone(), alias_path.clone());
                    }
                    cells_aggregator.add_cell_alias_entry(
                        path.clone(),
                        alias,
                        alias_path.clone(),
                    )?;
                    if options.parse_cells {
                        work.push(alias_path);
                    }
                }
            }

            if let Some(buildfiles) = Self::parse_buildfile_name(&config)? {
                cells_aggregator.set_buildfiles(path.clone(), buildfiles);
            }

            buckconfigs.insert(path, config);
        }

        for cell_path in buckconfigs.keys() {
            for (alias, alias_path) in &root_aliases {
                cells_aggregator.add_cell_alias_entry(
                    cell_path.clone(),
                    alias.clone(),
                    alias_path.clone(),
                )?;
            }
        }

        let cell_resolver = cells_aggregator.make_cell_resolver()?;
        let configs_by_name = buckconfigs
            .into_iter()
            .map(|(path, config)| {
                Ok((
                    cell_resolver.find(path.project_relative_path())?.clone(),
                    config,
                ))
            })
            .collect::<anyhow::Result<_>>()?;

        Ok(Self {
            configs_by_name: LegacyBuckConfigs::new(configs_by_name),
            cell_resolver,
        })
    }

    /// Deal with the `buildfile.name` key (and `name_v2`)
    fn parse_buildfile_name(config: &LegacyBuckConfig) -> anyhow::Result<Option<Vec<FileNameBuf>>> {
        fn parse_list(val: &str) -> impl Iterator<Item = &str> {
            val.split(',').map(|v| v.trim())
        }

        // For buck2, we support a slightly different mechanism for setting the buildfile to
        // assist with easier migration from v1 to v2.
        // First, we check the key `buildfile.name_v2`, if this is provided, we use it.
        // Second, if that wasn't provided, we will use `buildfile.name` like buck1 does,
        // but for every entry `FOO` we will insert a preceding `FOO.v2`.
        // If neither of those is provided, we will use the default of `["BUCK.v2", "BUCK"]`.
        // This scheme provides a natural progression to buckv2, with the ability to use separate
        // buildfiles for the two where necessary.
        if let Some(buildfiles_value) = config.get("buildfile", "name_v2") {
            Ok(Some(
                parse_list(buildfiles_value)
                    .map(|s| FileNameBuf::try_from((*s).to_owned()))
                    .collect::<anyhow::Result<Vec<_>>>()?,
            ))
        } else if let Some(buildfiles_value) = config.get("buildfile", "name") {
            let mut buildfiles = Vec::new();
            for buildfile in parse_list(buildfiles_value) {
                buildfiles.push(FileNameBuf::try_from(format!("{}.v2", buildfile))?);
                buildfiles.push(FileNameBuf::try_from(buildfile.to_owned())?);
            }
            Ok(Some(buildfiles))
        } else {
            Ok(None)
        }
    }
}

#[cfg(test)]
mod tests {
    use buck2_core::cells::CellAlias;
    use buck2_core::cells::CellName;
    use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
    use buck2_core::fs::project::ProjectRelativePath;
    use buck2_core::fs::project::ProjectRoot;
    use gazebo::prelude::*;
    use indoc::indoc;

    use crate::legacy_configs::cells::BuckConfigBasedCells;
    use crate::legacy_configs::testing::TestConfigParserFileOps;
    use crate::legacy_configs::tests::assert_config_value;
    use crate::legacy_configs::LegacyConfigCmdArg;

    fn create_project_filesystem() -> ProjectRoot {
        #[cfg(not(windows))]
        let root_path = "/".to_owned();
        #[cfg(windows)]
        let root_path = "C:/".to_owned();
        ProjectRoot::new(AbsNormPathBuf::try_from(root_path).unwrap())
    }

    #[test]
    fn test_cells() -> anyhow::Result<()> {
        let file_ops = TestConfigParserFileOps::new(&[
            (
                "/.buckconfig",
                indoc!(
                    r#"
                            [repositories]
                                root = .
                                other = other/
                                other_alias = other/
                                third_party = third_party/
                        "#
                ),
            ),
            (
                "/other/.buckconfig",
                indoc!(
                    r#"
                            [repositories]
                                root = ..
                                other = .
                                third_party = ../third_party/
                            [buildfile]
                                name = TARGETS
                        "#
                ),
            ),
            (
                "/third_party/.buckconfig",
                indoc!(
                    r#"
                            [repositories]
                                third_party = .
                            [buildfile]
                                name_v2 = OKAY
                                name = OKAY_v1
                        "#
                ),
            ),
        ])?;

        let project_fs = create_project_filesystem();
        let cells = BuckConfigBasedCells::parse_with_file_ops(
            &project_fs,
            &file_ops,
            &[],
            ProjectRelativePath::empty(),
        )?;

        let resolver = &cells.cell_resolver;

        let root_instance = resolver.get(&CellName::unchecked_new("root".to_owned()))?;
        let other_instance = resolver.get(&CellName::unchecked_new("other".to_owned()))?;
        let tp_instance = resolver.get(&CellName::unchecked_new("third_party".to_owned()))?;

        assert_eq!(
            vec!["BUCK.v2", "BUCK"],
            root_instance.buildfiles().map(|n| n.as_str())
        );
        assert_eq!(
            vec!["TARGETS.v2", "TARGETS"],
            other_instance.buildfiles().map(|n| n.as_str())
        );
        assert_eq!(vec!["OKAY"], tp_instance.buildfiles().map(|n| n.as_str()));

        assert_eq!(
            "other",
            root_instance
                .cell_alias_resolver()
                .resolve(&CellAlias::new("other_alias".to_owned()))?
                .as_str()
        );

        assert_eq!(
            "other",
            tp_instance
                .cell_alias_resolver()
                .resolve(&CellAlias::new("other_alias".to_owned()))?
                .as_str()
        );

        assert_eq!("", root_instance.path().as_str());
        assert_eq!("other", other_instance.path().as_str());
        assert_eq!("third_party", tp_instance.path().as_str());

        Ok(())
    }

    #[test]
    fn test_multi_cell_with_config_file() -> anyhow::Result<()> {
        let file_ops = TestConfigParserFileOps::new(&[
            (
                "/.buckconfig",
                indoc!(
                    r#"
                            [repositories]
                                root = .
                                other = other/
                                other_alias = other/
                                third_party = third_party/
                        "#
                ),
            ),
            (
                "/other/.buckconfig",
                indoc!(
                    r#"
                            [repositories]
                                root = ..
                                other = .
                                third_party = ../third_party/
                            [buildfile]
                                name = TARGETS
                        "#
                ),
            ),
            (
                "/third_party/.buckconfig",
                indoc!(
                    r#"
                            [repositories]
                                third_party = .
                            [buildfile]
                                name_v2 = OKAY
                                name = OKAY_v1
                        "#
                ),
            ),
            (
                "/other/cli-conf",
                indoc!(
                    r#"
                            [foo]
                                bar = blah
                        "#
                ),
            ),
        ])?;

        let project_fs = create_project_filesystem();
        #[cfg(not(windows))]
        let file_arg = "/other/cli-conf".to_owned();
        #[cfg(windows)]
        let file_arg = "C:/other/cli-conf".to_owned();
        let cells = BuckConfigBasedCells::parse_with_file_ops(
            &project_fs,
            &file_ops,
            &[LegacyConfigCmdArg::UnresolvedFile(file_arg)],
            ProjectRelativePath::empty(),
        )?;

        let configs = &cells.configs_by_name;
        let root_config = configs
            .get(&CellName::unchecked_new("root".to_owned()))
            .unwrap();
        let other_config = configs
            .get(&CellName::unchecked_new("other".to_owned()))
            .unwrap();
        let tp_config = configs
            .get(&CellName::unchecked_new("third_party".to_owned()))
            .unwrap();

        assert_eq!(root_config.get("foo", "bar"), Some("blah"));
        assert_eq!(other_config.get("foo", "bar"), Some("blah"));
        assert_eq!(tp_config.get("foo", "bar"), Some("blah"));

        Ok(())
    }

    #[test]
    fn test_multi_cell_no_repositories_in_non_root_cell() -> anyhow::Result<()> {
        let file_ops = TestConfigParserFileOps::new(&[
            (
                "/.buckconfig",
                indoc!(
                    r#"
                            [repositories]
                                root = .
                                other = other/
                        "#
                ),
            ),
            (
                "/other/.buckconfig",
                indoc!(
                    r#"
                            [foo]
                                bar = baz
                        "#
                ),
            ),
        ])?;

        let project_fs = create_project_filesystem();
        let cells = BuckConfigBasedCells::parse_with_file_ops(
            &project_fs,
            &file_ops,
            &[],
            ProjectRelativePath::empty(),
        )?;

        let configs = &cells.configs_by_name;

        let ohter_config = configs
            .get(&CellName::unchecked_new("other".to_owned()))
            .unwrap();

        assert_eq!(ohter_config.get("foo", "bar"), Some("baz"));

        Ok(())
    }

    #[test]
    fn test_multi_cell_with_cell_relative() -> anyhow::Result<()> {
        let file_ops = TestConfigParserFileOps::new(&[
            (
                "/.buckconfig",
                indoc!(
                    r#"
                            [repositories]
                                root = .
                                other = other/
                        "#
                ),
            ),
            (
                "/global-conf",
                indoc!(
                    r#"
                            [apple]
                                test_tool = xctool
                        "#
                ),
            ),
            (
                "/other/.buckconfig",
                indoc!(
                    r#"
                            [repositories]
                                root = ..
                                other = .
                            [buildfile]
                                name = TARGETS
                        "#
                ),
            ),
            (
                "/other/app-conf",
                indoc!(
                    r#"
                            [apple]
                                ide = Xcode
                        "#
                ),
            ),
        ])?;

        let project_fs = create_project_filesystem();
        let cells = BuckConfigBasedCells::parse_with_file_ops(
            &project_fs,
            &file_ops,
            &[
                LegacyConfigCmdArg::UnresolvedFile("other//app-conf".to_owned()),
                LegacyConfigCmdArg::UnresolvedFile("//global-conf".to_owned()),
            ],
            ProjectRelativePath::empty(),
        )?;

        let configs = &cells.configs_by_name;
        let other_config = configs
            .get(&CellName::unchecked_new("other".to_owned()))
            .unwrap();

        assert_eq!(other_config.get("apple", "ide"), Some("Xcode"));
        assert_eq!(other_config.get("apple", "test_tool"), Some("xctool"));

        Ok(())
    }

    #[test]
    fn test_local_config_file_overwrite_config_file() -> anyhow::Result<()> {
        let file_ops = TestConfigParserFileOps::new(&[
            (
                "/.buckconfig",
                indoc!(
                    r#"
                            [repositories]
                                root = .
                            [apple]
                                key = value1
                                key2 = value2
                        "#
                ),
            ),
            (
                "/.buckconfig.local",
                indoc!(
                    r#"
                            [orange]
                                key = value3
                            [apple]
                                key2 = value5
                                key3 = value4
                        "#
                ),
            ),
        ])?;

        let project_fs = create_project_filesystem();
        let cells = BuckConfigBasedCells::parse_with_file_ops(
            &project_fs,
            &file_ops,
            &[],
            ProjectRelativePath::empty(),
        )?;

        let configs = &cells.configs_by_name;
        let config = configs
            .get(&CellName::unchecked_new("root".to_owned()))
            .unwrap();
        // No local override
        assert_config_value(config, "apple", "key", "value1");
        // local override to new value
        assert_config_value(config, "apple", "key2", "value5");
        // local override new field
        assert_config_value(config, "apple", "key3", "value4");
        // local override new section
        assert_config_value(config, "orange", "key", "value3");

        Ok(())
    }

    #[test]
    fn test_multi_cell_local_config_file_overwrite_config_file() -> anyhow::Result<()> {
        let file_ops = TestConfigParserFileOps::new(&[
            (
                "/.buckconfig",
                indoc!(
                    r#"
                            [repositories]
                                root = .
                                other = other/
                            [apple]
                                key = value1
                                key2 = value2
                        "#
                ),
            ),
            (
                "/.buckconfig.local",
                indoc!(
                    r#"
                            [orange]
                                key = value3
                            [apple]
                                key2 = value5
                                key3 = value4
                        "#
                ),
            ),
            (
                "/other/.buckconfig",
                indoc!(
                    r#"
                            [repositories]
                                root = ..
                                other = .
                            [apple]
                                key = othervalue1
                                key2 = othervalue2
                        "#
                ),
            ),
            (
                "/other/.buckconfig.local",
                indoc!(
                    r#"
                            [orange]
                                key = othervalue3
                            [apple]
                                key2 = othervalue5
                                key3 = othervalue4
                        "#
                ),
            ),
        ])?;

        let project_fs = create_project_filesystem();
        let cells = BuckConfigBasedCells::parse_with_file_ops(
            &project_fs,
            &file_ops,
            &[],
            ProjectRelativePath::empty(),
        )?;

        let configs = &cells.configs_by_name;
        let root_config = configs
            .get(&CellName::unchecked_new("root".to_owned()))
            .unwrap();
        let other_config = configs
            .get(&CellName::unchecked_new("other".to_owned()))
            .unwrap();

        // No local override
        assert_config_value(root_config, "apple", "key", "value1");
        // local override to new value
        assert_config_value(root_config, "apple", "key2", "value5");
        // local override new field
        assert_config_value(root_config, "apple", "key3", "value4");
        // local override new section
        assert_config_value(root_config, "orange", "key", "value3");

        // No local override
        assert_config_value(other_config, "apple", "key", "othervalue1");
        // local override to new value
        assert_config_value(other_config, "apple", "key2", "othervalue5");
        // local override new field
        assert_config_value(other_config, "apple", "key3", "othervalue4");
        // local override new section
        assert_config_value(other_config, "orange", "key", "othervalue3");

        Ok(())
    }
}
