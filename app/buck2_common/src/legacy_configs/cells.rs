/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cell::OnceCell;
use std::collections::HashMap;
use std::collections::HashSet;
use std::sync::Arc;

use anyhow::Context;
use buck2_core::buck2_env;
use buck2_core::cells::alias::NonEmptyCellAlias;
use buck2_core::cells::cell_root_path::CellRootPath;
use buck2_core::cells::cell_root_path::CellRootPathBuf;
use buck2_core::cells::CellResolver;
use buck2_core::cells::CellsAggregator;
use buck2_core::fs::paths::abs_norm_path::AbsNormPath;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePath;
use buck2_core::fs::paths::RelativePath;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;

use crate::legacy_configs::init::DaemonStartupConfig;
use crate::legacy_configs::path::BuckConfigFile;
use crate::legacy_configs::path::DEFAULT_BUCK_CONFIG_FILES;
use crate::legacy_configs::push_all_files_from_a_directory;
use crate::legacy_configs::BuckConfigParseOptions;
use crate::legacy_configs::CellResolutionState;
use crate::legacy_configs::ConfigDirEntry;
use crate::legacy_configs::ConfigParserFileOps;
use crate::legacy_configs::DefaultConfigParserFileOps;
use crate::legacy_configs::LegacyBuckConfig;
use crate::legacy_configs::LegacyBuckConfigs;
use crate::legacy_configs::LegacyConfigCmdArg;
use crate::legacy_configs::MainConfigFile;
use crate::legacy_configs::ResolvedLegacyConfigArg;

#[derive(Debug, buck2_error::Error)]
enum CellsError {
    #[error(
        "Repository root buckconfig must have `[repositories]` section with a pointer to itself \
        like `root = .` which defines the root cell name"
    )]
    MissingRootCellName,
}

/// Used for creating a CellResolver in a buckv1-compatible way based on values
/// in .buckconfig in each cell.
///
/// We'll traverse the structure of the `[repositories]` sections starting from
/// the root .buckconfig. All aliases found in the root config will also be
/// available in all other cells (v1 provides that same behavior).
///
/// We don't (currently) enforce that all aliases appear in the root config, but
/// unlike v1, our cells implementation works just fine if that isn't the case.
#[derive(Clone)]
pub struct BuckConfigBasedCells {
    pub configs_by_name: LegacyBuckConfigs,
    pub cell_resolver: CellResolver,
    pub config_paths: HashSet<AbsNormPathBuf>,
    pub resolved_args: Arc<[ResolvedLegacyConfigArg]>,
}

impl BuckConfigBasedCells {
    /// Performs a parse of the root `.buckconfig` for the cell _only_ without following includes
    /// and without parsing any configs for any referenced cells. This means this function might return
    /// an empty mapping if the root `.buckconfig` does not contain the cell definitions.
    pub fn parse_immediate_config(project_fs: &ProjectRoot) -> anyhow::Result<ImmediateConfig> {
        Self::parse_immediate_config_with_file_ops(project_fs, &mut DefaultConfigParserFileOps {})
    }

    /// Private function with semantics of `parse_immediate_config` but usable for testing.
    pub(crate) fn parse_immediate_config_with_file_ops(
        project_fs: &ProjectRoot,
        file_ops: &mut dyn ConfigParserFileOps,
    ) -> anyhow::Result<ImmediateConfig> {
        let opts = BuckConfigParseOptions {
            follow_includes: false,
        };
        let cells = Self::parse_with_file_ops_and_options(
            project_fs,
            file_ops,
            &[],
            ProjectRelativePath::empty(),
            opts,
        )?;

        let root_config = cells
            .configs_by_name
            .get(cells.cell_resolver.root_cell())
            .context("No config for root cell")?;

        Ok(ImmediateConfig {
            cell_resolver: cells.cell_resolver,
            daemon_startup_config: DaemonStartupConfig::new(root_config)
                .context("Error loading daemon startup config")?,
        })
    }

    pub fn parse(project_fs: &ProjectRoot) -> anyhow::Result<Self> {
        Self::parse_with_file_ops(
            project_fs,
            &mut DefaultConfigParserFileOps {},
            &[],
            ProjectRelativePath::empty(),
        )
    }

    pub fn parse_with_config_args(
        project_fs: &ProjectRoot,
        config_args: &[LegacyConfigCmdArg],
        cwd: &ProjectRelativePath,
    ) -> anyhow::Result<Self> {
        Self::parse_with_file_ops(
            project_fs,
            &mut DefaultConfigParserFileOps {},
            config_args,
            cwd,
        )
    }

    pub fn parse_with_file_ops(
        project_fs: &ProjectRoot,
        file_ops: &mut dyn ConfigParserFileOps,
        config_args: &[LegacyConfigCmdArg],
        cwd: &ProjectRelativePath,
    ) -> anyhow::Result<Self> {
        let opts = BuckConfigParseOptions {
            follow_includes: true,
        };
        Self::parse_with_file_ops_and_options(project_fs, file_ops, config_args, cwd, opts)
    }

    fn parse_with_file_ops_and_options(
        project_fs: &ProjectRoot,
        file_ops: &mut dyn ConfigParserFileOps,
        config_args: &[LegacyConfigCmdArg],
        cwd: &ProjectRelativePath,
        options: BuckConfigParseOptions,
    ) -> anyhow::Result<Self> {
        // Tracing file ops to record config file accesses on command invocation.
        struct TracingFileOps<'a> {
            inner: &'a mut dyn ConfigParserFileOps,
            trace: HashSet<AbsNormPathBuf>,
        }

        impl ConfigParserFileOps for TracingFileOps<'_> {
            fn read_file_lines(
                &mut self,
                path: &AbsNormPath,
            ) -> anyhow::Result<Box<dyn Iterator<Item = Result<String, std::io::Error>>>>
            {
                self.trace.insert(path.to_buf());
                self.inner.read_file_lines(path)
            }

            fn file_exists(&self, path: &AbsNormPath) -> bool {
                self.inner.file_exists(path)
            }

            fn read_dir(&self, path: &AbsNormPath) -> anyhow::Result<Vec<ConfigDirEntry>> {
                self.inner.read_dir(path)
            }
        }

        let mut file_ops = TracingFileOps {
            inner: file_ops,
            trace: Default::default(),
        };

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
            LegacyBuckConfig::process_config_args(config_args, &cell_resolution, &mut file_ops)?;

        while let Some(path) = work.pop() {
            if buckconfigs.contains_key(&path) {
                continue;
            }

            let buckconfig_paths = get_buckconfig_paths_for_cell(&path, project_fs, &file_ops)?;

            let config = LegacyBuckConfig::parse_with_file_ops_with_includes(
                buckconfig_paths.as_slice(),
                project_fs.resolve(path.as_project_relative_path()),
                &mut file_ops,
                &processed_config_args,
                options.follow_includes,
            )?;

            let is_root = path.is_repo_root();

            let repositories = config
                .get_section("repositories")
                .or_else(|| config.get_section("cells"));
            if let Some(repositories) = repositories {
                for (alias, alias_path) in repositories.iter() {
                    let alias_path = CellRootPathBuf::new(path
                        .join_normalized(RelativePath::new(alias_path.as_str()))
                        .with_context(|| {
                            format!(
                                "expected alias path to be a relative path, but found `{}` for `{}` in buckconfig `{}`",
                                alias_path.as_str(),
                                alias,
                                path
                            )
                        })?);
                    let alias = NonEmptyCellAlias::new(alias.to_owned())?;
                    if is_root {
                        root_aliases.insert(alias.clone(), alias_path.clone());
                    }
                    cells_aggregator.add_cell_entry(path.clone(), alias, alias_path.clone())?;
                    work.push(alias_path);
                }
            }

            if is_root {
                if !cells_aggregator.has_name(&path) {
                    return Err(CellsError::MissingRootCellName.into());
                }
            } else {
                for (alias, alias_path) in &root_aliases {
                    cells_aggregator.add_cell_entry(
                        path.clone(),
                        alias.clone(),
                        alias_path.clone(),
                    )?;
                }
            }

            if let Some(aliases) = config
                .get_section("repository_aliases")
                .or_else(|| config.get_section("cell_aliases"))
            {
                for (alias, destination) in aliases.iter() {
                    let alias = NonEmptyCellAlias::new(alias.to_owned())?;
                    let destination = NonEmptyCellAlias::new(destination.as_str().to_owned())?;
                    let alias_path = cells_aggregator.add_cell_alias(
                        path.clone(),
                        alias.clone(),
                        destination,
                    )?;
                    if is_root {
                        root_aliases.insert(alias, alias_path.clone());
                    }
                }
            }

            buckconfigs.insert(path, config);
        }

        let cell_resolver = cells_aggregator.make_cell_resolver()?;
        let configs_by_name = buckconfigs
            .into_iter()
            .map(|(path, config)| {
                Ok((cell_resolver.find(path.as_project_relative_path())?, config))
            })
            .collect::<anyhow::Result<_>>()?;

        Ok(Self {
            configs_by_name: LegacyBuckConfigs::new(configs_by_name),
            cell_resolver,
            config_paths: file_ops.trace,
            resolved_args: processed_config_args.into_iter().collect(),
        })
    }
}

fn get_buckconfig_paths_for_cell(
    path: &CellRootPath,
    project_fs: &ProjectRoot,
    file_ops: &dyn ConfigParserFileOps,
) -> anyhow::Result<Vec<MainConfigFile>> {
    let skip_default_external_config = buck2_env!("BUCK2_TEST_SKIP_DEFAULT_EXTERNAL_CONFIG", bool)?;

    let mut buckconfig_paths: Vec<MainConfigFile> = Vec::new();

    for buckconfig in DEFAULT_BUCK_CONFIG_FILES {
        if skip_default_external_config && buckconfig.is_external() {
            continue;
        }

        match buckconfig {
            BuckConfigFile::CellRelativeFile(file) => {
                let buckconfig_path = ForwardRelativePath::new(file)?;
                buckconfig_paths.push(MainConfigFile {
                    path: project_fs
                        .resolve(&path.as_project_relative_path().join(buckconfig_path)),
                    owned_by_project: true,
                });
            }

            BuckConfigFile::CellRelativeFolder(folder) => {
                let buckconfig_folder_path = ForwardRelativePath::new(folder)?;
                let buckconfig_folder_abs_path = project_fs
                    .resolve(&path.as_project_relative_path().join(buckconfig_folder_path));
                push_all_files_from_a_directory(
                    &mut buckconfig_paths,
                    &buckconfig_folder_abs_path,
                    true,
                    file_ops,
                )?;
            }
            BuckConfigFile::UserFile(file) => {
                let home_dir = dirs::home_dir();
                if let Some(home_dir_path) = home_dir {
                    let buckconfig_path = ForwardRelativePath::new(file)?;
                    buckconfig_paths.push(MainConfigFile {
                        path: AbsNormPath::new(&home_dir_path)?.join_normalized(buckconfig_path)?,
                        owned_by_project: false,
                    });
                }
            }
            BuckConfigFile::UserFolder(folder) => {
                let home_dir = dirs::home_dir();
                if let Some(home_dir_path) = home_dir {
                    let buckconfig_path = ForwardRelativePath::new(folder)?;
                    let buckconfig_folder_abs_path =
                        AbsNormPath::new(&home_dir_path)?.join_normalized(buckconfig_path)?;
                    push_all_files_from_a_directory(
                        &mut buckconfig_paths,
                        &buckconfig_folder_abs_path,
                        false,
                        file_ops,
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
                let buckconfig_folder_abs_path = AbsNormPathBuf::from(String::from(*folder))?;
                push_all_files_from_a_directory(
                    &mut buckconfig_paths,
                    &buckconfig_folder_abs_path,
                    false,
                    file_ops,
                )?;
            }
        }
    }

    let extra_external_config = buck2_env!("BUCK2_TEST_EXTRA_EXTERNAL_CONFIG")?;

    if let Some(f) = extra_external_config {
        buckconfig_paths.push(MainConfigFile {
            path: AbsNormPathBuf::from(f.to_owned())?,
            owned_by_project: false,
        });
    }

    Ok(buckconfig_paths)
}

/// Limited view of the root config. This does not follow includes.
pub struct ImmediateConfig {
    pub cell_resolver: CellResolver,
    pub daemon_startup_config: DaemonStartupConfig,
}

pub(crate) fn create_project_filesystem() -> ProjectRoot {
    #[cfg(not(windows))]
    let root_path = "/".to_owned();
    #[cfg(windows)]
    let root_path = "C:/".to_owned();
    ProjectRoot::new_unchecked(AbsNormPathBuf::try_from(root_path).unwrap())
}

#[cfg(test)]
mod tests {
    use buck2_core::cells::name::CellName;
    use buck2_core::fs::project_rel_path::ProjectRelativePath;
    use indoc::indoc;

    use crate::legacy_configs::cells::create_project_filesystem;
    use crate::legacy_configs::cells::BuckConfigBasedCells;
    use crate::legacy_configs::key::BuckconfigKeyRef;
    use crate::legacy_configs::testing::TestConfigParserFileOps;
    use crate::legacy_configs::tests::assert_config_value;
    use crate::legacy_configs::LegacyConfigCmdArg;

    #[test]
    fn test_cells() -> anyhow::Result<()> {
        let mut file_ops = TestConfigParserFileOps::new(&[
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
                        "#
                ),
            ),
            (
                "/third_party/.buckconfig",
                indoc!(
                    r#"
                            [repositories]
                                third_party = .
                        "#
                ),
            ),
        ])?;

        let project_fs = create_project_filesystem();
        let cells = BuckConfigBasedCells::parse_with_file_ops(
            &project_fs,
            &mut file_ops,
            &[],
            ProjectRelativePath::empty(),
        )?;

        let resolver = &cells.cell_resolver;

        let root_instance = resolver.get(CellName::testing_new("root"))?;
        let other_instance = resolver.get(CellName::testing_new("other"))?;
        let tp_instance = resolver.get(CellName::testing_new("third_party"))?;

        assert_eq!(
            "other",
            root_instance
                .testing_cell_alias_resolver()
                .resolve("other_alias")?
                .as_str()
        );

        assert_eq!(
            "other",
            tp_instance
                .testing_cell_alias_resolver()
                .resolve("other_alias")?
                .as_str()
        );

        assert_eq!("", root_instance.path().as_str());
        assert_eq!("other", other_instance.path().as_str());
        assert_eq!("third_party", tp_instance.path().as_str());

        Ok(())
    }

    #[test]
    fn test_multi_cell_with_config_file() -> anyhow::Result<()> {
        let mut file_ops = TestConfigParserFileOps::new(&[
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
        let file_arg = "/other/cli-conf";
        #[cfg(windows)]
        let file_arg = "C:/other/cli-conf";
        let cells = BuckConfigBasedCells::parse_with_file_ops(
            &project_fs,
            &mut file_ops,
            &[LegacyConfigCmdArg::file(file_arg)?],
            ProjectRelativePath::empty(),
        )?;

        let configs = &cells.configs_by_name;
        let root_config = configs.get(CellName::testing_new("root")).unwrap();
        let other_config = configs.get(CellName::testing_new("other")).unwrap();
        let tp_config = configs.get(CellName::testing_new("third_party")).unwrap();

        assert_eq!(
            root_config.get(BuckconfigKeyRef {
                section: "foo",
                property: "bar"
            }),
            Some("blah")
        );
        assert_eq!(
            other_config.get(BuckconfigKeyRef {
                section: "foo",
                property: "bar"
            }),
            Some("blah")
        );
        assert_eq!(
            tp_config.get(BuckconfigKeyRef {
                section: "foo",
                property: "bar"
            }),
            Some("blah")
        );

        Ok(())
    }

    #[test]
    fn test_multi_cell_no_repositories_in_non_root_cell() -> anyhow::Result<()> {
        let mut file_ops = TestConfigParserFileOps::new(&[
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
            &mut file_ops,
            &[],
            ProjectRelativePath::empty(),
        )?;

        let configs = &cells.configs_by_name;

        let other_config = configs.get(CellName::testing_new("other")).unwrap();

        assert_eq!(
            other_config.get(BuckconfigKeyRef {
                section: "foo",
                property: "bar"
            }),
            Some("baz")
        );

        Ok(())
    }

    #[test]
    fn test_multi_cell_with_cell_relative() -> anyhow::Result<()> {
        let mut file_ops = TestConfigParserFileOps::new(&[
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
            &mut file_ops,
            &[
                LegacyConfigCmdArg::file("other//app-conf")?,
                LegacyConfigCmdArg::file("//global-conf")?,
            ],
            ProjectRelativePath::empty(),
        )?;

        let configs = &cells.configs_by_name;
        let other_config = configs.get(CellName::testing_new("other")).unwrap();

        assert_eq!(
            other_config.get(BuckconfigKeyRef {
                section: "apple",
                property: "ide"
            }),
            Some("Xcode")
        );
        assert_eq!(
            other_config.get(BuckconfigKeyRef {
                section: "apple",
                property: "test_tool"
            }),
            Some("xctool")
        );

        Ok(())
    }

    #[test]
    fn test_local_config_file_overwrite_config_file() -> anyhow::Result<()> {
        let mut file_ops = TestConfigParserFileOps::new(&[
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
            &mut file_ops,
            &[],
            ProjectRelativePath::empty(),
        )?;

        let configs = &cells.configs_by_name;
        let config = configs.get(CellName::testing_new("root")).unwrap();
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
        let mut file_ops = TestConfigParserFileOps::new(&[
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
            &mut file_ops,
            &[],
            ProjectRelativePath::empty(),
        )?;

        let configs = &cells.configs_by_name;
        let root_config = configs.get(CellName::testing_new("root")).unwrap();
        let other_config = configs.get(CellName::testing_new("other")).unwrap();

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

    #[test]
    fn test_config_arg_with_no_buckconfig() -> anyhow::Result<()> {
        let mut file_ops = TestConfigParserFileOps::new(&[(
            "/.buckconfig",
            indoc!(
                r#"
                        [repositories]
                            root = .
                            other = other
                    "#
            ),
        )])?;
        let project_fs = create_project_filesystem();

        let configs = BuckConfigBasedCells::parse_with_file_ops(
            &project_fs,
            &mut file_ops,
            &[LegacyConfigCmdArg::flag("some_section.key=value1")?],
            ProjectRelativePath::empty(),
        )?
        .configs_by_name;
        let config = configs.get(CellName::testing_new("other")).unwrap();

        assert_config_value(config, "some_section", "key", "value1");

        Ok(())
    }

    #[test]
    fn test_cell_config_section_name() -> anyhow::Result<()> {
        let mut file_ops = TestConfigParserFileOps::new(&[(
            "/.buckconfig",
            indoc!(
                r#"
                            [cells]
                                root = .
                                other = other/
                            [cell_aliases]
                                other_alias = other
                        "#
            ),
        )])?;

        let project_fs = create_project_filesystem();
        let resolver = BuckConfigBasedCells::parse_with_file_ops(
            &project_fs,
            &mut file_ops,
            &[],
            ProjectRelativePath::empty(),
        )?
        .cell_resolver;

        let root = resolver.get(CellName::testing_new("root")).unwrap();

        assert_eq!(
            root.testing_cell_alias_resolver()
                .resolve("other")
                .unwrap()
                .as_str(),
            "other"
        );
        assert_eq!(
            root.testing_cell_alias_resolver()
                .resolve("other_alias")
                .unwrap()
                .as_str(),
            "other"
        );

        Ok(())
    }
}
