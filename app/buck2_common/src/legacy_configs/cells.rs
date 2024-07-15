/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::collections::HashSet;
use std::sync::Arc;

use anyhow::Context;
use buck2_core::buck2_env;
use buck2_core::cells::alias::NonEmptyCellAlias;
use buck2_core::cells::cell_root_path::CellRootPath;
use buck2_core::cells::cell_root_path::CellRootPathBuf;
use buck2_core::cells::external::ExternalCellOrigin;
use buck2_core::cells::external::GitCellSetup;
use buck2_core::cells::name::CellName;
use buck2_core::cells::CellResolver;
use buck2_core::cells::CellsAggregator;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::paths::abs_path::AbsPath;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePath;
use buck2_core::fs::paths::RelativePath;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_error::BuckErrorContext;
use dice::DiceComputations;
use dupe::Dupe;

use crate::cas_digest::RawDigest;
use crate::dice::cells::HasCellResolver;
use crate::dice::data::HasIoProvider;
use crate::external_cells::EXTERNAL_CELLS_IMPL;
use crate::legacy_configs::args::resolve_config_args;
use crate::legacy_configs::args::ResolvedLegacyConfigArg;
use crate::legacy_configs::configs::BuckConfigParseOptions;
use crate::legacy_configs::configs::LegacyBuckConfig;
use crate::legacy_configs::configs::LegacyBuckConfigs;
use crate::legacy_configs::dice::HasInjectedLegacyConfigs;
use crate::legacy_configs::file_ops::push_all_files_from_a_directory;
use crate::legacy_configs::file_ops::ConfigDirEntry;
use crate::legacy_configs::file_ops::ConfigParserFileOps;
use crate::legacy_configs::file_ops::ConfigPath;
use crate::legacy_configs::file_ops::DefaultConfigParserFileOps;
use crate::legacy_configs::file_ops::DiceConfigFileOps;
use crate::legacy_configs::path::ExternalConfigSource;
use crate::legacy_configs::path::ProjectConfigSource;
use crate::legacy_configs::path::DEFAULT_EXTERNAL_CONFIG_SOURCES;
use crate::legacy_configs::path::DEFAULT_PROJECT_CONFIG_SOURCES;

#[derive(Debug, buck2_error::Error)]
enum CellsError {
    #[error(
        "Repository root buckconfig must have `[cells]` section with a pointer to itself \
        like `root = .` which defines the root cell name"
    )]
    MissingRootCellName,
    #[error("Unknown cell name `{}` when parsing external cell declarations", _0)]
    UnknownCellName(NonEmptyCellAlias),
}

/// Used for creating a CellResolver in a buckv1-compatible way based on values
/// in .buckconfig in each cell.
///
/// We'll traverse the structure of the `[cells]` sections starting from
/// the root .buckconfig. All aliases found in the root config will also be
/// available in all other cells (v1 provides that same behavior).
///
/// We don't (currently) enforce that all aliases appear in the root config, but
/// unlike v1, our cells implementation works just fine if that isn't the case.
#[derive(Clone)]
pub struct BuckConfigBasedCells {
    pub configs_by_name: LegacyBuckConfigs,
    pub cell_resolver: CellResolver,
    pub config_paths: HashSet<ConfigPath>,
    pub resolved_args: Arc<[ResolvedLegacyConfigArg]>,
}

impl BuckConfigBasedCells {
    pub(crate) fn parse_cell_resolver(
        project_fs: &ProjectRoot,
        file_ops: &mut dyn ConfigParserFileOps,
    ) -> anyhow::Result<CellResolver> {
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

        Ok(cells.cell_resolver)
    }

    pub fn parse(project_fs: &ProjectRoot) -> anyhow::Result<Self> {
        Self::parse_with_file_ops(
            project_fs,
            &mut DefaultConfigParserFileOps {
                project_fs: project_fs.dupe(),
            },
            &[],
            ProjectRelativePath::empty(),
        )
    }

    pub fn parse_with_config_args(
        project_fs: &ProjectRoot,
        config_args: &[buck2_cli_proto::ConfigOverride],
        cwd: &ProjectRelativePath,
    ) -> anyhow::Result<Self> {
        Self::parse_with_file_ops(
            project_fs,
            &mut DefaultConfigParserFileOps {
                project_fs: project_fs.dupe(),
            },
            config_args,
            cwd,
        )
    }

    pub fn parse_with_file_ops(
        project_fs: &ProjectRoot,
        file_ops: &mut dyn ConfigParserFileOps,
        config_args: &[buck2_cli_proto::ConfigOverride],
        cwd: &ProjectRelativePath,
    ) -> anyhow::Result<Self> {
        let opts = BuckConfigParseOptions {
            follow_includes: true,
        };
        Self::parse_with_file_ops_and_options(project_fs, file_ops, config_args, cwd, opts)
    }

    pub fn parse_no_follow_includes(project_fs: &ProjectRoot) -> anyhow::Result<Self> {
        let opts = BuckConfigParseOptions {
            follow_includes: false,
        };
        Self::parse_with_file_ops_and_options(
            project_fs,
            &mut DefaultConfigParserFileOps {
                project_fs: project_fs.dupe(),
            },
            &[],
            ProjectRelativePath::empty(),
            opts,
        )
    }

    fn parse_with_file_ops_and_options(
        project_root: &ProjectRoot,
        file_ops: &mut dyn ConfigParserFileOps,
        config_args: &[buck2_cli_proto::ConfigOverride],
        cwd: &ProjectRelativePath,
        options: BuckConfigParseOptions,
    ) -> anyhow::Result<Self> {
        Self::parse_with_file_ops_and_options_inner(
            project_root,
            file_ops,
            config_args,
            cwd,
            options,
        )
        .with_context(|| format!("Parsing cells with project root `{project_root}`, cwd `{cwd}`",))
    }

    fn parse_with_file_ops_and_options_inner(
        project_fs: &ProjectRoot,
        file_ops: &mut dyn ConfigParserFileOps,
        config_args: &[buck2_cli_proto::ConfigOverride],
        cwd: &ProjectRelativePath,
        options: BuckConfigParseOptions,
    ) -> anyhow::Result<Self> {
        // Tracing file ops to record config file accesses on command invocation.
        struct TracingFileOps<'a> {
            inner: &'a mut dyn ConfigParserFileOps,
            trace: HashSet<ConfigPath>,
        }

        #[async_trait::async_trait]
        impl ConfigParserFileOps for TracingFileOps<'_> {
            async fn read_file_lines(
                &mut self,
                path: &ConfigPath,
            ) -> anyhow::Result<Box<dyn Iterator<Item = Result<String, std::io::Error>> + Send>>
            {
                self.trace.insert(path.clone());
                self.inner.read_file_lines(path).await
            }

            async fn file_exists(&mut self, path: &ConfigPath) -> anyhow::Result<bool> {
                self.inner.file_exists(path).await
            }

            async fn read_dir(&mut self, path: &ConfigPath) -> anyhow::Result<Vec<ConfigDirEntry>> {
                self.inner.read_dir(path).await
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

        // NOTE: This will _not_ perform IO unless it needs to.
        let processed_config_args =
            resolve_config_args(&config_args, project_fs, cwd, &mut file_ops)?;

        let external_paths =
            futures::executor::block_on(get_external_buckconfig_paths(&mut file_ops))?;
        let started_parse =
            futures::executor::block_on(LegacyBuckConfig::start_parse_for_external_files(
                &external_paths,
                &mut file_ops,
                options.follow_includes,
            ))?;

        while let Some(path) = work.pop() {
            if buckconfigs.contains_key(&path) || cells_aggregator.is_external(&path) {
                continue;
            }

            // Blocking is ok because we know the fileops don't suspend
            let buckconfig_paths =
                futures::executor::block_on(get_project_buckconfig_paths(&path, &mut file_ops))?;

            let config = futures::executor::block_on(LegacyBuckConfig::finish_parse(
                started_parse.clone(),
                buckconfig_paths.as_slice(),
                &path,
                &mut file_ops,
                &processed_config_args,
                options.follow_includes,
            ))?;

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
                if cells_aggregator.get_name(&path).is_none() {
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

            for (alias, destination) in Self::get_cell_aliases_from_config(&config)? {
                let alias_path =
                    cells_aggregator.add_cell_alias(path.clone(), alias.clone(), destination)?;
                if is_root {
                    root_aliases.insert(alias, alias_path.clone());
                }
            }

            if is_root {
                if let Some(external_cells) = config.get_section("external_cells") {
                    for (alias, origin) in external_cells.iter() {
                        let alias = NonEmptyCellAlias::new(alias.to_owned())?;
                        let target = root_aliases
                            .get(&alias)
                            .ok_or(CellsError::UnknownCellName(alias))?;
                        let name = cells_aggregator
                            .get_name(target)
                            .internal_error("We just checked that this cell exists")?;
                        let origin =
                            Self::parse_external_cell_origin(name, origin.as_str(), &config)?;
                        if let ExternalCellOrigin::Bundled(name) = origin {
                            EXTERNAL_CELLS_IMPL.get()?.check_bundled_cell_exists(name)?;
                        }
                        cells_aggregator.mark_external_cell(target.to_owned(), origin)?;
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

    pub(crate) fn get_cell_aliases_from_config(
        config: &LegacyBuckConfig,
    ) -> anyhow::Result<impl Iterator<Item = (NonEmptyCellAlias, NonEmptyCellAlias)>> {
        let mut aliases = Vec::new();
        if let Some(section) = config
            .get_section("repository_aliases")
            .or_else(|| config.get_section("cell_aliases"))
        {
            for (alias, destination) in section.iter() {
                let alias = NonEmptyCellAlias::new(alias.to_owned())?;
                let destination = NonEmptyCellAlias::new(destination.as_str().to_owned())?;
                aliases.push((alias, destination));
            }
        }
        Ok(aliases.into_iter())
    }

    pub(crate) async fn parse_single_cell_with_dice(
        ctx: &mut DiceComputations<'_>,
        cell_path: &CellRootPath,
    ) -> anyhow::Result<LegacyBuckConfig> {
        let resolver = ctx.get_cell_resolver().await?;
        let io_provider = ctx.global_data().get_io_provider();
        let project_fs = io_provider.project_root();
        let overrides = ctx.get_injected_legacy_config_overrides().await?;

        let mut file_ops = DiceConfigFileOps::new(ctx, project_fs, &resolver);

        let config_paths = get_external_buckconfig_paths(&mut file_ops).await?;
        let started_parse = LegacyBuckConfig::start_parse_for_external_files(
            &config_paths,
            &mut file_ops,
            /* follow_includes */ true,
        )
        .await?;
        let config_paths = get_project_buckconfig_paths(cell_path, &mut file_ops).await?;
        LegacyBuckConfig::finish_parse(
            started_parse,
            &config_paths,
            cell_path,
            &mut file_ops,
            overrides.as_ref(),
            /* follow includes */ true,
        )
        .await
    }

    fn parse_external_cell_origin(
        cell: CellName,
        value: &str,
        config: &LegacyBuckConfig,
    ) -> anyhow::Result<ExternalCellOrigin> {
        #[derive(buck2_error::Error, Debug)]
        enum ExternalCellOriginParseError {
            #[error("Unknown external cell origin `{0}`")]
            Unknown(String),
            #[error("Missing buckconfig `{0}.{1}` for external cell configuration")]
            MissingConfiguration(String, String),
        }

        let get_config = |section: &str, property: &str| {
            config
                .get(crate::legacy_configs::key::BuckconfigKeyRef { section, property })
                .ok_or_else(|| {
                    ExternalCellOriginParseError::MissingConfiguration(
                        section.to_owned(),
                        property.to_owned(),
                    )
                })
        };

        if value == "bundled" {
            Ok(ExternalCellOrigin::Bundled(cell))
        } else if value == "git" {
            let section = &format!("external_cell_{}", cell.as_str());
            let commit: Arc<str> = get_config(section, "commit_hash")?.into();
            // No use in storing the commit hash as a byte array, but let's reuse existing code to
            // check for validity
            let _ = RawDigest::parse_sha1(commit.as_bytes())?;
            Ok(ExternalCellOrigin::Git(GitCellSetup {
                git_origin: get_config(section, "git_origin")?.into(),
                commit,
            }))
        } else {
            Err(ExternalCellOriginParseError::Unknown(value.to_owned()).into())
        }
    }
}

async fn get_external_buckconfig_paths(
    file_ops: &mut dyn ConfigParserFileOps,
) -> anyhow::Result<Vec<ConfigPath>> {
    let skip_default_external_config = buck2_env!(
        "BUCK2_TEST_SKIP_DEFAULT_EXTERNAL_CONFIG",
        bool,
        applicability = testing
    )?;

    let mut buckconfig_paths: Vec<ConfigPath> = Vec::new();

    if !skip_default_external_config {
        for buckconfig in DEFAULT_EXTERNAL_CONFIG_SOURCES {
            match buckconfig {
                ExternalConfigSource::UserFile(file) => {
                    let home_dir = dirs::home_dir();
                    if let Some(home_dir_path) = home_dir {
                        let buckconfig_path = ForwardRelativePath::new(file)?;
                        buckconfig_paths.push(ConfigPath::Global(
                            AbsPath::new(&home_dir_path)?.join(buckconfig_path.as_str()),
                        ));
                    }
                }
                ExternalConfigSource::UserFolder(folder) => {
                    let home_dir = dirs::home_dir();
                    if let Some(home_dir_path) = home_dir {
                        let buckconfig_path = ForwardRelativePath::new(folder)?;
                        let buckconfig_folder_abs_path =
                            AbsPath::new(&home_dir_path)?.join(buckconfig_path.as_str());
                        push_all_files_from_a_directory(
                            &mut buckconfig_paths,
                            &ConfigPath::Global(buckconfig_folder_abs_path),
                            file_ops,
                        )
                        .await?;
                    }
                }
                ExternalConfigSource::GlobalFile(file) => {
                    buckconfig_paths.push(ConfigPath::Global(AbsPath::new(*file)?.to_owned()));
                }
                ExternalConfigSource::GlobalFolder(folder) => {
                    let buckconfig_folder_abs_path = AbsPath::new(*folder)?.to_owned();
                    push_all_files_from_a_directory(
                        &mut buckconfig_paths,
                        &ConfigPath::Global(buckconfig_folder_abs_path),
                        file_ops,
                    )
                    .await?;
                }
            }
        }
    }

    let extra_external_config =
        buck2_env!("BUCK2_TEST_EXTRA_EXTERNAL_CONFIG", applicability = testing)?;

    if let Some(f) = extra_external_config {
        buckconfig_paths.push(ConfigPath::Global(AbsPath::new(f)?.to_owned()));
    }

    Ok(buckconfig_paths)
}

async fn get_project_buckconfig_paths(
    path: &CellRootPath,
    file_ops: &mut dyn ConfigParserFileOps,
) -> anyhow::Result<Vec<ConfigPath>> {
    let mut buckconfig_paths: Vec<ConfigPath> = Vec::new();

    for buckconfig in DEFAULT_PROJECT_CONFIG_SOURCES {
        match buckconfig {
            ProjectConfigSource::CellRelativeFile(file) => {
                let buckconfig_path = ForwardRelativePath::new(file)?;
                buckconfig_paths.push(ConfigPath::Project(
                    path.as_project_relative_path().join(buckconfig_path),
                ));
            }
            ProjectConfigSource::CellRelativeFolder(folder) => {
                let buckconfig_folder_path = ForwardRelativePath::new(folder)?;
                let buckconfig_folder_path =
                    path.as_project_relative_path().join(buckconfig_folder_path);
                push_all_files_from_a_directory(
                    &mut buckconfig_paths,
                    &ConfigPath::Project(buckconfig_folder_path),
                    file_ops,
                )
                .await?;
            }
        }
    }

    Ok(buckconfig_paths)
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
    use std::sync::Arc;

    use buck2_cli_proto::config_override::ConfigType;
    use buck2_cli_proto::ConfigOverride;
    use buck2_core::cells::cell_root_path::CellRootPath;
    use buck2_core::cells::external::ExternalCellOrigin;
    use buck2_core::cells::external::GitCellSetup;
    use buck2_core::cells::name::CellName;
    use buck2_core::fs::project_rel_path::ProjectRelativePath;
    use dice::DiceComputations;
    use indoc::indoc;

    use crate::dice::file_ops::delegate::FileOpsDelegate;
    use crate::external_cells::ExternalCellsImpl;
    use crate::external_cells::EXTERNAL_CELLS_IMPL;
    use crate::legacy_configs::cells::create_project_filesystem;
    use crate::legacy_configs::cells::BuckConfigBasedCells;
    use crate::legacy_configs::configs::testing::TestConfigParserFileOps;
    use crate::legacy_configs::configs::tests::assert_config_value;
    use crate::legacy_configs::key::BuckconfigKeyRef;

    #[test]
    fn test_cells() -> anyhow::Result<()> {
        let mut file_ops = TestConfigParserFileOps::new(&[
            (
                ".buckconfig",
                indoc!(
                    r#"
                            [cells]
                                root = .
                                other = other/
                                other_alias = other/
                                third_party = third_party/
                        "#
                ),
            ),
            (
                "other/.buckconfig",
                indoc!(
                    r#"
                            [cells]
                                root = ..
                                other = .
                                third_party = ../third_party/
                        "#
                ),
            ),
            (
                "third_party/.buckconfig",
                indoc!(
                    r#"
                            [cells]
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
                ".buckconfig",
                indoc!(
                    r#"
                            [cells]
                                root = .
                                other = other/
                                other_alias = other/
                                third_party = third_party/
                        "#
                ),
            ),
            (
                "other/.buckconfig",
                indoc!(
                    r#"
                            [cells]
                                root = ..
                                other = .
                                third_party = ../third_party/
                            [buildfile]
                                name = TARGETS
                        "#
                ),
            ),
            (
                "third_party/.buckconfig",
                indoc!(
                    r#"
                            [cells]
                                third_party = .
                            [buildfile]
                                name_v2 = OKAY
                                name = OKAY_v1
                        "#
                ),
            ),
            (
                "other/cli-conf",
                indoc!(
                    r#"
                            [foo]
                                bar = blah
                        "#
                ),
            ),
        ])?;

        let project_fs = create_project_filesystem();
        let cells = BuckConfigBasedCells::parse_with_file_ops(
            &project_fs,
            &mut file_ops,
            &[ConfigOverride {
                config_override: "other//cli-conf".to_owned(),
                config_type: ConfigType::File.into(),
            }],
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
                ".buckconfig",
                indoc!(
                    r#"
                            [cells]
                                root = .
                                other = other/
                        "#
                ),
            ),
            (
                "other/.buckconfig",
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
                ".buckconfig",
                indoc!(
                    r#"
                            [cells]
                                root = .
                                other = other/
                        "#
                ),
            ),
            (
                "global-conf",
                indoc!(
                    r#"
                            [apple]
                                test_tool = xctool
                        "#
                ),
            ),
            (
                "other/.buckconfig",
                indoc!(
                    r#"
                            [cells]
                                root = ..
                                other = .
                            [buildfile]
                                name = TARGETS
                        "#
                ),
            ),
            (
                "other/app-conf",
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
                ConfigOverride {
                    config_override: "other//app-conf".to_owned(),
                    config_type: ConfigType::File.into(),
                },
                ConfigOverride {
                    config_override: "//global-conf".to_owned(),
                    config_type: ConfigType::File.into(),
                },
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
                ".buckconfig",
                indoc!(
                    r#"
                            [cells]
                                root = .
                            [apple]
                                key = value1
                                key2 = value2
                        "#
                ),
            ),
            (
                ".buckconfig.local",
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
                ".buckconfig",
                indoc!(
                    r#"
                            [cells]
                                root = .
                                other = other/
                            [apple]
                                key = value1
                                key2 = value2
                        "#
                ),
            ),
            (
                ".buckconfig.local",
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
                "other/.buckconfig",
                indoc!(
                    r#"
                            [cells]
                                root = ..
                                other = .
                            [apple]
                                key = othervalue1
                                key2 = othervalue2
                        "#
                ),
            ),
            (
                "other/.buckconfig.local",
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
            ".buckconfig",
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
            &[ConfigOverride {
                config_override: "some_section.key=value1".to_owned(),
                config_type: ConfigType::Value.into(),
            }],
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
            ".buckconfig",
            indoc!(
                r#"
                            [repositories]
                                root = .
                                other = other/
                            [repository_aliases]
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

    fn initialize_external_cells_impl() {
        struct TestExternalCellsImpl;

        #[async_trait::async_trait]
        impl ExternalCellsImpl for TestExternalCellsImpl {
            async fn get_file_ops_delegate(
                &self,
                _ctx: &mut DiceComputations<'_>,
                _cell_name: CellName,
                _origin: ExternalCellOrigin,
            ) -> anyhow::Result<Arc<dyn FileOpsDelegate>> {
                // Not used in these tests
                unreachable!()
            }

            fn check_bundled_cell_exists(&self, cell_name: CellName) -> anyhow::Result<()> {
                if cell_name.as_str() == "test_bundled_cell" {
                    Ok(())
                } else {
                    Err(anyhow::anyhow!("No bundled cell with name `{}`", cell_name))
                }
            }

            async fn expand(
                &self,
                _ctx: &mut DiceComputations<'_>,
                _cell_name: CellName,
                _origin: ExternalCellOrigin,
                _path: &CellRootPath,
            ) -> anyhow::Result<()> {
                // Not used in these tests
                unreachable!()
            }
        }

        static INIT: std::sync::Once = std::sync::Once::new();

        // Sometimes multiple unittests are run in the same process
        INIT.call_once(|| {
            EXTERNAL_CELLS_IMPL.init(&TestExternalCellsImpl);
        });
    }

    #[test]
    fn test_external_cell_configs() -> anyhow::Result<()> {
        initialize_external_cells_impl();

        let mut file_ops = TestConfigParserFileOps::new(&[(
            ".buckconfig",
            indoc!(
                r#"
                    [cells]
                        root = .
                        test_bundled_cell = other1/
                        other2 = other2/
                    [cell_aliases]
                        other_alias = test_bundled_cell
                    [external_cells]
                        other_alias = bundled
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
        let other1 = root
            .testing_cell_alias_resolver()
            .resolve("other_alias")
            .unwrap();
        let other2 = root
            .testing_cell_alias_resolver()
            .resolve("other2")
            .unwrap();

        assert_eq!(
            resolver.get(other1).unwrap().external(),
            Some(&ExternalCellOrigin::Bundled(CellName::testing_new(
                "test_bundled_cell"
            ))),
        );
        assert_eq!(resolver.get(other2).unwrap().external(), None,);
        assert_eq!(
            root.testing_cell_alias_resolver()
                .resolve("other_alias")
                .unwrap()
                .as_str(),
            "test_bundled_cell",
        );

        Ok(())
    }

    #[test]
    fn test_nested_external_cell_configs() -> anyhow::Result<()> {
        initialize_external_cells_impl();

        let mut file_ops = TestConfigParserFileOps::new(&[(
            ".buckconfig",
            indoc!(
                r#"
                    [cells]
                        root = .
                        test_bundled_cell = foo/
                        bar = foo/bar/
                    [external_cells]
                        test_bundled_cell = bundled
                "#
            ),
        )])?;

        let project_fs = create_project_filesystem();
        BuckConfigBasedCells::parse_with_file_ops(
            &project_fs,
            &mut file_ops,
            &[],
            ProjectRelativePath::empty(),
        )
        .err()
        .unwrap();

        Ok(())
    }

    #[test]
    fn test_missing_bundled_cell() -> anyhow::Result<()> {
        initialize_external_cells_impl();

        let mut file_ops = TestConfigParserFileOps::new(&[(
            ".buckconfig",
            indoc!(
                r#"
                    [cells]
                        root = .
                        foo = foo/
                        bar = foo/bar/
                    [external_cells]
                        foo = bundled
                "#
            ),
        )])?;

        let project_fs = create_project_filesystem();
        let e = BuckConfigBasedCells::parse_with_file_ops(
            &project_fs,
            &mut file_ops,
            &[],
            ProjectRelativePath::empty(),
        )
        .err()
        .unwrap();

        let e = format!("{:?}", e);
        assert!(e.contains("No bundled cell"), "error: {}", e);

        Ok(())
    }

    #[test]
    fn test_git_external_cell() -> anyhow::Result<()> {
        initialize_external_cells_impl();

        let mut file_ops = TestConfigParserFileOps::new(&[(
            ".buckconfig",
            indoc!(
                r#"
                    [cells]
                        root = .
                        libfoo = foo/
                    [external_cells]
                        libfoo = git
                    [external_cell_libfoo]
                        git_origin = https://github.com/jeff/libfoo.git
                        commit_hash = aaaaaaaabbbbbbbbccccccccddddddddeeeeeeee
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

        let instance = resolver.get(CellName::testing_new("libfoo")).unwrap();

        assert_eq!(
            instance.external(),
            Some(&ExternalCellOrigin::Git(GitCellSetup {
                git_origin: "https://github.com/jeff/libfoo.git".into(),
                commit: "aaaaaaaabbbbbbbbccccccccddddddddeeeeeeee".into(),
            })),
        );

        Ok(())
    }

    #[test]
    fn test_git_external_cell_invalid_sha1() -> anyhow::Result<()> {
        initialize_external_cells_impl();

        let mut file_ops = TestConfigParserFileOps::new(&[(
            ".buckconfig",
            indoc!(
                r#"
                    [cells]
                        root = .
                        libfoo = foo/
                    [external_cells]
                        libfoo = git
                    [external_cell_libfoo]
                        git_origin = https://github.com/jeff/libfoo.git
                        commit_hash = abcde
                "#
            ),
        )])?;

        let project_fs = create_project_filesystem();
        let e = BuckConfigBasedCells::parse_with_file_ops(
            &project_fs,
            &mut file_ops,
            &[],
            ProjectRelativePath::empty(),
        )
        .err()
        .unwrap();

        let e = format!("{:?}", e);
        assert!(e.contains("not a valid SHA1 digest"), "error: {}", e);

        Ok(())
    }
}
