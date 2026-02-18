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
use std::str::FromStr;
use std::sync::Arc;

use allocative::Allocative;
use buck2_core::buck2_env;
use buck2_core::cells::CellAliasResolver;
use buck2_core::cells::CellResolver;
use buck2_core::cells::alias::NonEmptyCellAlias;
use buck2_core::cells::cell_root_path::CellRootPath;
use buck2_core::cells::cell_root_path::CellRootPathBuf;
use buck2_core::cells::external::ExternalCellOrigin;
use buck2_core::cells::external::GitCellSetup;
use buck2_core::cells::external::GitObjectFormat;
use buck2_core::cells::name::CellName;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_error::BuckErrorContext;
use buck2_fs::paths::RelativePath;
use buck2_fs::paths::abs_path::AbsPath;
use buck2_fs::paths::forward_rel_path::ForwardRelativePath;
use dice::DiceComputations;
use dupe::Dupe;

use crate::dice::cells::HasCellResolver;
use crate::dice::data::HasIoProvider;
use crate::external_cells::EXTERNAL_CELLS_IMPL;
use crate::legacy_configs::aggregator::CellsAggregator;
use crate::legacy_configs::args::ResolvedLegacyConfigArg;
use crate::legacy_configs::args::resolve_config_args;
use crate::legacy_configs::args::to_proto_config_args;
use crate::legacy_configs::configs::LegacyBuckConfig;
use crate::legacy_configs::dice::HasInjectedLegacyConfigs;
use crate::legacy_configs::file_ops::ConfigDirEntry;
use crate::legacy_configs::file_ops::ConfigParserFileOps;
use crate::legacy_configs::file_ops::ConfigPath;
use crate::legacy_configs::file_ops::DefaultConfigParserFileOps;
use crate::legacy_configs::file_ops::DiceConfigFileOps;
use crate::legacy_configs::file_ops::push_all_files_from_a_directory;
use crate::legacy_configs::key::BuckconfigKeyRef;
use crate::legacy_configs::parser::LegacyConfigParser;
use crate::legacy_configs::path::DEFAULT_EXTERNAL_CONFIG_SOURCES;
use crate::legacy_configs::path::DEFAULT_PROJECT_CONFIG_SOURCES;
use crate::legacy_configs::path::DOT_BUCKCONFIG_LOCAL;
use crate::legacy_configs::path::ExternalConfigSource;
use crate::legacy_configs::path::ProjectConfigSource;

/// Buckconfigs can partially be loaded from within dice. However, some parts of what makes up the
/// buckconfig comes from outside the buildgraph, and this type represents those parts.
#[derive(Clone, PartialEq, Eq, Allocative)]
pub struct ExternalBuckconfigData {
    // The result of parsing the buckconfigs coming from either global (e.g. /etc/buckconfig.d) or
    // user (e.g. ~/.buckconfig.d or $home_dir/.buckconfig.local) files/dirs outside of the repo
    // The order matters here and reflects the same order these are processed in buck, see
    // https://fburl.com/code/8ue78p1j
    external_path_configs: Vec<ExternalPathBuckconfigData>,
    // The result of parsing the buckconfigs coming from command line args (e.g. --config or --config-file)
    args: Vec<ResolvedLegacyConfigArg>,
}

#[derive(PartialEq, Eq, Allocative, Clone)]
pub struct ExternalPathBuckconfigData {
    pub(crate) parse_state: LegacyConfigParser,
    pub(crate) origin_path: ConfigPath,
}

impl ExternalBuckconfigData {
    pub fn testing_default() -> Self {
        Self {
            external_path_configs: Vec::new(),
            args: Vec::new(),
        }
    }

    pub fn filter_values<F>(self, filter: F) -> Self
    where
        F: Fn(&BuckconfigKeyRef) -> bool,
    {
        Self {
            external_path_configs: self
                .external_path_configs
                .into_iter()
                .map(|o| ExternalPathBuckconfigData {
                    parse_state: o.parse_state.filter_values(&filter),
                    origin_path: o.origin_path,
                })
                .collect(),
            args: self
                .args
                .into_iter()
                .filter(|arg| match arg {
                    ResolvedLegacyConfigArg::Flag(flag) => {
                        flag.cell.is_some()
                            || filter(&BuckconfigKeyRef {
                                section: &flag.section,
                                property: &flag.key,
                            })
                    }
                    _ => true,
                })
                .collect(),
        }
    }

    async fn get_local_config_components(
        project_root: &ProjectRoot,
    ) -> Vec<buck2_data::BuckconfigComponent> {
        use buck2_data::buckconfig_component::Data::GlobalExternalConfigFile;
        let file_ops = &mut DefaultConfigParserFileOps {
            project_fs: project_root.dupe(),
        };
        let mut local_config_components = Vec::new();
        if let Ok(legacy_cells) =
            BuckConfigBasedCells::parse_with_config_args(project_root, &[]).await
        {
            let path = ForwardRelativePath::new(DOT_BUCKCONFIG_LOCAL).expect(
                "Internal error: .buckconfig.local should always be a valid forward relative path",
            );
            for (_cell, cell_instance) in legacy_cells.cell_resolver.cells() {
                let relative_path = cell_instance.path().as_project_relative_path().join(path);
                let origin_path = relative_path.to_string();
                let local_config = ConfigPath::Project(relative_path);

                let mut parser = LegacyConfigParser::new();
                if parser
                    .parse_file(&local_config, None, true, file_ops)
                    .await
                    .is_ok()
                {
                    let values = parser.to_proto_external_config_values(false);
                    if values.is_empty() {
                        // Don't create an empty component for cells with non-existing .buckconfig.local
                        continue;
                    }
                    local_config_components.push(buck2_data::BuckconfigComponent {
                        data: Some(GlobalExternalConfigFile(buck2_data::GlobalExternalConfig {
                            values,
                            origin_path,
                        })),
                    });
                }
            }
        }
        local_config_components
    }

    pub async fn get_buckconfig_components(
        &self,
        project_root: &ProjectRoot,
    ) -> Vec<buck2_data::BuckconfigComponent> {
        use buck2_data::buckconfig_component::Data::GlobalExternalConfigFile;
        let mut res: Vec<buck2_data::BuckconfigComponent> = self
            .external_path_configs
            .clone()
            .into_iter()
            .map(|o| {
                let external_file = buck2_data::GlobalExternalConfig {
                    values: o.parse_state.to_proto_external_config_values(false),
                    origin_path: o.origin_path.to_string(),
                };
                buck2_data::BuckconfigComponent {
                    data: Some(GlobalExternalConfigFile(external_file)),
                }
            })
            .collect();

        res.extend(Self::get_local_config_components(project_root).await);
        res.extend(to_proto_config_args(&self.args));
        res
    }
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
pub struct BuckConfigBasedCells {
    pub cell_resolver: CellResolver,
    pub root_config: LegacyBuckConfig,
    pub config_paths: HashSet<ConfigPath>,
    pub external_data: ExternalBuckconfigData,
}

impl BuckConfigBasedCells {
    /// In the client and one place in the daemon, we need access to the alias resolver for the cwd
    /// in some places where we don't have normal dice access
    ///
    /// This function reads buckconfigs to compute an appropriate cell alias resolver to make that
    /// possible.
    pub async fn get_cell_alias_resolver_for_cwd_fast(
        &self,
        project_fs: &ProjectRoot,
        cwd: &ProjectRelativePath,
    ) -> buck2_error::Result<CellAliasResolver> {
        self.get_cell_alias_resolver_for_cwd_fast_with_file_ops(
            &mut DefaultConfigParserFileOps {
                project_fs: project_fs.dupe(),
            },
            cwd,
        )
        .await
    }

    pub(crate) async fn get_cell_alias_resolver_for_cwd_fast_with_file_ops(
        &self,
        file_ops: &mut dyn ConfigParserFileOps,
        cwd: &ProjectRelativePath,
    ) -> buck2_error::Result<CellAliasResolver> {
        let cell_name = self.cell_resolver.find(cwd);
        let cell_path = self.cell_resolver.get(cell_name)?.path();

        let follow_includes = false;

        let config_paths = get_project_buckconfig_paths(cell_path, file_ops).await?;
        let config = LegacyBuckConfig::finish_parse(
            self.external_data.external_path_configs.clone(),
            &config_paths,
            cell_path,
            file_ops,
            &[],
            follow_includes,
        )
        .await?;

        CellAliasResolver::new_for_non_root_cell(
            cell_name,
            self.cell_resolver.root_cell_cell_alias_resolver(),
            BuckConfigBasedCells::get_cell_aliases_from_config(&config)?,
        )
    }

    pub async fn parse_with_config_args(
        project_fs: &ProjectRoot,
        config_args: &[buck2_cli_proto::ConfigOverride],
    ) -> buck2_error::Result<Self> {
        Self::parse_with_file_ops_and_options(
            &mut DefaultConfigParserFileOps {
                project_fs: project_fs.dupe(),
            },
            config_args,
            false, /* follow includes */
        )
        .await
    }

    pub async fn testing_parse_with_file_ops(
        file_ops: &mut dyn ConfigParserFileOps,
        config_args: &[buck2_cli_proto::ConfigOverride],
    ) -> buck2_error::Result<Self> {
        Self::parse_with_file_ops_and_options(
            file_ops,
            config_args,
            true, /* follow includes */
        )
        .await
    }

    async fn parse_with_file_ops_and_options(
        file_ops: &mut dyn ConfigParserFileOps,
        config_args: &[buck2_cli_proto::ConfigOverride],
        follow_includes: bool,
    ) -> buck2_error::Result<Self> {
        Self::parse_with_file_ops_and_options_inner(file_ops, config_args, follow_includes)
            .await
            .buck_error_context("Parsing cells")
    }

    async fn parse_with_file_ops_and_options_inner(
        file_ops: &mut dyn ConfigParserFileOps,
        config_args: &[buck2_cli_proto::ConfigOverride],
        follow_includes: bool,
    ) -> buck2_error::Result<Self> {
        // Tracing file ops to record config file accesses on command invocation.
        struct TracingFileOps<'a> {
            inner: &'a mut dyn ConfigParserFileOps,
            trace: HashSet<ConfigPath>,
        }

        #[async_trait::async_trait]
        impl ConfigParserFileOps for TracingFileOps<'_> {
            async fn read_file_lines_if_exists(
                &mut self,
                path: &ConfigPath,
            ) -> buck2_error::Result<Option<Vec<String>>> {
                let res = self.inner.read_file_lines_if_exists(path).await?;

                if res.is_some() {
                    self.trace.insert(path.clone());
                }

                Ok(res)
            }

            async fn read_dir(
                &mut self,
                path: &ConfigPath,
            ) -> buck2_error::Result<Vec<ConfigDirEntry>> {
                self.inner.read_dir(path).await
            }
        }

        let mut file_ops = TracingFileOps {
            inner: file_ops,
            trace: Default::default(),
        };

        // NOTE: This will _not_ perform IO unless it needs to.
        let processed_config_args = resolve_config_args(config_args, &mut file_ops).await?;

        let external_paths = get_external_buckconfig_paths(&mut file_ops).await?;
        let started_parse = LegacyBuckConfig::start_parse_for_external_files(
            &external_paths,
            &mut file_ops,
            follow_includes,
        )
        .await?;

        let root_path = CellRootPathBuf::new(ProjectRelativePath::empty().to_owned());

        let buckconfig_paths = get_project_buckconfig_paths(&root_path, &mut file_ops).await?;

        let root_config = LegacyBuckConfig::finish_parse(
            started_parse.clone(),
            buckconfig_paths.as_slice(),
            &root_path,
            &mut file_ops,
            &processed_config_args,
            follow_includes,
        )
        .await?;

        let mut cell_definitions = Vec::new();

        // `cells` is preferred over `repositories` since it's more clear, however it's unlikely
        // that we'll ever remove `repositories` since that's probably unnecessary breakage in OSS.
        //
        // Note that `cells` is buck2-only
        let repositories = root_config
            .get_section("cells")
            .or_else(|| root_config.get_section("repositories"));
        if let Some(repositories) = repositories {
            for (alias, alias_path) in repositories.iter() {
                let alias_path = CellRootPathBuf::new(
                    root_path.as_project_relative_path()
                        .join_normalized(RelativePath::new(alias_path.as_str()))
                        .with_buck_error_context(|| {
                            format!(
                                "expected alias path to be a relative path, but found `{}` for `{}`",
                                alias_path.as_str(),
                                alias,
                            )
                        })?
                );
                let name = CellName::unchecked_new(alias)?;
                cell_definitions.push((name, alias_path));
            }
        }

        let root_aliases = Self::get_cell_aliases_from_config(&root_config)?.collect();

        let mut aggregator = CellsAggregator::new(cell_definitions, root_aliases)?;

        if let Some(external_cells) = root_config.get_section("external_cells") {
            for (alias, origin) in external_cells.iter() {
                if origin.as_str() == "disabled" {
                    // Ignore this entry, treat it as a normal cell
                    continue;
                }
                let alias = NonEmptyCellAlias::new(alias.to_owned())?;
                let name = aggregator.resolve_root_alias(alias)?;
                let origin = Self::parse_external_cell_origin(name, origin.as_str(), &root_config)?;
                if let ExternalCellOrigin::Bundled(name) = origin {
                    // This code is executed both in the client and in the daemon. When in the
                    // client and using a client-only build, this late binding might not be bound,
                    // and so we can't check this. That doesn't matter though, as we'll get an error
                    // when this fails in the daemon anyway
                    if let Ok(imp) = EXTERNAL_CELLS_IMPL.get() {
                        imp.check_bundled_cell_exists(name)?;
                    }
                }
                aggregator.mark_external_cell(name, origin)?;
            }
        }

        let cell_resolver = aggregator.make_cell_resolver()?;

        Ok(Self {
            cell_resolver,
            root_config,
            config_paths: file_ops.trace,
            external_data: ExternalBuckconfigData {
                external_path_configs: started_parse,
                args: processed_config_args,
            },
        })
    }

    pub(crate) fn get_cell_aliases_from_config(
        config: &LegacyBuckConfig,
    ) -> buck2_error::Result<impl Iterator<Item = (NonEmptyCellAlias, NonEmptyCellAlias)> + use<>>
    {
        let mut aliases = Vec::new();
        if let Some(section) = config
            .get_section("cell_aliases")
            .or_else(|| config.get_section("repository_aliases"))
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
    ) -> buck2_error::Result<LegacyBuckConfig> {
        let resolver = ctx.get_cell_resolver().await?;
        let io_provider = ctx.global_data().get_io_provider();
        let project_fs = io_provider.project_root();
        let external_data = ctx.get_injected_external_buckconfig_data().await?;

        let mut file_ops = DiceConfigFileOps::new(ctx, project_fs, &resolver);

        Self::parse_single_cell_with_file_ops_inner(&external_data, &mut file_ops, cell_path).await
    }

    pub async fn parse_single_cell(
        &self,
        cell: CellName,
        project_fs: &ProjectRoot,
    ) -> buck2_error::Result<LegacyBuckConfig> {
        self.parse_single_cell_with_file_ops(
            cell,
            &mut DefaultConfigParserFileOps {
                project_fs: project_fs.dupe(),
            },
        )
        .await
    }

    pub(crate) async fn parse_single_cell_with_file_ops(
        &self,
        cell: CellName,
        file_ops: &mut dyn ConfigParserFileOps,
    ) -> buck2_error::Result<LegacyBuckConfig> {
        Self::parse_single_cell_with_file_ops_inner(
            &self.external_data,
            file_ops,
            self.cell_resolver.get(cell)?.path(),
        )
        .await
    }

    async fn parse_single_cell_with_file_ops_inner(
        external_data: &ExternalBuckconfigData,
        file_ops: &mut dyn ConfigParserFileOps,
        cell_path: &CellRootPath,
    ) -> buck2_error::Result<LegacyBuckConfig> {
        let config_paths = get_project_buckconfig_paths(cell_path, file_ops).await?;
        LegacyBuckConfig::finish_parse(
            external_data.external_path_configs.clone(),
            &config_paths,
            cell_path,
            file_ops,
            external_data.args.as_ref(),
            /* follow includes */ true,
        )
        .await
    }

    fn parse_external_cell_origin(
        cell: CellName,
        value: &str,
        config: &LegacyBuckConfig,
    ) -> buck2_error::Result<ExternalCellOrigin> {
        #[derive(buck2_error::Error, Debug)]
        #[buck2(tag = Input)]
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
            let commit = get_config(section, "commit_hash")?;
            let object_format = match get_config(section, "object_format") {
                Ok(s) => {
                    let object_format = GitObjectFormat::from_str(s)?;
                    object_format.check(commit)?;
                    Option::Some(GitObjectFormat::from_str(s)?)
                }
                Err(_) => {
                    // We pretend that the object format is SHA1 for this check only;
                    // We do not use it when interacting with Git.
                    GitObjectFormat::Sha1.check(commit)?;
                    Option::None
                }
            };
            Ok(ExternalCellOrigin::Git(GitCellSetup {
                git_origin: get_config(section, "git_origin")?.into(),
                commit: Arc::from(commit),
                object_format,
            }))
        } else {
            Err(ExternalCellOriginParseError::Unknown(value.to_owned()).into())
        }
    }
}

async fn get_external_buckconfig_paths(
    file_ops: &mut dyn ConfigParserFileOps,
) -> buck2_error::Result<Vec<ConfigPath>> {
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
) -> buck2_error::Result<Vec<ConfigPath>> {
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

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use buck2_cli_proto::ConfigOverride;
    use buck2_core::cells::cell_root_path::CellRootPath;
    use buck2_core::cells::cell_root_path::CellRootPathBuf;
    use buck2_core::cells::external::ExternalCellOrigin;
    use buck2_core::cells::external::GitCellSetup;
    use buck2_core::cells::name::CellName;
    use dice::DiceComputations;
    use indoc::indoc;

    use crate::external_cells::EXTERNAL_CELLS_IMPL;
    use crate::external_cells::ExternalCellsImpl;
    use crate::file_ops::delegate::FileOpsDelegate;
    use crate::legacy_configs::cells::BuckConfigBasedCells;
    use crate::legacy_configs::configs::testing::TestConfigParserFileOps;
    use crate::legacy_configs::configs::tests::assert_config_value;
    use crate::legacy_configs::key::BuckconfigKeyRef;

    #[tokio::test]
    async fn test_cells() -> buck2_error::Result<()> {
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

        let cells = BuckConfigBasedCells::testing_parse_with_file_ops(&mut file_ops, &[]).await?;

        let resolver = &cells.cell_resolver;

        let root_instance = resolver.get(CellName::testing_new("root"))?;
        let other_instance = resolver.get(CellName::testing_new("other"))?;
        let tp_instance = resolver.get(CellName::testing_new("third_party"))?;

        assert_eq!("", root_instance.path().as_str());
        assert_eq!("other", other_instance.path().as_str());
        assert_eq!("third_party", tp_instance.path().as_str());

        assert_eq!(
            "other",
            resolver
                .root_cell_cell_alias_resolver()
                .resolve("other_alias")?
                .as_str()
        );

        let tp_resolver = cells
            .get_cell_alias_resolver_for_cwd_fast_with_file_ops(
                &mut file_ops,
                tp_instance.path().as_project_relative_path(),
            )
            .await?;

        assert_eq!("other", tp_resolver.resolve("other_alias")?.as_str());

        Ok(())
    }

    #[tokio::test]
    async fn test_multi_cell_with_config_file() -> buck2_error::Result<()> {
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

        let cells = BuckConfigBasedCells::testing_parse_with_file_ops(
            &mut file_ops,
            &[ConfigOverride::file(
                "cli-conf",
                Some(CellRootPathBuf::testing_new("other")),
            )],
        )
        .await?;

        let root_config = cells
            .parse_single_cell_with_file_ops(CellName::testing_new("root"), &mut file_ops)
            .await?;
        let other_config = cells
            .parse_single_cell_with_file_ops(CellName::testing_new("other"), &mut file_ops)
            .await?;
        let tp_config = cells
            .parse_single_cell_with_file_ops(CellName::testing_new("third_party"), &mut file_ops)
            .await?;

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

    #[tokio::test]
    async fn test_multi_cell_no_repositories_in_non_root_cell() -> buck2_error::Result<()> {
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

        let cells = BuckConfigBasedCells::testing_parse_with_file_ops(&mut file_ops, &[]).await?;

        let other_config = cells
            .parse_single_cell_with_file_ops(CellName::testing_new("other"), &mut file_ops)
            .await?;

        assert_eq!(
            other_config.get(BuckconfigKeyRef {
                section: "foo",
                property: "bar"
            }),
            Some("baz")
        );

        Ok(())
    }

    #[tokio::test]
    async fn test_multi_cell_with_cell_relative() -> buck2_error::Result<()> {
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

        let cells = BuckConfigBasedCells::testing_parse_with_file_ops(
            &mut file_ops,
            &[
                ConfigOverride::file("app-conf", Some(CellRootPathBuf::testing_new("other"))),
                ConfigOverride::file("global-conf", Some(CellRootPathBuf::testing_new(""))),
            ],
        )
        .await?;

        let other_config = cells
            .parse_single_cell_with_file_ops(CellName::testing_new("other"), &mut file_ops)
            .await?;

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

    #[tokio::test]
    async fn test_local_config_file_overwrite_config_file() -> buck2_error::Result<()> {
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

        let cells = BuckConfigBasedCells::testing_parse_with_file_ops(&mut file_ops, &[]).await?;

        let config = cells
            .parse_single_cell_with_file_ops(CellName::testing_new("root"), &mut file_ops)
            .await?;
        // No local override
        assert_config_value(&config, "apple", "key", "value1");
        // local override to new value
        assert_config_value(&config, "apple", "key2", "value5");
        // local override new field
        assert_config_value(&config, "apple", "key3", "value4");
        // local override new section
        assert_config_value(&config, "orange", "key", "value3");

        Ok(())
    }

    #[tokio::test]
    async fn test_multi_cell_local_config_file_overwrite_config_file() -> buck2_error::Result<()> {
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

        let cells = BuckConfigBasedCells::testing_parse_with_file_ops(&mut file_ops, &[]).await?;

        let root_config = cells
            .parse_single_cell_with_file_ops(CellName::testing_new("root"), &mut file_ops)
            .await?;
        let other_config = cells
            .parse_single_cell_with_file_ops(CellName::testing_new("other"), &mut file_ops)
            .await?;

        // No local override
        assert_config_value(&root_config, "apple", "key", "value1");
        // local override to new value
        assert_config_value(&root_config, "apple", "key2", "value5");
        // local override new field
        assert_config_value(&root_config, "apple", "key3", "value4");
        // local override new section
        assert_config_value(&root_config, "orange", "key", "value3");

        // No local override
        assert_config_value(&other_config, "apple", "key", "othervalue1");
        // local override to new value
        assert_config_value(&other_config, "apple", "key2", "othervalue5");
        // local override new field
        assert_config_value(&other_config, "apple", "key3", "othervalue4");
        // local override new section
        assert_config_value(&other_config, "orange", "key", "othervalue3");

        Ok(())
    }

    #[tokio::test]
    async fn test_config_arg_with_no_buckconfig() -> buck2_error::Result<()> {
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

        let cells = BuckConfigBasedCells::testing_parse_with_file_ops(
            &mut file_ops,
            &[ConfigOverride::flag_no_cell("some_section.key=value1")],
        )
        .await?;
        let config = cells
            .parse_single_cell_with_file_ops(CellName::testing_new("other"), &mut file_ops)
            .await?;

        assert_config_value(&config, "some_section", "key", "value1");

        Ok(())
    }

    #[tokio::test]
    async fn test_cell_config_section_name() -> buck2_error::Result<()> {
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

        let resolver = BuckConfigBasedCells::testing_parse_with_file_ops(&mut file_ops, &[])
            .await?
            .cell_resolver;

        assert_eq!(
            "other",
            resolver
                .root_cell_cell_alias_resolver()
                .resolve("other_alias")?
                .as_str(),
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
            ) -> buck2_error::Result<Arc<dyn FileOpsDelegate>> {
                // Not used in these tests
                unreachable!()
            }

            fn check_bundled_cell_exists(&self, cell_name: CellName) -> buck2_error::Result<()> {
                if cell_name.as_str() == "test_bundled_cell" {
                    Ok(())
                } else {
                    Err(buck2_error::buck2_error!(
                        buck2_error::ErrorTag::Input,
                        "No bundled cell with name `{}`",
                        cell_name
                    ))
                }
            }

            async fn expand(
                &self,
                _ctx: &mut DiceComputations<'_>,
                _cell_name: CellName,
                _origin: ExternalCellOrigin,
                _path: &CellRootPath,
            ) -> buck2_error::Result<()> {
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

    #[tokio::test]
    async fn test_external_cell_configs() -> buck2_error::Result<()> {
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

        let resolver = BuckConfigBasedCells::testing_parse_with_file_ops(&mut file_ops, &[])
            .await?
            .cell_resolver;

        let other1 = resolver
            .root_cell_cell_alias_resolver()
            .resolve("other_alias")
            .unwrap();
        let other2 = resolver
            .root_cell_cell_alias_resolver()
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
            resolver
                .root_cell_cell_alias_resolver()
                .resolve("other_alias")
                .unwrap()
                .as_str(),
            "test_bundled_cell",
        );

        Ok(())
    }

    #[tokio::test]
    async fn test_nested_external_cell_configs() -> buck2_error::Result<()> {
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

        BuckConfigBasedCells::testing_parse_with_file_ops(&mut file_ops, &[])
            .await
            .err()
            .unwrap();

        Ok(())
    }

    #[tokio::test]
    async fn test_missing_bundled_cell() -> buck2_error::Result<()> {
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

        let e = BuckConfigBasedCells::testing_parse_with_file_ops(&mut file_ops, &[])
            .await
            .err()
            .unwrap();

        let e = format!("{e:?}");
        assert!(e.contains("No bundled cell"), "error: {e}");

        Ok(())
    }

    #[tokio::test]
    async fn test_git_external_cell() -> buck2_error::Result<()> {
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

        let resolver = BuckConfigBasedCells::testing_parse_with_file_ops(&mut file_ops, &[])
            .await?
            .cell_resolver;

        let instance = resolver.get(CellName::testing_new("libfoo")).unwrap();

        assert_eq!(
            instance.external(),
            Some(&ExternalCellOrigin::Git(GitCellSetup {
                git_origin: "https://github.com/jeff/libfoo.git".into(),
                commit: "aaaaaaaabbbbbbbbccccccccddddddddeeeeeeee".into(),
                object_format: None,
            })),
        );

        Ok(())
    }

    #[tokio::test]
    async fn test_git_external_cell_invalid_sha1() -> buck2_error::Result<()> {
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

        let e = BuckConfigBasedCells::testing_parse_with_file_ops(&mut file_ops, &[])
            .await
            .err()
            .unwrap();

        let e = format!("{e:?}");
        assert!(e.contains("not a valid SHA1 digest"), "error: {e}");

        Ok(())
    }
}
