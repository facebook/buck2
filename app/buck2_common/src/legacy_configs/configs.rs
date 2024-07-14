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
use std::fmt;
use std::fmt::Display;
use std::io::BufRead;
use std::path::PathBuf;
use std::sync::Arc;

use allocative::Allocative;
use anyhow::Context;
use buck2_cli_proto::ConfigOverride;
use buck2_core::cells::name::CellName;
use buck2_core::fs::paths::abs_norm_path::AbsNormPath;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::paths::file_name::FileNameBuf;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use dupe::Dupe;
use futures::future::BoxFuture;
use futures::FutureExt;
use starlark_map::small_map::SmallMap;
use starlark_map::sorted_map::SortedMap;

use crate::legacy_configs::args::ResolvedLegacyConfigArg;
use crate::legacy_configs::parser::LegacyConfigParser;

/// A collection of configs, keyed by cell.
#[derive(Clone, Dupe, Debug, Allocative)]
pub struct LegacyBuckConfigs {
    pub(crate) data: Arc<SortedMap<CellName, LegacyBuckConfig>>,
}

impl LegacyBuckConfigs {
    pub fn new(data: HashMap<CellName, LegacyBuckConfig>) -> Self {
        let data = SortedMap::from_iter(data);
        Self {
            data: Arc::new(data),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct MainConfigFile {
    pub(crate) path: AbsNormPathBuf,

    /// if a main config file is in project or global
    pub(crate) owned_by_project: bool,
}

#[derive(Clone, Dupe, Debug, Allocative)]
pub struct LegacyBuckConfig(pub(crate) Arc<ConfigData>);

#[derive(Debug, Allocative)]
pub(crate) struct ConfigData {
    pub(crate) values: SortedMap<String, LegacyBuckConfigSection>,
}

#[derive(Clone, Debug, Allocative)]
pub(crate) enum ResolvedValue {
    // A placeholder used before we do resolution.
    Unknown,
    // Indicates that there's no resolution required, the resolved value and raw value are the same.
    Literal,
    // The resolved value for non-literals.
    Resolved(String),
}

#[derive(Debug, Allocative)]
pub(crate) struct ConfigFileLocation {
    pub(crate) path: String,
    pub(crate) include_source: Option<Location>,
}

#[derive(Clone, Debug, Allocative)]
pub(crate) struct ConfigFileLocationWithLine {
    pub(crate) source_file: Arc<ConfigFileLocation>,
    pub(crate) line: usize,
}

#[derive(Clone, Debug, Allocative)]
pub(crate) enum Location {
    File(ConfigFileLocationWithLine),
    CommandLineArgument,
}

impl Location {
    pub(crate) fn as_legacy_buck_config_location(&self) -> LegacyBuckConfigLocation {
        match self {
            Self::File(x) => LegacyBuckConfigLocation::File(&x.source_file.path, x.line),
            Self::CommandLineArgument => LegacyBuckConfigLocation::CommandLineArgument,
        }
    }
}

// Represents a config section and key only, for example, `cxx.compiler`.
#[derive(Clone, Debug)]
pub struct ConfigSectionAndKey {
    //  TODO(scottcao): Add cell_path
    pub section: String,
    pub key: String,
}

#[derive(buck2_error::Error, Debug)]
pub(crate) enum ConfigArgumentParseError {
    #[error("Could not find section separator (`.`) in pair `{0}`")]
    NoSectionDotSeparator(String),
    #[error("Could not find equals sign (`=`) in pair `{0}`")]
    NoEqualsSeparator(String),

    #[error("Expected key-value in format of `section.key=value` but only got `{0}`")]
    MissingData(String),

    #[error("Contains whitespace in key-value pair `{0}`")]
    WhitespaceInKeyOrValue(String),

    #[error("Specifying cells via cli config overrides is banned (`{0}.key=value`)")]
    CellOverrideViaCliConfig(&'static str),
}

// Parses config key in the format `section.key`
pub fn parse_config_section_and_key(
    raw_section_and_key: &str,
    raw_arg_in_err: Option<&str>, // Used in error strings to preserve the original config argument, not just section and key
) -> anyhow::Result<ConfigSectionAndKey> {
    let raw_arg = raw_arg_in_err.unwrap_or(raw_section_and_key);
    let (raw_section, raw_key) = raw_section_and_key
        .split_once('.')
        .ok_or_else(|| ConfigArgumentParseError::NoSectionDotSeparator(raw_arg.to_owned()))?;

    // We only trim the section + key, whitespace in values needs to be preserved. For example,
    // Buck can be invoked with --config section.key="Some Value" that contains important whitespace.
    let trimmed_section = raw_section.trim_start();
    if trimmed_section.find(char::is_whitespace).is_some()
        || raw_key.find(char::is_whitespace).is_some()
    {
        return Err(anyhow::anyhow!(
            ConfigArgumentParseError::WhitespaceInKeyOrValue(raw_arg.to_owned())
        ));
    }

    if trimmed_section.is_empty() || raw_key.is_empty() {
        return Err(anyhow::anyhow!(ConfigArgumentParseError::MissingData(
            raw_arg.to_owned()
        )));
    }

    Ok(ConfigSectionAndKey {
        section: trimmed_section.to_owned(),
        key: raw_key.to_owned(),
    })
}

#[derive(Debug, Allocative)]
pub(crate) struct ConfigValue {
    raw_value: String,
    pub(crate) resolved_value: ResolvedValue,
    pub(crate) source: Location,
}

#[derive(Debug, Default, Allocative)]
pub struct LegacyBuckConfigSection {
    pub(crate) values: SortedMap<String, ConfigValue>,
}

impl ConfigValue {
    pub(crate) fn new_raw(source: ConfigFileLocationWithLine, value: String) -> Self {
        Self {
            raw_value: value,
            resolved_value: ResolvedValue::Unknown,
            source: Location::File(source),
        }
    }

    pub(crate) fn new_raw_arg(raw_value: String) -> Self {
        Self {
            raw_value,
            resolved_value: ResolvedValue::Unknown,
            source: Location::CommandLineArgument,
        }
    }

    pub(crate) fn raw_value(&self) -> &str {
        &self.raw_value
    }

    pub(crate) fn as_str(&self) -> &str {
        match &self.resolved_value {
            ResolvedValue::Literal => &self.raw_value,
            ResolvedValue::Resolved(v) => v,
            ResolvedValue::Unknown => {
                unreachable!("cannot call as_str() until all values are resolved")
            }
        }
    }
}

pub struct ConfigDirEntry {
    pub(crate) name: FileNameBuf,
    pub(crate) is_dir: bool,
}

#[async_trait::async_trait]
pub trait ConfigParserFileOps: Send + Sync {
    async fn read_file_lines(
        &mut self,
        path: &AbsNormPath,
    ) -> anyhow::Result<Box<dyn Iterator<Item = Result<String, std::io::Error>> + Send>>;

    async fn file_exists(&mut self, path: &AbsNormPath) -> bool;

    async fn read_dir(&mut self, path: &AbsNormPath) -> anyhow::Result<Vec<ConfigDirEntry>>;
}

#[derive(buck2_error::Error, Debug)]
enum ReadDirError {
    #[error("Non-utf8 entry `{0}` in directory `{1}`")]
    NotUtf8(String, String),
}

pub(crate) struct DefaultConfigParserFileOps {}

#[async_trait::async_trait]
impl ConfigParserFileOps for DefaultConfigParserFileOps {
    async fn read_file_lines(
        &mut self,
        path: &AbsNormPath,
    ) -> anyhow::Result<Box<dyn Iterator<Item = Result<String, std::io::Error>> + Send>> {
        let f = std::fs::File::open(path).with_context(|| format!("Reading file `{:?}`", path))?;
        let file = std::io::BufReader::new(f);
        Ok(Box::new(file.lines()))
    }

    async fn file_exists(&mut self, path: &AbsNormPath) -> bool {
        PathBuf::from(path.as_os_str()).exists()
    }

    async fn read_dir(&mut self, path: &AbsNormPath) -> anyhow::Result<Vec<ConfigDirEntry>> {
        let read_dir = match std::fs::read_dir(path.as_path()) {
            Ok(read_dir) => read_dir,
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(Vec::new()),
            Err(e) if e.kind() == std::io::ErrorKind::NotADirectory => {
                tracing::warn!("Expected a directory of buckconfig files at: `{}`", path);
                return Ok(Vec::new());
            }
            Err(e) => return Err(e.into()),
        };
        let mut entries = Vec::new();
        for entry in read_dir {
            let entry = entry?;
            let name = entry.file_name().into_string().map_err(|s| {
                ReadDirError::NotUtf8(
                    std::path::Path::display(s.as_ref()).to_string(),
                    path.to_string(),
                )
            })?;
            let name = FileNameBuf::try_from(name)?;
            let file_type = entry.file_type()?;
            if file_type.is_file() {
                entries.push(ConfigDirEntry {
                    name,
                    is_dir: false,
                });
            } else if file_type.is_dir() {
                entries.push(ConfigDirEntry { name, is_dir: true });
            } else {
                tracing::warn!(
                    "Expected a directory of buckconfig files at `{}`, but this entry was not a file or directory: `{}`",
                    path,
                    name,
                );
            }
        }
        Ok(entries)
    }
}

pub struct LegacyBuckConfigValue<'a> {
    pub(crate) value: &'a ConfigValue,
}

#[derive(PartialEq, Debug)]
pub enum LegacyBuckConfigLocation<'a> {
    File(&'a str, usize),
    CommandLineArgument,
}

impl<'a> Display for LegacyBuckConfigLocation<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::File(file, line) => {
                write!(f, "at {}:{}", file, line)
            }
            Self::CommandLineArgument => {
                write!(f, "on the command line")
            }
        }
    }
}

impl<'a> LegacyBuckConfigValue<'a> {
    pub fn as_str(&self) -> &'a str {
        self.value.as_str()
    }

    pub fn raw_value(&self) -> &str {
        self.value.raw_value()
    }

    pub fn location(&self) -> LegacyBuckConfigLocation {
        match &self.value.source {
            Location::File(file) => {
                LegacyBuckConfigLocation::File(&file.source_file.path, file.line)
            }
            Location::CommandLineArgument => LegacyBuckConfigLocation::CommandLineArgument,
        }
    }

    pub fn location_stack(&self) -> Vec<LegacyBuckConfigLocation> {
        let mut res = Vec::new();
        let mut location = Some(&self.value.source);

        while let Some(loc) = location.take() {
            match &loc {
                Location::File(loc) => {
                    res.push(LegacyBuckConfigLocation::File(
                        &loc.source_file.path,
                        loc.line,
                    ));
                    location = loc.source_file.include_source.as_ref();
                }
                Location::CommandLineArgument => {
                    // No stack
                }
            }
        }
        res
    }
}

impl LegacyBuckConfig {
    pub fn empty() -> Self {
        Self(Arc::new(ConfigData {
            values: SortedMap::new(),
        }))
    }

    pub(crate) async fn parse_with_file_ops_with_includes(
        main_config_files: &[MainConfigFile],
        cell_path: AbsNormPathBuf,
        file_ops: &mut dyn ConfigParserFileOps,
        config_args: &[ResolvedLegacyConfigArg],
        follow_includes: bool,
    ) -> anyhow::Result<Self> {
        let mut parser = LegacyConfigParser::new();
        for main_config_file in main_config_files {
            parser
                .parse_file(&main_config_file.path, None, follow_includes, file_ops)
                .await?;
        }

        for config_arg in config_args {
            match config_arg {
                ResolvedLegacyConfigArg::Flag(config_value) => {
                    parser.apply_config_arg(config_value, cell_path.clone())?
                }
                ResolvedLegacyConfigArg::File(file_path) => {
                    parser
                        .parse_file(
                            file_path,
                            Some(Location::CommandLineArgument),
                            follow_includes,
                            file_ops,
                        )
                        .await?
                }
            };
        }

        parser.finish()
    }
}

// Options on how to exactly parse config files
pub(crate) struct BuckConfigParseOptions {
    // Defines whether includes are followed, this can significantly reduce parse time.
    pub(crate) follow_includes: bool,
}

pub(crate) fn push_all_files_from_a_directory<'a>(
    buckconfig_paths: &'a mut Vec<MainConfigFile>,
    folder_path: &'a AbsNormPath,
    owned_by_project: bool,
    file_ops: &'a mut dyn ConfigParserFileOps,
) -> BoxFuture<'a, anyhow::Result<()>> {
    async move {
        for entry in file_ops.read_dir(folder_path).await? {
            let entry_path = folder_path.join(&entry.name);
            if entry.is_dir {
                push_all_files_from_a_directory(
                    buckconfig_paths,
                    &entry_path,
                    owned_by_project,
                    file_ops,
                )
                .await?;
            } else {
                buckconfig_paths.push(MainConfigFile {
                    path: entry_path,
                    owned_by_project,
                });
            }
        }

        Ok(())
    }
    .boxed()
}

#[derive(Debug, Clone, PartialEq)]
pub enum ConfigDiffEntry {
    Added(String),
    Removed(String),
    Changed { new: String, old: String },
}

// config name to config diff
#[derive(Debug, Clone, Default, PartialEq)]
pub struct SectionConfigDiff(pub SmallMap<String, ConfigDiffEntry>);

// section name to config diffs
#[derive(Debug, Clone, Default, PartialEq)]
pub struct CellConfigDiff(pub SmallMap<String, SectionConfigDiff>);

#[derive(Debug, Clone, Default, PartialEq)]
pub struct ConfigDiffMetrics {
    // count of changed/removed/added config antries
    pub count: usize,
    // key + old value + new value
    pub size_bytes: usize,
    // if diff map is complete or partial due to size limit
    pub diff_size_exceeded: bool,
    // cell to config diffs
    pub diff: SmallMap<CellName, CellConfigDiff>,
}

pub mod testing {
    use std::cmp::min;

    use super::*;
    use crate::legacy_configs::args::CellResolutionState;
    use crate::legacy_configs::cells::create_project_filesystem;

    pub fn parse(data: &[(&str, &str)], path: &str) -> anyhow::Result<LegacyBuckConfig> {
        parse_with_config_args(data, path, &[])
    }

    pub fn parse_with_config_args(
        data: &[(&str, &str)],
        path: &str,
        config_args: &[ConfigOverride],
    ) -> anyhow::Result<LegacyBuckConfig> {
        let mut file_ops = TestConfigParserFileOps::new(data)?;
        #[cfg(not(windows))]
        let path = &AbsNormPathBuf::from(path.into())?;
        // Need to add some disk drive on Windows to make path absolute.
        #[cfg(windows)]
        let path = &AbsNormPathBuf::from(format!("C:{}", path))?;
        let project_fs = create_project_filesystem();
        // As long as people don't pass config files, making up values here is ok
        let cell_resolution = CellResolutionState {
            project_filesystem: &project_fs,
            cwd: ProjectRelativePath::empty(),
            cell_resolver: OnceCell::new(),
        };
        let processed_config_args =
            LegacyBuckConfig::resolve_config_args(config_args, &cell_resolution, &mut file_ops)?;
        futures::executor::block_on(LegacyBuckConfig::parse_with_file_ops_with_includes(
            &[MainConfigFile {
                path: path.to_buf(),
                owned_by_project: true,
            }],
            path.clone(),
            &mut file_ops,
            &processed_config_args,
            true,
        ))
    }

    pub struct TestConfigParserFileOps {
        data: HashMap<AbsNormPathBuf, String>,
    }

    impl TestConfigParserFileOps {
        pub fn new(data: &[(&str, &str)]) -> anyhow::Result<Self> {
            let mut holder_data = HashMap::new();
            for (file, content) in data {
                #[cfg(not(windows))]
                let file_path = (*file).to_owned();
                // Need to add some disk drive on Windows to make path absolute.
                #[cfg(windows)]
                let file_path = format!("C:{}", file);
                holder_data.insert(AbsNormPathBuf::from(file_path)?, (*content).to_owned());
            }
            Ok(TestConfigParserFileOps { data: holder_data })
        }
    }

    #[async_trait::async_trait]
    impl ConfigParserFileOps for TestConfigParserFileOps {
        async fn file_exists(&mut self, path: &AbsNormPath) -> bool {
            self.data.contains_key(path)
        }

        async fn read_file_lines(
            &mut self,
            path: &AbsNormPath,
        ) -> anyhow::Result<
            Box<(dyn std::iter::Iterator<Item = Result<String, std::io::Error>> + Send + 'static)>,
        > {
            let content = self
                .data
                .get(path)
                .ok_or_else(|| anyhow::anyhow!("didn't have data for {:?}", path))?
                .to_owned();
            // Need a Read implementation that owns the bytes.
            struct StringReader(Vec<u8>, usize);
            impl std::io::Read for StringReader {
                fn read(&mut self, buf: &mut [u8]) -> Result<usize, std::io::Error> {
                    let remaining = self.0.len() - self.1;
                    let to_return = min(remaining, buf.len());
                    buf[..to_return].clone_from_slice(&self.0[self.1..self.1 + to_return]);
                    self.1 += to_return;
                    Ok(to_return)
                }
            }
            let file = std::io::BufReader::new(StringReader(content.into_bytes(), 0));
            Ok(Box::new(file.lines()))
        }

        async fn read_dir(&mut self, _path: &AbsNormPath) -> anyhow::Result<Vec<ConfigDirEntry>> {
            // This is only used for listing files in `buckconfig.d` directories, which we can just
            // say are always empty in tests
            Ok(Vec::new())
        }
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use buck2_core::fs::paths::abs_path::AbsPath;
    use indoc::indoc;
    use itertools::Itertools;
    use starlark_map::smallmap;

    use super::testing::*;
    use super::*;
    use crate::legacy_configs::key::BuckconfigKeyRef;

    pub(crate) fn assert_config_value(
        config: &LegacyBuckConfig,
        section: &str,
        key: &str,
        expected: &str,
    ) {
        match config.get_section(section) {
            None => {
                panic!(
                    "Expected config to have section `{}`, but had sections `<{}>`",
                    section,
                    config.sections().join(", ")
                );
            }
            Some(values) => match values.get(key) {
                None => panic!(
                    "Expected section `{}` to have key `{}`, but had keys `<{}>`",
                    section,
                    key,
                    values.keys().join(", ")
                ),
                Some(v) if v.as_str() != expected => {
                    panic!(
                        "Expected `{}.{}` to have value `{}`. Got `{}`.",
                        section,
                        key,
                        expected,
                        v.as_str()
                    );
                }
                _ => {}
            },
        }
    }

    fn assert_config_value_is_empty(config: &LegacyBuckConfig, section: &str, key: &str) {
        match config.get_section(section) {
            Some(values) => match values.get(key) {
                Some(v) => {
                    panic!(
                        "Expected `{}.{}` to not exist. Got `{}` for value.",
                        section,
                        key,
                        v.as_str()
                    );
                }
                _ => {}
            },
            _ => {}
        };
    }

    #[test]
    fn test_simple() -> anyhow::Result<()> {
        let config = parse(
            &[(
                "/config",
                indoc!(
                    r#"
            [section]
                int = 1
                string = hello
                multiline = hello \
                            world\
                            !

                # this is a comment
                commented = okay

            [new_section]
                overridden = 1

            [another_section]
                some_val = 2

            [new_section]
                reopened = ok
                # override overridden
                overridden = 3

                    # note trailing whitespace
                    [bad_formatting]

            value                 =             1
        "#
                ),
            )],
            "/config",
        )?;

        assert_eq!(
            None,
            config.get(BuckconfigKeyRef {
                section: "section",
                property: "missing"
            })
        );
        assert_eq!(
            None,
            config.get(BuckconfigKeyRef {
                section: "missing",
                property: "int"
            })
        );
        assert_config_value(&config, "section", "int", "1");
        assert_config_value(&config, "section", "string", "hello");
        // Note that lines are all trimmed, so leading whitespace after a newline is
        // dropped.
        assert_config_value(&config, "section", "multiline", "hello world!");
        assert_config_value(&config, "section", "commented", "okay");
        assert_config_value(&config, "another_section", "some_val", "2");
        assert_config_value(&config, "new_section", "reopened", "ok");
        assert_config_value(&config, "new_section", "overridden", "3");
        assert_config_value(&config, "bad_formatting", "value", "1");
        Ok(())
    }

    #[test]
    fn test_comments() -> anyhow::Result<()> {
        let config = parse(
            &[(
                "/config",
                indoc!(
                    r#"
            [section1] # stuff
                key1 = value1
            [section2#name]
                key2 = value2
        "#
                ),
            )],
            "/config",
        )?;
        assert_config_value(&config, "section1", "key1", "value1");
        assert_config_value(&config, "section2#name", "key2", "value2");
        Ok(())
    }

    #[test]
    fn test_references() -> anyhow::Result<()> {
        let config = parse(
            &[(
                "/config",
                indoc!(
                    r#"

            [section1]
                ref1_1 = ref1_1<$(config section3.ref3_2)>

            [section2]
                ref2_1 = ref2_1<$(config section3.ref3_1)>
                ref2_2 = ref2_2<$(config section2.ref2_1)>
            [section3]
                ref3_1 = ref3_1<$(config section1.ref1_1), $(config section3.ref3_2)>
                ref3_2 = ref3_2

            [simple]
                s1 = $(config simple.s2)$(config simple.s2)$(config simple.s2)
                s2 = $(config simple.s3)$(config simple.s3)$(config simple.s3)
                s3 = x
        "#
                ),
            )],
            "/config",
        )?;

        assert_config_value(
            &config,
            "section2",
            "ref2_2",
            "ref2_2<ref2_1<ref3_1<ref1_1<ref3_2>, ref3_2>>>",
        );

        assert_config_value(&config, "simple", "s1", "xxxxxxxxx");
        Ok(())
    }

    #[test]
    fn test_reference_cycle() -> anyhow::Result<()> {
        let res = parse(
            &[(
                "/config",
                indoc!(
                    r#"

            [x]
                a = $(config x.b)
                b = $(config x.c)
                c = $(config x.d)
                d = $(config x.e)
                e = $(config x.f)
                f = $(config x.g)
                g = $(config x.d)
        "#
                ),
            )],
            "/config",
        );

        match res {
            Ok(_) => panic!("Expected failure."),
            Err(e) => {
                let message = e.to_string();
                let cycle = "`x.d` -> `x.e` -> `x.f` -> `x.g` -> `x.d`";
                assert!(
                    message.contains(cycle),
                    "Expected error to contain \"{}\", but was `{}`",
                    cycle,
                    message
                );
            }
        }

        Ok(())
    }

    #[test]
    fn test_includes() -> anyhow::Result<()> {
        let config = parse(
            &[
                (
                    "/base",
                    indoc!(
                        r#"
                            base = okay!
                        "#
                    ),
                ),
                (
                    "/section",
                    indoc!(
                        r#"
                            [section]
                        "#
                    ),
                ),
                (
                    "/some/deep/dir/includes_base",
                    indoc!(
                        r#"
                            <file:../../../base>
                        "#
                    ),
                ),
                (
                    "/includes_section",
                    indoc!(
                        r#"
                            <file:section>
                        "#
                    ),
                ),
                (
                    "/config",
                    indoc!(
                        r#"
                        # use a couple optional includes in here to ensure those work when the file exists.
                        [opened_section]
                            # include into an already open section
                            <?file:base>
                        # start a section with an include
                        <?file:includes_section>
                             key = wild
                             <?file:some/deep/dir/includes_base>
                        [other_section]
                        # ensure can reopen section with an include
                        <file:section>
                              other_key=wildtoo

                        # Check that an optional include for a file that doesn't exist is okay.
                        <?file:this_file_doesnt_exist>
                        "#
                    ),
                ),
                (
                    "/test_bad_include",
                    indoc!(
                        r#"
                        <file:this_file_doesnt_exist>
                        "#
                    ),
                ),
            ],
            "/config",
        )?;

        assert_config_value(&config, "opened_section", "base", "okay!");
        assert_config_value(&config, "section", "base", "okay!");
        // Note that lines are all trimmed, so leading whitespace after a newline is
        // dropped.
        assert_config_value(&config, "section", "key", "wild");
        assert_config_value(&config, "section", "other_key", "wildtoo");
        Ok(())
    }

    #[test]
    fn test_config_args_ordering() -> anyhow::Result<()> {
        let config_args = vec![
            ConfigOverride::flag("apple.key=value1"),
            ConfigOverride::flag("apple.key=value2"),
        ];
        let config =
            parse_with_config_args(&[("/config", indoc!(r#""#))], "/config", &config_args)?;
        assert_config_value(&config, "apple", "key", "value2");

        Ok(())
    }

    #[test]
    fn test_config_args_empty() -> anyhow::Result<()> {
        let config_args = vec![ConfigOverride::flag("apple.key=")];
        let config =
            parse_with_config_args(&[("/config", indoc!(r#""#))], "/config", &config_args)?;
        assert_config_value_is_empty(&config, "apple", "key");

        Ok(())
    }

    #[test]
    fn test_config_args_overwrite_config_file() -> anyhow::Result<()> {
        let config_args = vec![ConfigOverride::flag("apple.key=value2")];
        let config = parse_with_config_args(
            &[(
                "/config",
                indoc!(
                    r#"
            [apple]
                key = value1
        "#
                ),
            )],
            "/config",
            &config_args,
        )?;

        assert_config_value(&config, "apple", "key", "value2");

        let apple_section = config.get_section("apple").unwrap();
        let key_value = apple_section.get("key").unwrap();
        assert_eq!(
            key_value.location(),
            LegacyBuckConfigLocation::CommandLineArgument
        );

        Ok(())
    }

    #[test]
    fn test_section_and_key() -> anyhow::Result<()> {
        // Valid Formats

        let normal_section_and_key = parse_config_section_and_key("apple.key", None)?;

        assert_eq!("apple", normal_section_and_key.section);
        assert_eq!("key", normal_section_and_key.key);

        // Whitespace

        let section_leading_whitespace = parse_config_section_and_key("  apple.key", None)?;
        assert_eq!("apple", section_leading_whitespace.section);
        assert_eq!("key", section_leading_whitespace.key);

        let pair_with_whitespace_in_key = parse_config_section_and_key("apple. key", None);
        assert!(pair_with_whitespace_in_key.is_err());

        // Invalid Formats

        let pair_without_dot = parse_config_section_and_key("applekey", None);
        assert!(pair_without_dot.is_err());

        Ok(())
    }

    #[test]
    fn test_config_file_args_overwrite_config_file() -> anyhow::Result<()> {
        #[cfg(not(windows))]
        let file_arg = "/cli-config";
        #[cfg(windows)]
        let file_arg = "C:/cli-config";
        let config_args = vec![
            ConfigOverride::flag("apple.key=value3"),
            ConfigOverride::file(file_arg),
        ];
        let config = parse_with_config_args(
            &[
                (
                    "/config",
                    indoc!(
                        r#"
            [apple]
                key = value1
        "#
                    ),
                ),
                (
                    "/cli-config",
                    indoc!(
                        r#"
            [apple]
                key = value2
        "#
                    ),
                ),
            ],
            "/config",
            &config_args,
        )?;

        assert_config_value(&config, "apple", "key", "value2");

        let apple_section = config.get_section("apple").unwrap();
        let key_value = apple_section.get("key").unwrap();
        #[cfg(not(windows))]
        let expected_path = LegacyBuckConfigLocation::File("/cli-config", 2);
        #[cfg(windows)]
        let expected_path = LegacyBuckConfigLocation::File("C:/cli-config", 2);
        assert_eq!(key_value.location(), expected_path);

        Ok(())
    }

    #[test]
    fn test_config_args_cell_in_value() -> anyhow::Result<()> {
        let config_args = vec![ConfigOverride::flag("apple.key=foo//value1")];
        let config =
            parse_with_config_args(&[("/config", indoc!(r#""#))], "/config", &config_args)?;
        assert_config_value(&config, "apple", "key", "foo//value1");

        Ok(())
    }

    #[test]
    fn test_diff_metrics_equal_configs() -> anyhow::Result<()> {
        let cell = CellName::testing_new("root");
        let config_args = vec![
            ConfigOverride::flag("buck2.config_diff_size_limit=10000"),
            ConfigOverride::flag("apple.key=value1"),
        ];
        let config =
            parse_with_config_args(&[("/config", indoc!(r#""#))], "/config", &config_args)?;

        let configs = LegacyBuckConfigs::new(hashmap![
            CellName::testing_new("root") =>
            config
        ]);

        let metrics = ConfigDiffMetrics::new(cell, &configs, &configs);

        assert_eq!(metrics.count, 0);
        assert_eq!(metrics.has_changed(), false);
        assert_eq!(metrics.size_bytes, 0);
        assert_eq!(metrics.diff, SmallMap::new());
        assert_eq!(metrics.diff_size_exceeded, false);
        Ok(())
    }

    #[test]
    fn test_diff_metrics_with_empty() -> anyhow::Result<()> {
        let cell = CellName::testing_new("root");

        let key = "key";
        let value = "value1";
        let limit_key = "config_diff_size_limit";
        let limit_value = "10000";
        let config_args = vec![
            ConfigOverride::flag(&format!("buck2.{limit_key}={limit_value}")),
            ConfigOverride::flag(&format!("apple.{key}={value}")),
        ];
        let config =
            parse_with_config_args(&[("/config", indoc!(r#""#))], "/config", &config_args)?;

        let configs1 = LegacyBuckConfigs::new(hashmap![
            cell => config
        ]);
        let empty = LegacyBuckConfigs::new(hashmap![]);
        let metrics = ConfigDiffMetrics::new(cell, &configs1, &empty);

        assert_eq!(metrics.count, 2);
        assert_eq!(metrics.has_changed(), true);
        assert_eq!(
            metrics.size_bytes,
            key.len() + value.len() + limit_key.len() + limit_value.len()
        );
        let expected = smallmap![
            cell => CellConfigDiff(smallmap![
                "apple".to_owned() => SectionConfigDiff(smallmap![
                    key.to_owned() => ConfigDiffEntry::Added(value.to_owned())
                ]),
                "buck2".to_owned() => SectionConfigDiff(smallmap![
                    limit_key.to_owned() => ConfigDiffEntry::Added(limit_value.to_owned())
                ]),
            ])
        ];
        assert_eq!(metrics.diff, expected);
        assert_eq!(metrics.diff_size_exceeded, false);
        Ok(())
    }

    #[test]
    fn test_diff_metrics_only_changed() -> anyhow::Result<()> {
        let cell = CellName::testing_new("root");

        let key1 = "key1";
        let value1 = "value1";
        let key2 = "key2";
        let value2_1 = "value2";
        let value2_2 = "value3";
        let key3 = "key3";
        let value3 = "value3";

        let config_args1 = vec![
            ConfigOverride::flag("buck2.config_diff_size_limit=10000"),
            ConfigOverride::flag(&format!("apple.{key1}={value1}")),
            ConfigOverride::flag(&format!("apple.{key2}={value2_1}")),
        ];
        let config1 =
            parse_with_config_args(&[("/config", indoc!(r#""#))], "/config", &config_args1)?;

        let config_args2 = vec![
            ConfigOverride::flag("buck2.config_diff_size_limit=10000"),
            ConfigOverride::flag(&format!("apple.{key1}={value1}")),
            ConfigOverride::flag(&format!("apple.{key2}={value2_2}")),
            ConfigOverride::flag(&format!("apple.{key3}={value3}")),
        ];
        let config2 =
            parse_with_config_args(&[("/config", indoc!(r#""#))], "/config", &config_args2)?;

        let configs1 = LegacyBuckConfigs::new(hashmap![cell => config1]);
        let configs2 = LegacyBuckConfigs::new(hashmap![cell => config2]);
        let metrics = ConfigDiffMetrics::new(cell, &configs1, &configs2);

        assert_eq!(metrics.count, 2);
        assert_eq!(metrics.has_changed(), true);
        assert_eq!(
            metrics.size_bytes,
            key2.len() + value2_1.len() + value2_2.len() + key3.len() + value3.len()
        );

        let expected = smallmap![
            cell => CellConfigDiff(smallmap![
                "apple".to_owned() => SectionConfigDiff(smallmap![
                    key2.to_owned() => ConfigDiffEntry::Changed {
                        new: value2_1.to_owned(),
                        old: value2_2.to_owned()
                    },
                    key3.to_owned() => ConfigDiffEntry::Removed(value3.to_owned())
                ])
            ])
        ];
        assert_eq!(metrics.diff, expected);
        assert_eq!(metrics.diff_size_exceeded, false);
        Ok(())
    }

    #[test]
    fn test_diff_metrics_size_exceeded() -> anyhow::Result<()> {
        let cell = CellName::testing_new("root");

        let key1 = "key1";
        let value1 = "value1";
        let key2 = "key2";
        let value2 = "value2";

        let config_args1 = vec![
            ConfigOverride::flag("buck2.config_diff_size_limit=12"),
            ConfigOverride::flag(&format!("apple.{key1}={value1}")),
        ];
        let config1 =
            parse_with_config_args(&[("/config", indoc!(r#""#))], "/config", &config_args1)?;

        let config_args2 = vec![
            ConfigOverride::flag("buck2.config_diff_size_limit=12"),
            ConfigOverride::flag(&format!("apple.{key2}={value2}")),
        ];
        let config2 =
            parse_with_config_args(&[("/config", indoc!(r#""#))], "/config", &config_args2)?;

        let configs1 = LegacyBuckConfigs::new(hashmap![cell => config1]);
        let configs2 = LegacyBuckConfigs::new(hashmap![cell => config2]);
        let metrics = ConfigDiffMetrics::new(cell, &configs1, &configs2);

        assert_eq!(metrics.count, 2);
        assert_eq!(metrics.has_changed(), true);
        assert_eq!(
            metrics.size_bytes,
            key1.len() + value1.len() + key2.len() + value2.len()
        );

        let expected = smallmap![
            cell => CellConfigDiff(smallmap![
                "apple".to_owned() => SectionConfigDiff(smallmap![
                    key1.to_owned() => ConfigDiffEntry::Added(value1.to_owned())
                ])
            ])
        ];
        assert_eq!(metrics.diff, expected);
        assert_eq!(metrics.diff_size_exceeded, true);

        Ok(())
    }

    mod test_push_all_files_from_a_directory {
        use buck2_core::fs::fs_util;

        use super::*;

        #[test]
        fn dir_with_file() -> anyhow::Result<()> {
            let mut v = vec![];
            let dir = tempfile::tempdir()?;
            let root = AbsPath::new(dir.path())?;
            let file = root.join("foo");
            fs_util::write(&file, "")?;

            let file = AbsNormPath::new(&file)?;
            let dir = AbsNormPath::new(&dir)?;

            futures::executor::block_on(push_all_files_from_a_directory(
                &mut v,
                dir,
                false,
                &mut DefaultConfigParserFileOps {},
            ))?;
            assert_eq!(
                v,
                vec![MainConfigFile {
                    path: file.to_owned(),
                    owned_by_project: false,
                }]
            );

            Ok(())
        }

        #[test]
        fn empty_dir() -> anyhow::Result<()> {
            let mut v = vec![];
            let dir = tempfile::tempdir()?;
            let dir = AbsNormPath::new(&dir)?;

            futures::executor::block_on(push_all_files_from_a_directory(
                &mut v,
                dir,
                false,
                &mut DefaultConfigParserFileOps {},
            ))?;
            assert_eq!(v, vec![]);

            Ok(())
        }

        #[test]
        fn non_existent_dir() -> anyhow::Result<()> {
            let mut v = vec![];
            let dir = tempfile::tempdir()?;
            let dir = dir.path().join("bad");
            let dir = AbsNormPath::new(&dir)?;

            futures::executor::block_on(push_all_files_from_a_directory(
                &mut v,
                dir,
                false,
                &mut DefaultConfigParserFileOps {},
            ))?;
            assert_eq!(v, vec![]);

            Ok(())
        }

        #[test]
        fn dir_in_dir() -> anyhow::Result<()> {
            let mut v = vec![];
            let dir = tempfile::tempdir()?;
            let dir = AbsPath::new(dir.path())?;
            fs_util::create_dir_all(dir.join("bad"))?;

            futures::executor::block_on(push_all_files_from_a_directory(
                &mut v,
                AbsNormPath::new(dir)?,
                false,
                &mut DefaultConfigParserFileOps {},
            ))?;
            assert_eq!(v, vec![]);

            Ok(())
        }

        #[test]
        fn file() -> anyhow::Result<()> {
            let mut v = vec![];
            let file = tempfile::NamedTempFile::new()?;
            let file = AbsNormPath::new(file.path())?;

            futures::executor::block_on(push_all_files_from_a_directory(
                &mut v,
                file,
                false,
                &mut DefaultConfigParserFileOps {},
            ))?;
            assert_eq!(v, vec![]);

            Ok(())
        }

        #[test]
        fn dir_with_file_in_dir() -> anyhow::Result<()> {
            let mut v = vec![];
            let dir = tempfile::tempdir()?;
            let dir = AbsPath::new(dir.path())?;
            let nested_dir = dir.join("nested");
            fs_util::create_dir_all(&nested_dir)?;
            let file = nested_dir.join("foo");
            fs_util::write(&file, "")?;

            let file = AbsNormPath::new(&file)?;
            let dir = AbsNormPath::new(&dir)?;

            futures::executor::block_on(push_all_files_from_a_directory(
                &mut v,
                dir,
                false,
                &mut DefaultConfigParserFileOps {},
            ))?;
            assert_eq!(
                v,
                vec![MainConfigFile {
                    path: file.to_owned(),
                    owned_by_project: false,
                }]
            );

            Ok(())
        }
    }
}
