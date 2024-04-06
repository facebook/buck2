/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Contains utilities for dealing with buckv1 concepts (ex. buckv1's
//! .buckconfig files as configuration)

mod access;
pub mod buildfiles;
pub mod cells;
pub mod dice;
pub mod init;
pub mod key;
mod parser;
pub(crate) mod path;
pub mod view;

use std::cell::OnceCell;
use std::collections::HashMap;
use std::fmt;
use std::fmt::Display;
use std::fs;
use std::io::BufRead;
use std::path::PathBuf;
use std::sync::Arc;

use allocative::Allocative;
use anyhow::Context;
use buck2_core::cells::name::CellName;
use buck2_core::cells::CellResolver;
use buck2_core::fs::paths::abs_norm_path::AbsNormPath;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::project::ProjectRoot;
use derive_more::Display;
use dupe::Dupe;
use gazebo::prelude::*;
use starlark_map::sorted_map::SortedMap;

use crate::legacy_configs::cells::BuckConfigBasedCells;
use crate::legacy_configs::parser::LegacyConfigParser;
use crate::legacy_configs::view::LegacyBuckConfigView;

#[derive(buck2_error::Error, Debug)]
enum ConfigCellResolutionError {
    #[error("Unable to resolve cell-relative path `{0}`")]
    UnableToResolveCellRelativePath(String),
}

/// A collection of configs, keyed by cell.
#[derive(Clone, Dupe, Debug, Allocative)]
pub struct LegacyBuckConfigs {
    data: Arc<SortedMap<CellName, LegacyBuckConfig>>,
}

impl LegacyBuckConfigs {
    pub fn new(data: HashMap<CellName, LegacyBuckConfig>) -> Self {
        let data = SortedMap::from_iter(data);
        Self {
            data: Arc::new(data),
        }
    }
}

#[derive(Clone, Debug, Allocative)]
struct ConfigFileLocation {
    source_file: Arc<ConfigFile>,
    line: usize,
}

#[derive(Debug, PartialEq, Eq)]
struct MainConfigFile {
    path: AbsNormPathBuf,

    /// if a main config file is in project or global
    owned_by_project: bool,
}

#[derive(Debug, Allocative)]
struct ConfigFile {
    id: String,
    include_source: Option<Location>,
}

#[derive(Clone, Dupe, Debug, Allocative)]
pub struct LegacyBuckConfig(Arc<ConfigData>);

#[derive(Debug, Allocative)]
struct ConfigData {
    values: SortedMap<String, LegacyBuckConfigSection>,
}

#[derive(Clone, Debug, Allocative)]
enum ResolvedValue {
    // A placeholder used before we do resolution.
    Unknown,
    // Indicates that there's no resolution required, the resolved value and raw value are the same.
    Literal,
    // The resolved value for non-literals.
    Resolved(String),
}

#[derive(Clone, Debug, Allocative)]
enum Location {
    File(ConfigFileLocation),
    CommandLineArgument,
}

impl Location {
    fn as_legacy_buck_config_location(&self) -> LegacyBuckConfigLocation {
        match self {
            Self::File(x) => LegacyBuckConfigLocation::File(&x.source_file.id, x.line),
            Self::CommandLineArgument => LegacyBuckConfigLocation::CommandLineArgument,
        }
    }
}

#[derive(Clone, Debug)]
struct ConfigArgumentPair {
    section: String,
    key: String,
    // None value means this config is unset.
    value: Option<String>,
    // Stores config's cell dir for resolving cell, when applicable.
    // cell name not used due to the many-to-one mapping of cell aliases to
    // actual cells, which complicates parsing.
    cell_path: Option<AbsNormPathBuf>,
}

// Represents a config section and key only, for example, `cxx.compiler`.
#[derive(Clone, Debug)]
pub struct ConfigSectionAndKey {
    //  TODO(scottcao): Add cell_path
    pub section: String,
    pub key: String,
}

/// Represents a configuration argument that can be passed
/// on the command line. For example, `--config foo.bar=val`
/// or `--config-file foo.bcfg`.
#[derive(Debug, Display)]
pub enum LegacyConfigCmdArg {
    /// A single config key-value pair (in `a.b=c` format).
    Flag(LegacyConfigCmdArgFlag),
    /// A file containing additional config values (in `.buckconfig` format).
    File(LegacyConfigCmdArgFile),
}

impl LegacyConfigCmdArg {
    pub fn flag(val: &str) -> anyhow::Result<Self> {
        let (cell, val) = match val.split_once("//") {
            Some((cell, val)) if !cell.contains('=') => (Some(cell.to_owned()), val),
            _ => (None, val),
        };

        let ParsedConfigArg {
            section,
            key,
            value,
        } = parse_config_arg(val)?;

        Ok(Self::Flag(LegacyConfigCmdArgFlag {
            cell,
            section,
            key,
            value,
        }))
    }

    pub fn file(val: &str) -> anyhow::Result<Self> {
        let (cell, val) = match val.split_once("//") {
            Some((cell, val)) => (Some(cell.to_owned()), val), // This should also reject =?
            _ => (None, val),
        };

        Ok(LegacyConfigCmdArg::File(LegacyConfigCmdArgFile {
            cell,
            path: val.to_owned(),
        }))
    }
}

#[derive(Debug)]
pub struct LegacyConfigCmdArgFlag {
    cell: Option<String>,
    section: String,
    key: String,
    value: Option<String>,
}

impl fmt::Display for LegacyConfigCmdArgFlag {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(cell) = &self.cell {
            write!(f, "{}//", cell)?;
        }
        write!(f, "{}.{}=", self.section, self.key)?;
        if let Some(value) = &self.value {
            write!(f, "{}", value)?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct LegacyConfigCmdArgFile {
    cell: Option<String>,
    path: String,
}

impl fmt::Display for LegacyConfigCmdArgFile {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(cell) = &self.cell {
            write!(f, "{}//", cell)?;
        }
        write!(f, "{}", self.path)
    }
}

/// Private representation of a processed config arg, namely after file
/// path resolution has been performed.
#[derive(Debug)]
enum ResolvedLegacyConfigArg {
    /// A single config key-value pair (in `a.b=c` format).
    Flag(ConfigArgumentPair),
    /// A file containing additional config values (in `.buckconfig` format).
    File(AbsNormPathBuf),
}

/// State required to perform resolution of cell-relative paths.
struct CellResolutionState<'a> {
    project_filesystem: &'a ProjectRoot,
    cwd: &'a AbsNormPath,
    cell_resolver: OnceCell<CellResolver>,
}

#[derive(buck2_error::Error, Debug)]
enum ConfigArgumentParseError {
    #[error("Could not find section separator (`.`) in pair `{0}`")]
    NoSectionDotSeparator(String),
    #[error("Could not find equals sign (`=`) in pair `{0}`")]
    NoEqualsSeparator(String),

    #[error("Expected key-value in format of `section.key=value` but only got `{0}`")]
    MissingData(String),

    #[error("Contains whitespace in key-value pair `{0}`")]
    WhitespaceInKeyOrValue(String),

    #[error("Specifying cells via cli config overrides is banned (`repositories.key=value`)")]
    CellOverrideViaCliConfig,
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

struct ParsedConfigArg {
    section: String,
    key: String,
    value: Option<String>,
}

/// Parses key-value pairs in the format `section.key=value` or `section.key=`.
fn parse_config_arg(raw_arg: &str) -> anyhow::Result<ParsedConfigArg> {
    let (raw_section_and_key, raw_value) = raw_arg
        .split_once('=')
        .ok_or_else(|| ConfigArgumentParseError::NoEqualsSeparator(raw_arg.to_owned()))?;
    let config_section_and_key = parse_config_section_and_key(raw_section_and_key, Some(raw_arg))?;

    let value = match raw_value {
        "" => None, // An empty string unsets this config.
        v => Some(v.to_owned()),
    };

    Ok(ParsedConfigArg {
        section: config_section_and_key.section,
        key: config_section_and_key.key,
        value,
    })
}

#[derive(Debug, Allocative)]
struct ConfigValue {
    raw_value: String,
    resolved_value: ResolvedValue,
    source: Location,
}

#[derive(Debug, Default, Allocative)]
pub struct LegacyBuckConfigSection {
    values: SortedMap<String, ConfigValue>,
}

impl ConfigValue {
    fn new_raw(source: ConfigFileLocation, value: String) -> Self {
        Self {
            raw_value: value,
            resolved_value: ResolvedValue::Unknown,
            source: Location::File(source),
        }
    }

    fn new_raw_arg(raw_value: String) -> Self {
        Self {
            raw_value,
            resolved_value: ResolvedValue::Unknown,
            source: Location::CommandLineArgument,
        }
    }

    fn raw_value(&self) -> &str {
        &self.raw_value
    }

    fn as_str(&self) -> &str {
        match &self.resolved_value {
            ResolvedValue::Literal => &self.raw_value,
            ResolvedValue::Resolved(v) => v,
            ResolvedValue::Unknown => {
                unreachable!("cannot call as_str() until all values are resolved")
            }
        }
    }
}

pub trait ConfigParserFileOps {
    fn read_file_lines(
        &mut self,
        path: &AbsNormPath,
    ) -> anyhow::Result<Box<dyn Iterator<Item = Result<String, std::io::Error>>>>;

    fn file_exists(&self, path: &AbsNormPath) -> bool;

    fn file_id(&self, path: &AbsNormPath) -> String {
        path.to_string()
    }
}

struct DefaultConfigParserFileOps {}

impl ConfigParserFileOps for DefaultConfigParserFileOps {
    fn read_file_lines(
        &mut self,
        path: &AbsNormPath,
    ) -> anyhow::Result<Box<dyn Iterator<Item = Result<String, std::io::Error>>>> {
        let f = std::fs::File::open(path).with_context(|| format!("Reading file `{:?}`", path))?;
        let file = std::io::BufReader::new(f);
        Ok(Box::new(file.lines()))
    }

    fn file_exists(&self, path: &AbsNormPath) -> bool {
        PathBuf::from(path.as_os_str()).exists()
    }
}

pub struct LegacyBuckConfigValue<'a> {
    value: &'a ConfigValue,
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
            Location::File(file) => LegacyBuckConfigLocation::File(&file.source_file.id, file.line),
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
                        &loc.source_file.id,
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

    fn resolve_config_flag_arg(
        flag_arg: &LegacyConfigCmdArgFlag,
        cell_resolution: Option<&CellResolutionState>,
        file_ops: &mut dyn ConfigParserFileOps,
    ) -> anyhow::Result<ConfigArgumentPair> {
        let cell_path = flag_arg
            .cell
            .as_ref()
            .map(|cell| {
                Self::resolve_config_file_arg(
                    &LegacyConfigCmdArgFile {
                        cell: Some(cell.clone()),
                        path: "".to_owned(),
                    },
                    cell_resolution,
                    file_ops,
                )
            })
            .transpose()?;

        Ok(ConfigArgumentPair {
            section: flag_arg.section.clone(),
            key: flag_arg.key.clone(),
            value: flag_arg.value.clone(),
            cell_path,
        })
    }

    fn resolve_config_file_arg(
        file_arg: &LegacyConfigCmdArgFile,
        cell_resolution: Option<&CellResolutionState>,
        file_ops: &mut dyn ConfigParserFileOps,
    ) -> anyhow::Result<AbsNormPathBuf> {
        if let Some(cell_alias) = &file_arg.cell {
            let cell_resolution_state = cell_resolution.ok_or_else(|| {
                anyhow::anyhow!(ConfigCellResolutionError::UnableToResolveCellRelativePath(
                    format!("{}//{}", cell_alias, file_arg.path)
                ))
            })?;
            if let Some(cell_resolver) = cell_resolution_state.cell_resolver.get() {
                return cell_resolver.resolve_cell_relative_path(
                    cell_alias,
                    &file_arg.path,
                    cell_resolution_state.project_filesystem,
                    cell_resolution_state.cwd,
                );
            } else {
                // Reading an immediate cell mapping is extremely fast as we just read a single
                // config file (which would already be in memory). There is another alternative,
                // we can take advantage of the fact that config files argument resolution happens
                // _after_ initial parsing of root. But this requires quite a bit more work to
                // access the unresolved parts and making further assumptions. The saving would
                // be < 1ms, so we take this approach here. It can easily be changed later.
                let cell_resolver = BuckConfigBasedCells::parse_immediate_config_with_file_ops(
                    cell_resolution_state.project_filesystem,
                    file_ops,
                )?
                .cell_resolver;
                let resolved_path = cell_resolver.resolve_cell_relative_path(
                    cell_alias,
                    &file_arg.path,
                    cell_resolution_state.project_filesystem,
                    cell_resolution_state.cwd,
                );
                let set_result = cell_resolution_state.cell_resolver.set(cell_resolver);
                assert!(set_result.is_ok());
                return resolved_path;
            }
        }

        // Cargo relative file paths are expanded before they make it into the daemon
        AbsNormPathBuf::try_from(file_arg.path.to_owned())
    }

    fn process_config_args(
        args: &[LegacyConfigCmdArg],
        cell_resolution: Option<&CellResolutionState>,
        file_ops: &mut dyn ConfigParserFileOps,
    ) -> anyhow::Result<Vec<ResolvedLegacyConfigArg>> {
        let resolved_args = args.map(|unprocessed_arg| match unprocessed_arg {
            LegacyConfigCmdArg::Flag(value) => {
                let resolved_flag =
                    Self::resolve_config_flag_arg(value, cell_resolution, file_ops)?;
                Ok(ResolvedLegacyConfigArg::Flag(resolved_flag))
            }
            LegacyConfigCmdArg::File(file) => {
                let resolved_path = Self::resolve_config_file_arg(file, cell_resolution, file_ops)?;
                Ok(ResolvedLegacyConfigArg::File(resolved_path))
            }
        });

        resolved_args.into_try_map(|x| x)
    }

    fn parse_with_file_ops_with_includes(
        main_config_files: &[MainConfigFile],
        file_ops: &mut dyn ConfigParserFileOps,
        config_args: &[ResolvedLegacyConfigArg],
        follow_includes: bool,
    ) -> anyhow::Result<Self> {
        let mut parser = LegacyConfigParser::new(file_ops);
        let mut cell_path = None;
        for main_config_file in main_config_files {
            parser.parse_file(&main_config_file.path, None, follow_includes)?;
            if main_config_file.owned_by_project {
                cell_path = match main_config_file.path.parent() {
                    Some(cell) => Some(cell),
                    None => panic!("Encountered invalid .buckconfig directory (no parent)"),
                };
            }
        }
        if cell_path.is_none() {
            panic!("Could not find cell path");
        }

        for config_arg in config_args {
            match config_arg {
                ResolvedLegacyConfigArg::Flag(config_value) => {
                    parser.apply_config_arg(config_value, cell_path.unwrap().to_buf())?
                }
                ResolvedLegacyConfigArg::File(file_path) => parser.parse_file(
                    file_path,
                    Some(Location::CommandLineArgument),
                    follow_includes,
                )?,
            };
        }

        parser.finish()
    }
}

// Options on how to exactly parse config files
struct BuckConfigParseOptions {
    // Defines whether includes are followed, this can significantly reduce parse time.
    follow_includes: bool,
}

fn push_all_files_from_a_directory(
    buckconfig_paths: &mut Vec<MainConfigFile>,
    folder_path: &AbsNormPath,
    owned_by_project: bool,
) -> anyhow::Result<()> {
    let readdir = match fs::read_dir(folder_path) {
        Ok(p) => p,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(()),
        Err(e) if e.kind() == std::io::ErrorKind::NotADirectory => {
            tracing::warn!(
                "Expected a directory of buckconfig files at: `{}`",
                folder_path
            );
            return Ok(());
        }
        Err(e) => {
            return Err(anyhow::Error::from(e)
                .context(format!("Error reading configs in `{}`", folder_path)));
        }
    };

    for entry in readdir {
        let entry_path = entry?.path();
        if entry_path.is_file() {
            buckconfig_paths.push(MainConfigFile {
                path: AbsNormPath::new(&entry_path)?.to_buf(),
                owned_by_project,
            });
        } else if entry_path.is_dir() {
            push_all_files_from_a_directory(
                buckconfig_paths,
                &AbsNormPathBuf::try_from(entry_path)?,
                owned_by_project,
            )?;
        } else {
            tracing::warn!(
                "Expected a directory of buckconfig files at `{}`, but this entry was not a file or directory: `{}`",
                folder_path,
                entry_path.display()
            );
        }
    }

    Ok(())
}

pub mod testing {
    use std::cmp::min;

    use super::*;

    pub fn parse(data: &[(&str, &str)], path: &str) -> anyhow::Result<LegacyBuckConfig> {
        parse_with_config_args(data, path, &[])
    }

    pub fn parse_with_config_args(
        data: &[(&str, &str)],
        path: &str,
        config_args: &[LegacyConfigCmdArg],
    ) -> anyhow::Result<LegacyBuckConfig> {
        let mut file_ops = TestConfigParserFileOps::new(data)?;
        #[cfg(not(windows))]
        let path = &AbsNormPathBuf::from(path.into())?;
        // Need to add some disk drive on Windows to make path absolute.
        #[cfg(windows)]
        let path = &AbsNormPathBuf::from(format!("C:{}", path))?;
        // This function is only used internally for tests, so it's to skip cell resolution
        // as we do not have a `ProjectFilesystem`. Either way, this will fail gracefully
        // if there's a cell-relative config arg, so this can updated appropriately.
        let processed_config_args =
            LegacyBuckConfig::process_config_args(config_args, None, &mut file_ops)?;
        LegacyBuckConfig::parse_with_file_ops_with_includes(
            &[MainConfigFile {
                path: path.to_buf(),
                owned_by_project: true,
            }],
            &mut file_ops,
            &processed_config_args,
            true,
        )
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

    impl ConfigParserFileOps for TestConfigParserFileOps {
        fn file_exists(&self, path: &AbsNormPath) -> bool {
            self.data.contains_key(path)
        }

        fn read_file_lines(
            &mut self,
            path: &AbsNormPath,
        ) -> anyhow::Result<
            Box<(dyn std::iter::Iterator<Item = Result<String, std::io::Error>> + 'static)>,
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
    }
}

#[cfg(test)]
mod tests {
    use buck2_core::fs::paths::abs_path::AbsPath;
    use indoc::indoc;
    use itertools::Itertools;

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
            LegacyConfigCmdArg::flag("apple.key=value1")?,
            LegacyConfigCmdArg::flag("apple.key=value2")?,
        ];
        let config =
            parse_with_config_args(&[("/config", indoc!(r#""#))], "/config", &config_args)?;
        assert_config_value(&config, "apple", "key", "value2");

        Ok(())
    }

    #[test]
    fn test_config_args_empty() -> anyhow::Result<()> {
        let config_args = vec![LegacyConfigCmdArg::flag("apple.key=")?];
        let config =
            parse_with_config_args(&[("/config", indoc!(r#""#))], "/config", &config_args)?;
        assert_config_value_is_empty(&config, "apple", "key");

        Ok(())
    }

    #[test]
    fn test_config_args_overwrite_config_file() -> anyhow::Result<()> {
        let config_args = vec![LegacyConfigCmdArg::flag("apple.key=value2")?];
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
    fn test_argument_pair() -> anyhow::Result<()> {
        // Valid Formats

        let normal_pair = parse_config_arg("apple.key=value")?;

        assert_eq!("apple", normal_pair.section);
        assert_eq!("key", normal_pair.key);
        assert_eq!(Some("value".to_owned()), normal_pair.value);

        let unset_pair = parse_config_arg("apple.key=")?;

        assert_eq!("apple", unset_pair.section);
        assert_eq!("key", unset_pair.key);
        assert_eq!(None, unset_pair.value);

        // Whitespace

        let section_leading_whitespace = parse_config_arg("  apple.key=value")?;
        assert_eq!("apple", section_leading_whitespace.section);
        assert_eq!("key", section_leading_whitespace.key);
        assert_eq!(Some("value".to_owned()), section_leading_whitespace.value);

        let pair_with_whitespace_in_key = parse_config_arg("apple. key=value");
        assert!(pair_with_whitespace_in_key.is_err());

        let pair_with_whitespace_in_value = parse_config_arg("apple.key= value with whitespace  ")?;
        assert_eq!("apple", pair_with_whitespace_in_value.section);
        assert_eq!("key", pair_with_whitespace_in_value.key);
        assert_eq!(
            Some(" value with whitespace  ".to_owned()),
            pair_with_whitespace_in_value.value
        );

        // Invalid Formats

        let pair_without_section = parse_config_arg("key=value");
        assert!(pair_without_section.is_err());

        let pair_without_equals = parse_config_arg("apple.keyvalue");
        assert!(pair_without_equals.is_err());

        let pair_without_section_or_equals = parse_config_arg("applekeyvalue");
        assert!(pair_without_section_or_equals.is_err());

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
            LegacyConfigCmdArg::flag("apple.key=value3")?,
            LegacyConfigCmdArg::file(file_arg)?,
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
        let config_args = vec![LegacyConfigCmdArg::flag("apple.key=foo//value1")?];
        let config =
            parse_with_config_args(&[("/config", indoc!(r#""#))], "/config", &config_args)?;
        assert_config_value(&config, "apple", "key", "foo//value1");

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

            push_all_files_from_a_directory(&mut v, dir, false)?;
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

            push_all_files_from_a_directory(&mut v, dir, false)?;
            assert_eq!(v, vec![]);

            Ok(())
        }

        #[test]
        fn non_existent_dir() -> anyhow::Result<()> {
            let mut v = vec![];
            let dir = tempfile::tempdir()?;
            let dir = dir.path().join("bad");
            let dir = AbsNormPath::new(&dir)?;

            push_all_files_from_a_directory(&mut v, dir, false)?;
            assert_eq!(v, vec![]);

            Ok(())
        }

        #[test]
        fn dir_in_dir() -> anyhow::Result<()> {
            let mut v = vec![];
            let dir = tempfile::tempdir()?;
            let dir = AbsPath::new(dir.path())?;
            fs_util::create_dir_all(dir.join("bad"))?;

            push_all_files_from_a_directory(&mut v, AbsNormPath::new(dir)?, false)?;
            assert_eq!(v, vec![]);

            Ok(())
        }

        #[test]
        fn file() -> anyhow::Result<()> {
            let mut v = vec![];
            let file = tempfile::NamedTempFile::new()?;
            let file = AbsNormPath::new(file.path())?;

            push_all_files_from_a_directory(&mut v, file, false)?;
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

            push_all_files_from_a_directory(&mut v, dir, false)?;
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

    #[test]
    fn test_arg_display() -> anyhow::Result<()> {
        assert_eq!(
            LegacyConfigCmdArg::flag("foo.bar=baz")?.to_string(),
            "foo.bar=baz"
        );
        assert_eq!(
            LegacyConfigCmdArg::flag("foo//bar.baz=")?.to_string(),
            "foo//bar.baz="
        );
        assert_eq!(LegacyConfigCmdArg::file("foo")?.to_string(), "foo");
        assert_eq!(
            LegacyConfigCmdArg::file("foo//bar")?.to_string(),
            "foo//bar"
        );
        Ok(())
    }
}
