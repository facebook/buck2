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

pub mod cells;
pub mod dice;
pub(crate) mod path;
pub mod view;

use std::collections::BTreeMap;
use std::collections::HashMap;
use std::fs;
use std::io::prelude::*;
use std::path::PathBuf;
use std::str::FromStr;
use std::sync::Arc;

use allocative::Allocative;
use anyhow::Context;
use buck2_core::cells::CellName;
use buck2_core::cells::CellResolver;
use buck2_core::collections::sorted_map::SortedMap;
use buck2_core::fs::paths::abs_norm_path::AbsNormPath;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::paths::*;
use buck2_core::fs::project::*;
use gazebo::eq_chain;
use gazebo::prelude::*;
use itertools::Itertools;
use once_cell::sync::Lazy;
use once_cell::unsync::OnceCell;
use regex::Regex;
use thiserror::Error;

use crate::legacy_configs::cells::BuckConfigBasedCells;
use crate::legacy_configs::view::LegacyBuckConfigView;
use crate::legacy_configs::view::LegacyBuckConfigsView;
use crate::target_aliases::BuckConfigTargetAliasResolver;

#[derive(Error, Debug)]
pub(crate) enum ConfigError {
    #[error("Expected line of the form `key = value` but key was empty. Line was `{0}`")]
    EmptyKey(String),
    #[error("Included file doesn't exist `{0}`")]
    MissingInclude(String),
    #[error("Improperly formatted section. Expected something of the form `[section]`, got {0}")]
    SectionMissingTrailingBracket(String),
    #[error("Improperly include directive path. Got {0}")]
    BadIncludePath(String),
    #[error(
        "Couldn't parse line. Expected include directive (`<file:/file.bcfg>`), section(`[some_section]`), or key assignment (`some_key = some_value`). Got `{0}`"
    )]
    InvalidLine(String),
    #[error("Detected cycles in buckconfig $(config) references: {}", format_cycle(.0))]
    ReferenceCycle(Vec<(String, String)>),
    #[error("Unable to resolve cell-relative path `{0}`")]
    UnableToResolveCellRelativePath(String),
    #[error("Invalid value for buckconfig `{section}.{key}`: conversion to {ty} failed")]
    ParseFailed {
        section: String,
        key: String,
        ty: &'static str,
    },
    #[error("Unknown cell: `{0}`")]
    UnknownCell(CellName),
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

    pub fn get<'a>(&'a self, cell_name: &'_ CellName) -> anyhow::Result<&'a LegacyBuckConfig> {
        self.data
            .get(cell_name)
            .ok_or_else(|| ConfigError::UnknownCell(cell_name.clone()).into())
    }

    pub fn iter(&self) -> impl Iterator<Item = (&CellName, &LegacyBuckConfig)> {
        self.data.iter()
    }

    pub fn target_alias_resolver(
        &self,
        cell_name: &CellName,
    ) -> anyhow::Result<BuckConfigTargetAliasResolver> {
        Ok(self.get(cell_name)?.target_alias_resolver())
    }

    pub(crate) fn compare(&self, other: &Self) -> bool {
        let x = &self.data;
        let y = &other.data;

        eq_chain! {
            x.len() == y.len(),
            x.iter().all(|(cell, config)| {
                y.get(cell).map_or(false, |y_config| y_config.compare(config))
            })
        }
    }
}

fn format_cycle(cycle: &[(String, String)]) -> String {
    cycle
        .iter()
        .map(|(section, key)| format!("`{}.{}`", section, key))
        .join(" -> ")
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

impl LegacyBuckConfig {
    /// configs are equal if the data they resolve in is equal, regardless of the origin of the config
    pub(crate) fn compare(&self, other: &Self) -> bool {
        eq_chain!(
            self.0.values.len() == other.0.values.len(),
            self.0.values.iter().all(|(section_name, section)| {
                other
                    .0
                    .values
                    .get(section_name)
                    .map_or(false, |other_sec| other_sec.compare(section))
            })
        )
    }
}

impl LegacyBuckConfigView for LegacyBuckConfig {
    fn get(&self, section: &str, key: &str) -> anyhow::Result<Option<Arc<str>>> {
        Ok(self.get(section, key).map(|v| v.to_owned().into()))
    }
}

impl LegacyBuckConfigsView for LegacyBuckConfigs {
    fn get(&self, cell_name: &CellName) -> anyhow::Result<&dyn LegacyBuckConfigView> {
        Ok(self.get(cell_name)?)
    }

    fn iter<'a>(
        &'a self,
    ) -> Box<dyn Iterator<Item = (&'a CellName, &'a dyn LegacyBuckConfigView)> + 'a> {
        box self
            .iter()
            .map(|(cell, config)| (cell, config as &dyn LegacyBuckConfigView))
    }
}

#[derive(Debug, Default)]
struct SectionBuilder {
    values: BTreeMap<String, ConfigValue>,
}

impl SectionBuilder {
    fn finish(self) -> LegacyBuckConfigSection {
        LegacyBuckConfigSection {
            values: SortedMap::from_iter(self.values),
        }
    }
}

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
#[derive(Debug)]
pub enum LegacyConfigCmdArg {
    /// A single config key-value pair (in `a.b=c` format).
    Flag(String),
    /// A file containing additional config values (in `.buckconfig` format).
    UnresolvedFile(String),
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

#[derive(Error, Debug)]
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

// Parses key-value pairs in the format `section.key=value`.
fn parse_config_argument_pair(
    raw_arg: &str,
    cell_path: Option<AbsNormPathBuf>,
) -> anyhow::Result<ConfigArgumentPair> {
    let (raw_section_and_key, raw_value) = raw_arg
        .split_once('=')
        .ok_or_else(|| ConfigArgumentParseError::NoEqualsSeparator(raw_arg.to_owned()))?;
    let config_section_and_key = parse_config_section_and_key(raw_section_and_key, Some(raw_arg))?;

    let value = match raw_value {
        "" => None, // An empty string unsets this config.
        v => Some(v.to_owned()),
    };

    let pair = ConfigArgumentPair {
        section: config_section_and_key.section,
        key: config_section_and_key.key,
        value,
        cell_path,
    };

    Ok(pair)
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

impl LegacyBuckConfigSection {
    /// configs are equal if the data they resolve in is equal, regardless of the origin of the config
    pub(crate) fn compare(&self, other: &Self) -> bool {
        eq_chain!(
            self.values.len() == other.values.len(),
            self.values.iter().all(|(name, value)| other
                .values
                .get(name)
                .map_or(false, |other_val| other_val.as_str() == value.as_str()))
        )
    }

    pub fn iter(&self) -> impl Iterator<Item = (&str, LegacyBuckConfigValue)> {
        self.values
            .iter()
            .map(move |(key, value)| (key.as_str(), LegacyBuckConfigValue { value }))
    }

    pub fn keys(&self) -> impl Iterator<Item = &String> {
        self.values.keys()
    }

    pub fn get(&self, key: &str) -> Option<LegacyBuckConfigValue> {
        self.values
            .get(key)
            .map(move |value| LegacyBuckConfigValue { value })
    }
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

struct LegacyConfigParser<'a> {
    file_ops: &'a dyn ConfigParserFileOps,
    include_stack: Vec<ConfigFileLocation>,
    current_file: Option<Arc<ConfigFile>>,
    values: BTreeMap<String, SectionBuilder>,
    current_section: (String, BTreeMap<String, ConfigValue>),
}

/// Matches file include directives. `optional` indicates whether it's an
/// optional include, `include` is the path.  Examples:
///   <file:/some/absolute>
///   <?file:/optional/absolute>
///   <file:relative/to/current>
///   <file:../../doesnt/need/to/be/forward/relative>
static FILE_INCLUDE: Lazy<Regex> =
    Lazy::new(|| Regex::new("<(?P<optional>\\?)?file:(?P<include>..*)>").unwrap());

pub trait ConfigParserFileOps {
    fn read_file_lines(
        &self,
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
        &self,
        path: &AbsNormPath,
    ) -> anyhow::Result<Box<dyn Iterator<Item = Result<String, std::io::Error>>>> {
        let f = std::fs::File::open(path).with_context(|| format!("Reading file `{:?}`", path))?;
        let file = std::io::BufReader::new(f);
        Ok(box file.lines())
    }

    fn file_exists(&self, path: &AbsNormPath) -> bool {
        PathBuf::from(path.as_os_str()).exists()
    }
}

impl<'a> LegacyConfigParser<'a> {
    fn new(file_ops: &'a dyn ConfigParserFileOps) -> Self {
        LegacyConfigParser {
            values: BTreeMap::new(),
            include_stack: Vec::new(),
            current_file: None,
            current_section: Self::unspecified_section(),
            file_ops,
        }
    }

    fn unspecified_section() -> (String, BTreeMap<String, ConfigValue>) {
        ("__unspecified__".to_owned(), BTreeMap::new())
    }

    fn parse_file(
        &mut self,
        path: &AbsNormPath,
        source: Option<Location>,
        follow_includes: bool,
    ) -> anyhow::Result<()> {
        self.start_file(path, source)?;
        self.parse_file_on_stack(path, follow_includes)
            .with_context(|| format!("When parsing buckconfig `{}`", path))?;
        self.finish_file();
        Ok(())
    }

    fn push_file(&mut self, line: usize, path: &AbsNormPath) -> anyhow::Result<()> {
        let include_source = ConfigFileLocation {
            source_file: self.current_file.dupe().unwrap_or_else(|| panic!("push_file() called without any files on the include stack. top-level files should use start_file()")),
            line,
        };

        self.include_stack.push(include_source.clone());

        let source_file = Arc::new(ConfigFile {
            id: self.file_ops.file_id(path),
            include_source: Some(Location::File(include_source)),
        });
        self.current_file = Some(source_file);
        Ok(())
    }

    fn start_file(&mut self, path: &AbsNormPath, source: Option<Location>) -> anyhow::Result<()> {
        let source_file = Arc::new(ConfigFile {
            id: self.file_ops.file_id(path),
            include_source: source,
        });
        self.current_file = Some(source_file);
        Ok(())
    }

    fn pop_file(&mut self) {
        match self.include_stack.pop() {
            Some(loc) => {
                self.current_file = Some(loc.source_file);
            }
            None => {
                self.current_file = None;
            }
        }
    }

    fn location(&self, line_number: usize) -> ConfigFileLocation {
        ConfigFileLocation {
            source_file: self
                .current_file
                .dupe()
                .unwrap_or_else(|| panic!("tried to get location without any current file.")),
            // Our line numbers at this point are 0-based, but most people expect file line numbers to be 1-based.
            line: line_number + 1,
        }
    }

    fn apply_config_arg(
        &mut self,
        config_pair: &ConfigArgumentPair,
        current_cell_path: AbsNormPathBuf,
    ) -> anyhow::Result<()> {
        if config_pair.section == "repositories" {
            return Err(anyhow::anyhow!(
                ConfigArgumentParseError::CellOverrideViaCliConfig
            ));
        };
        let pair = config_pair.to_owned();
        let cell_matches = pair.cell_path == Some(current_cell_path) || pair.cell_path.is_none();
        if cell_matches {
            let config_section = self
                .values
                .entry(pair.section)
                .or_insert_with(SectionBuilder::default);

            match pair.value {
                Some(raw_value) => {
                    let config_value = ConfigValue::new_raw_arg(raw_value);
                    config_section.values.insert(pair.key, config_value)
                }
                None => config_section.values.remove(&pair.key),
            };
        }
        Ok(())
    }

    fn parse_file_on_stack(
        &mut self,
        path: &AbsNormPath,
        parse_includes: bool,
    ) -> anyhow::Result<()> {
        let parent = path
            .parent()
            .context("parent should give directory containing the config file")?;
        self.parse_lines(parent, self.file_ops.read_file_lines(path)?, parse_includes)
    }

    fn strip_line_comment(line: &str) -> &str {
        match line.split_once(" #") {
            Some((before, _)) => before,
            None => line,
        }
    }

    fn parse_section_marker(line: &str) -> anyhow::Result<Option<&str>> {
        // We allow trailing comment markers at the end of sections, since otherwise
        // using oss-enable/oss-disable is super tricky
        match line.strip_prefix('[') {
            Some(remaining) => match Self::strip_line_comment(remaining).strip_suffix(']') {
                None => Err(ConfigError::SectionMissingTrailingBracket(line.to_owned()).into()),
                Some(section) => Ok(Some(section)),
            },
            None => Ok(None),
        }
    }

    fn parse_lines<T, E>(
        &mut self,
        dir: &AbsNormPath,
        lines: T,
        parse_includes: bool,
    ) -> anyhow::Result<()>
    where
        T: Iterator<Item = Result<String, E>>,
        E: std::error::Error + Send + Sync + 'static,
    {
        let lines: Vec<String> = lines.collect::<Result<Vec<_>, _>>()?;

        let lines = lines
            .into_iter()
            // Trim leading/trailing whitespace.
            .map(|line| line.trim().to_owned())
            // add line numbers
            .enumerate()
            // Coalesce escaped newlines.
            .coalesce(|(i, mut prev), (j, next)| {
                if prev.ends_with('\\') {
                    prev.truncate(prev.len() - 1);
                    prev.push_str(&next);
                    Ok((i, prev))
                } else {
                    Err(((i, prev), (j, next)))
                }
            })
            // Remove commented lines.
            // This needs to come after the coalesce in case someone has an empty line after an escaped newline
            // Remove empty lines and comment lines (support both '#' and ';' for comment lines)
            .filter(|(_, l)| !l.is_empty() && !l.starts_with('#') && !l.starts_with(';'));

        for (i, line) in lines {
            if let Some(section) = Self::parse_section_marker(&line)? {
                // Start the new section, grabbing the recorded values for the previous
                // section.
                let section = std::mem::replace(
                    &mut self.current_section,
                    (section.to_owned(), BTreeMap::new()),
                );
                self.commit_section(section)
            } else if let Some((key, val)) = line.split_once('=') {
                let key = key.trim();
                let val = val.trim();
                if key.is_empty() {
                    return Err(anyhow::anyhow!(ConfigError::EmptyKey(line.to_owned())));
                }
                self.current_section.1.insert(
                    key.to_owned(),
                    ConfigValue::new_raw(self.location(i), val.to_owned()),
                );
            } else if let Some(m) = FILE_INCLUDE.captures(&line) {
                if parse_includes {
                    let include = m.name("include").unwrap().as_str();
                    let include = if cfg!(windows) && include.contains(':') {
                        // On Windows absolute includes look like /C:/foo/bar.
                        // For compatibility with Python parser we need to support this.
                        include.trim_start_matches('/')
                    } else {
                        include
                    };
                    let optional = m.name("optional").is_some();
                    let include_file = if let Ok(absolute) = AbsNormPath::new(include) {
                        absolute.to_owned()
                    } else {
                        let relative = RelativePath::new(include);
                        match dir.join_normalized(relative) {
                            Ok(d) => d,
                            Err(_) => {
                                return Err(anyhow::anyhow!(ConfigError::BadIncludePath(
                                    include.to_owned()
                                )));
                            }
                        }
                    };

                    match (optional, self.file_ops.file_exists(&include_file)) {
                        (_, true) => {
                            self.push_file(i, &include_file)?;
                            self.parse_file_on_stack(&include_file, parse_includes)?;
                            self.pop_file();
                        }
                        (false, false) => {
                            return Err(anyhow::anyhow!(ConfigError::MissingInclude(
                                include.to_owned()
                            )));
                        }
                        (true, _) => {
                            // optional case, missing is okay.
                        }
                    }
                }
            } else {
                return Err(anyhow::anyhow!(ConfigError::InvalidLine(line.to_owned())));
            }
        }
        Ok(())
    }

    fn commit_section(&mut self, section: (String, BTreeMap<String, ConfigValue>)) {
        let (section, values) = section;
        // Commit the previous section.
        let committed = self
            .values
            .entry(section)
            .or_insert_with(SectionBuilder::default);
        values.into_iter().for_each(|(k, v)| {
            committed.values.insert(k, v);
        });
    }

    fn finish_file(&mut self) {
        self.pop_file();

        let section = std::mem::replace(&mut self.current_section, Self::unspecified_section());
        self.commit_section(section);
    }

    fn finish(self) -> anyhow::Result<LegacyBuckConfig> {
        let LegacyConfigParser { values, .. } = self;

        let values = ConfigResolver::resolve(values)?;

        Ok(LegacyBuckConfig(Arc::new(ConfigData { values })))
    }
}

// Since we can't change other entries in values while we iterate over the configuration, we use
// ResolvedItems to store information about recursive resolutions and the current resolution stack.
struct ResolvedItems(
    // Maintains map of items that are resolved in the process of resolving requested items.
    BTreeMap<String, BTreeMap<String, ResolveState>>,
    // Maintains the resolution stack to provide error messages when a cycle is detected.
    Vec<(String, String)>,
);

enum ResolveState {
    Resolving,
    Done(String),
}

impl ResolvedItems {
    fn start_resolving(&mut self, section: &str, key: &str) -> anyhow::Result<()> {
        let section_values = match self.0.get_mut(section) {
            Some(v) => v,
            None => {
                self.0.insert(section.to_owned(), BTreeMap::new());
                self.0.get_mut(section).unwrap()
            }
        };

        if section_values
            .insert(key.to_owned(), ResolveState::Resolving)
            .is_some()
        {
            return Err(anyhow::anyhow!(self.cycle_error(section, key)));
        }

        self.1.push((section.to_owned(), key.to_owned()));

        Ok(())
    }

    fn finish_resolving(&mut self, section: &str, key: &str, value: String) {
        let entry = self.0.get_mut(section).unwrap().get_mut(key).unwrap();
        *entry = ResolveState::Done(value);
        self.1.pop();
    }

    fn cycle_error(&self, section: &str, key: &str) -> ConfigError {
        let mut iter = self.1.iter();
        for v in &mut iter {
            if v.0 == section && v.1 == key {
                break;
            }
        }

        let mut cycle = vec![(section.to_owned(), key.to_owned())];
        cycle.extend(iter.cloned());
        cycle.push((section.to_owned(), key.to_owned()));

        ConfigError::ReferenceCycle(cycle)
    }

    fn get(&self, section: &str, key: &str) -> Option<&String> {
        self.0
            .get(section)
            .and_then(|e| e.get(key))
            .and_then(|e| match e {
                ResolveState::Resolving => None,
                ResolveState::Done(v) => Some(v),
            })
    }

    fn drain_to(self, value: &mut BTreeMap<String, SectionBuilder>) -> anyhow::Result<()> {
        assert!(self.1.is_empty(), "All values should have been resolved.");
        for (section, items) in self.0.into_iter() {
            let result_section = value.get_mut(&section).unwrap_or_else(
                || panic!("Shouldn't have a resolved value for something that doesn't appear in the base config"));
            for (key, value) in items.into_iter() {
                match value {
                    ResolveState::Resolving => {
                        unreachable!("All values should have been resolved.");
                    }
                    ResolveState::Done(v) => {
                        result_section.values.get_mut(&key).unwrap().resolved_value =
                            ResolvedValue::Resolved(v);
                    }
                }
            }
        }

        Ok(())
    }
}

struct ConfigResolver {
    values: BTreeMap<String, SectionBuilder>,
}

impl ConfigResolver {
    #[allow(clippy::from_iter_instead_of_collect)]
    fn resolve(
        values: BTreeMap<String, SectionBuilder>,
    ) -> anyhow::Result<SortedMap<String, LegacyBuckConfigSection>> {
        let mut resolver = Self { values };
        resolver.resolve_all()?;
        Ok(SortedMap::from_iter(
            resolver.values.into_iter().map(|(k, v)| (k, v.finish())),
        ))
    }

    fn resolve_all(&mut self) -> anyhow::Result<()> {
        // First, identify all the values that need to be resolved and mark all the others as literals.
        let mut to_resolve = Vec::new();
        for (section_name, section) in &mut self.values {
            for (key, value) in &mut section.values {
                // if it's been resolved already, move the resolved value into values.
                if Self::regex().is_match(value.raw_value()) {
                    to_resolve.push((section_name.to_owned(), key.to_owned()));
                } else {
                    value.resolved_value = ResolvedValue::Literal;
                }
            }
        }

        // Now, resolve all the items.
        for (section, key) in to_resolve {
            let mut resolved_items = ResolvedItems(BTreeMap::new(), Vec::new());
            self.resolve_item(&mut resolved_items, &section, &key)?;
            resolved_items.drain_to(&mut self.values)?;
        }
        Ok(())
    }

    fn regex() -> &'static Regex {
        static RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"\$\(config ([^)]*)\)").unwrap());
        &RE
    }

    fn resolve_item<'a>(
        &'a self,
        resolved_items: &'a mut ResolvedItems,
        section: &str,
        key: &str,
    ) -> anyhow::Result<&'a str> {
        let raw_value = match self.values.get(section).and_then(|e| e.values.get(key)) {
            None => return Ok(""),
            Some(v) => match &v.resolved_value {
                ResolvedValue::Unknown => v.raw_value(),
                ResolvedValue::Literal => {
                    return Ok(v.raw_value());
                }
                ResolvedValue::Resolved(v) => {
                    return Ok(v);
                }
            },
        };

        if resolved_items.get(section, key).is_none() {
            resolved_items.start_resolving(section, key)?;
            let v = self.do_resolve(resolved_items, raw_value)?;
            resolved_items.finish_resolving(section, key, v);
        }

        Ok(resolved_items.get(section, key).unwrap())
    }

    fn do_resolve(
        &self,
        resolved_items: &mut ResolvedItems,
        raw_value: &str,
    ) -> anyhow::Result<String> {
        let mut resolved = String::new();
        let mut last = 0;

        let re = Self::regex();

        // TODO(cjhopman): Should add support for escaping the call, I guess.
        for capture in re.captures_iter(raw_value) {
            let m = capture.get(0).unwrap();

            resolved.push_str(&raw_value[last..m.start()]);
            last = m.end();

            let captures = re.captures(m.as_str()).unwrap();

            let config_key = captures.get(1).unwrap().as_str();

            let config_section_and_key = parse_config_section_and_key(config_key, None)?;

            resolved.push_str(self.resolve_item(
                resolved_items,
                &config_section_and_key.section,
                &config_section_and_key.key,
            )?);
        }

        resolved.push_str(&raw_value[last..]);
        Ok(resolved)
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

    pub fn target_alias_resolver(&self) -> BuckConfigTargetAliasResolver {
        BuckConfigTargetAliasResolver::new(self.dupe())
    }

    pub fn parse_with_file_ops(
        path: &AbsNormPath,
        file_ops: &dyn ConfigParserFileOps,
        config_args: &[LegacyConfigCmdArg],
    ) -> anyhow::Result<Self> {
        // This function is only used internally for tests, so it's to skip cell resolution
        // as we do not have a `ProjectFilesystem`. Either way, this will fail gracefully
        // if there's a cell-relative config arg, so this can updated appropriately.
        let processed_config_args =
            LegacyBuckConfig::process_config_args(config_args, None, file_ops)?;
        Self::parse_with_file_ops_with_includes(
            &[MainConfigFile {
                path: path.to_buf(),
                owned_by_project: true,
            }],
            file_ops,
            &processed_config_args,
            true,
        )
    }

    fn resolve_config_flag_arg(
        flag_arg: &str,
        cell_resolution: Option<&CellResolutionState>,
        file_ops: &dyn ConfigParserFileOps,
    ) -> anyhow::Result<ConfigArgumentPair> {
        let cell_path: Option<AbsNormPathBuf>;
        let raw_config: &str;
        match flag_arg.split_once("//") {
            Some((cell, config)) if !cell.contains('=') => {
                cell_path = Some(Self::resolve_config_file_arg(
                    &format!("{}//", cell),
                    cell_resolution,
                    file_ops,
                )?);
                raw_config = config;
            }
            _ => {
                cell_path = None;
                raw_config = flag_arg;
            }
        }

        parse_config_argument_pair(raw_config, cell_path)
    }

    fn resolve_config_file_arg(
        file_arg: &str,
        cell_resolution: Option<&CellResolutionState>,
        file_ops: &dyn ConfigParserFileOps,
    ) -> anyhow::Result<AbsNormPathBuf> {
        if let Some((cell_alias, cell_relative_path)) = file_arg.split_once("//") {
            let cell_resolution_state = cell_resolution.ok_or_else(|| {
                anyhow::anyhow!(ConfigError::UnableToResolveCellRelativePath(
                    file_arg.to_owned()
                ))
            })?;
            if let Some(cell_resolver) = cell_resolution_state.cell_resolver.get() {
                return cell_resolver.resolve_cell_relative_path(
                    cell_alias,
                    cell_relative_path,
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
                let cell_resolver =
                    BuckConfigBasedCells::parse_immediate_cell_mapping_with_file_ops(
                        cell_resolution_state.project_filesystem,
                        file_ops,
                    )?;
                let resolved_path = cell_resolver.resolve_cell_relative_path(
                    cell_alias,
                    cell_relative_path,
                    cell_resolution_state.project_filesystem,
                    cell_resolution_state.cwd,
                );
                let set_result = cell_resolution_state.cell_resolver.set(cell_resolver);
                assert!(set_result.is_ok());
                return resolved_path;
            }
        }

        // Cargo relative file paths are expanded before they make it into the daemon
        AbsNormPathBuf::try_from(file_arg.to_owned())
    }

    fn process_config_args(
        args: &[LegacyConfigCmdArg],
        cell_resolution: Option<&CellResolutionState>,
        file_ops: &dyn ConfigParserFileOps,
    ) -> anyhow::Result<Vec<ResolvedLegacyConfigArg>> {
        let resolved_args = args.map(|unprocessed_arg| match unprocessed_arg {
            LegacyConfigCmdArg::Flag(value) => {
                let resolved_flag =
                    Self::resolve_config_flag_arg(value, cell_resolution, file_ops)?;
                Ok(ResolvedLegacyConfigArg::Flag(resolved_flag))
            }
            LegacyConfigCmdArg::UnresolvedFile(file) => {
                let resolved_path = Self::resolve_config_file_arg(file, cell_resolution, file_ops)?;
                Ok(ResolvedLegacyConfigArg::File(resolved_path))
            }
        });

        resolved_args.into_try_map(|x| x)
    }

    fn parse_with_file_ops_with_includes(
        main_config_files: &[MainConfigFile],
        file_ops: &dyn ConfigParserFileOps,
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

    pub fn get(&self, section: &str, key: &str) -> Option<&str> {
        self.0
            .values
            .get(section)
            .and_then(|s| s.values.get(key))
            .map(|s| s.as_str())
    }

    /// Iterate all entries.
    pub fn iter(&self) -> impl Iterator<Item = (&str, impl IntoIterator<Item = (&str, &str)>)> {
        self.0.values.iter().map(|(section, section_values)| {
            (
                section.as_str(),
                section_values
                    .values
                    .iter()
                    .map(|(key, value)| (key.as_str(), value.as_str())),
            )
        })
    }

    pub fn parse_impl<T: FromStr>(section: &str, key: &str, value: &str) -> anyhow::Result<T>
    where
        anyhow::Error: From<<T as FromStr>::Err>,
    {
        value
            .parse()
            .map_err(anyhow::Error::from)
            .with_context(|| ConfigError::ParseFailed {
                section: section.to_owned(),
                key: key.to_owned(),
                ty: std::any::type_name::<T>(),
            })
    }

    pub fn parse<T: FromStr>(&self, section: &str, key: &str) -> anyhow::Result<Option<T>>
    where
        anyhow::Error: From<<T as FromStr>::Err>,
    {
        self.get(section, key)
            .map(|s| Self::parse_impl(section, key, s))
            .transpose()
    }

    pub fn sections(&self) -> impl Iterator<Item = &String> {
        self.0.values.keys()
    }

    pub fn all_sections(&self) -> impl Iterator<Item = (&String, &LegacyBuckConfigSection)> + '_ {
        self.0.values.iter()
    }

    pub fn get_section(&self, section: &str) -> Option<&LegacyBuckConfigSection> {
        self.0.values.get(section)
    }
}

// Options on how to exactly parse config files
struct BuckConfigParseOptions {
    // Defines whether includes are followed, this can significantly reduce parse time.
    follow_includes: bool,
    // Defines whether the configs for any cells found in [repositories] should be parsed.
    parse_cells: bool,
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
        } else {
            tracing::warn!(
                "Expected a directory of buckconfig files at `{}`, but this entry was not a file: `{}`",
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

    pub fn legacy_buck_config_from_entries<'a>(
        entries: impl IntoIterator<Item = (&'a str, &'a str, &'a str)>,
    ) -> anyhow::Result<LegacyBuckConfig> {
        let mut values: BTreeMap<String, SectionBuilder> = BTreeMap::new();
        for (section, key, value) in entries {
            values
                .entry(section.to_owned())
                .or_default()
                .values
                .insert(key.to_owned(), ConfigValue::new_raw_arg(value.to_owned()));
        }
        let values = ConfigResolver::resolve(values)?;
        Ok(LegacyBuckConfig(Arc::new(ConfigData { values })))
    }

    pub fn parse(data: &[(&str, &str)], path: &str) -> anyhow::Result<LegacyBuckConfig> {
        parse_with_config_args(data, path, &[])
    }

    pub fn parse_with_config_args(
        data: &[(&str, &str)],
        path: &str,
        config_args: &[LegacyConfigCmdArg],
    ) -> anyhow::Result<LegacyBuckConfig> {
        let file_ops = TestConfigParserFileOps::new(data)?;
        #[cfg(not(windows))]
        let path = &AbsNormPathBuf::from(path.into())?;
        // Need to add some disk drive on Windows to make path absolute.
        #[cfg(windows)]
        let path = &AbsNormPathBuf::from(format!("C:{}", path))?;
        LegacyBuckConfig::parse_with_file_ops(path, &file_ops, config_args)
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
            &self,
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
            impl Read for StringReader {
                fn read(&mut self, buf: &mut [u8]) -> Result<usize, std::io::Error> {
                    let remaining = self.0.len() - self.1;
                    let to_return = min(remaining, buf.len());
                    buf[..to_return].clone_from_slice(&self.0[self.1..self.1 + to_return]);
                    self.1 += to_return;
                    Ok(to_return)
                }
            }
            let file = std::io::BufReader::new(StringReader(content.into_bytes(), 0));
            Ok(box file.lines())
        }
    }
}

#[cfg(test)]
mod tests {
    use indoc::indoc;
    use itertools::Itertools;

    use super::testing::*;
    use super::*;

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

        assert_eq!(None, config.get("section", "missing"));
        assert_eq!(None, config.get("missing", "int"));
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
            LegacyConfigCmdArg::Flag("apple.key=value1".to_owned()),
            LegacyConfigCmdArg::Flag("apple.key=value2".to_owned()),
        ];
        let config =
            parse_with_config_args(&[("/config", indoc!(r#""#))], "/config", &config_args)?;
        assert_config_value(&config, "apple", "key", "value2");

        Ok(())
    }

    #[test]
    fn test_config_args_empty() -> anyhow::Result<()> {
        let config_args = vec![LegacyConfigCmdArg::Flag("apple.key=".to_owned())];
        let config =
            parse_with_config_args(&[("/config", indoc!(r#""#))], "/config", &config_args)?;
        assert_config_value_is_empty(&config, "apple", "key");

        Ok(())
    }

    #[test]
    fn test_config_args_overwrite_config_file() -> anyhow::Result<()> {
        let config_args = vec![LegacyConfigCmdArg::Flag("apple.key=value2".to_owned())];
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

        let normal_pair = parse_config_argument_pair("apple.key=value", None)?;

        assert_eq!("apple", normal_pair.section);
        assert_eq!("key", normal_pair.key);
        assert_eq!(Some("value".to_owned()), normal_pair.value);

        let unset_pair = parse_config_argument_pair("apple.key=", None)?;

        assert_eq!("apple", unset_pair.section);
        assert_eq!("key", unset_pair.key);
        assert_eq!(None, unset_pair.value);

        // Whitespace

        let section_leading_whitespace = parse_config_argument_pair("  apple.key=value", None)?;
        assert_eq!("apple", section_leading_whitespace.section);
        assert_eq!("key", section_leading_whitespace.key);
        assert_eq!(Some("value".to_owned()), section_leading_whitespace.value);

        let pair_with_whitespace_in_key = parse_config_argument_pair("apple. key=value", None);
        assert!(pair_with_whitespace_in_key.is_err());

        let pair_with_whitespace_in_value =
            parse_config_argument_pair("apple.key= value with whitespace  ", None)?;
        assert_eq!("apple", pair_with_whitespace_in_value.section);
        assert_eq!("key", pair_with_whitespace_in_value.key);
        assert_eq!(
            Some(" value with whitespace  ".to_owned()),
            pair_with_whitespace_in_value.value
        );

        // Invalid Formats

        let pair_without_section = parse_config_argument_pair("key=value", None);
        assert!(pair_without_section.is_err());

        let pair_without_equals = parse_config_argument_pair("apple.keyvalue", None);
        assert!(pair_without_equals.is_err());

        let pair_without_section_or_equals = parse_config_argument_pair("applekeyvalue", None);
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
        let file_arg = "/cli-config".to_owned();
        #[cfg(windows)]
        let file_arg = "C:/cli-config".to_owned();
        let config_args = vec![
            LegacyConfigCmdArg::Flag("apple.key=value3".to_owned()),
            LegacyConfigCmdArg::UnresolvedFile(file_arg),
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
        let config_args = vec![LegacyConfigCmdArg::Flag("apple.key=foo//value1".to_owned())];
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
            let file = dir.path().join("foo");
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
            fs_util::create_dir_all(&dir.path().join("bad"))?;
            let dir = AbsNormPath::new(&dir)?;

            push_all_files_from_a_directory(&mut v, dir, false)?;
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
    }
}
