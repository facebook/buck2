/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::BTreeMap;
use std::sync::Arc;

use anyhow::Context;
use buck2_core::fs::paths::abs_norm_path::AbsNormPath;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::paths::RelativePath;
use dupe::Dupe;
use futures::future::BoxFuture;
use futures::FutureExt;
use itertools::Itertools;
use once_cell::sync::Lazy;
use regex::Regex;
use starlark_map::sorted_map::SortedMap;

use crate::legacy_configs::configs::ConfigArgumentParseError;
use crate::legacy_configs::configs::ConfigData;
use crate::legacy_configs::configs::ConfigFileLocation;
use crate::legacy_configs::configs::ConfigFileLocationWithLine;
use crate::legacy_configs::configs::ConfigParserFileOps;
use crate::legacy_configs::configs::ConfigValue;
use crate::legacy_configs::configs::LegacyBuckConfig;
use crate::legacy_configs::configs::LegacyBuckConfigSection;
use crate::legacy_configs::configs::Location;
use crate::legacy_configs::configs::ResolvedConfigFlag;
use crate::legacy_configs::parser::resolver::ConfigResolver;

mod resolver;

#[derive(buck2_error::Error, Debug)]
enum ConfigError {
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
}

fn format_cycle(cycle: &[(String, String)]) -> String {
    cycle
        .iter()
        .map(|(section, key)| format!("`{}.{}`", section, key))
        .join(" -> ")
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

pub(crate) struct LegacyConfigParser {
    include_stack: Vec<ConfigFileLocationWithLine>,
    current_file: Option<Arc<ConfigFileLocation>>,
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

impl LegacyConfigParser {
    pub(crate) fn new() -> Self {
        LegacyConfigParser {
            values: BTreeMap::new(),
            include_stack: Vec::new(),
            current_file: None,
            current_section: Self::unspecified_section(),
        }
    }

    fn unspecified_section() -> (String, BTreeMap<String, ConfigValue>) {
        ("__unspecified__".to_owned(), BTreeMap::new())
    }

    pub(crate) async fn parse_file(
        &mut self,
        path: &AbsNormPath,
        source: Option<Location>,
        follow_includes: bool,
        file_ops: &mut dyn ConfigParserFileOps,
    ) -> anyhow::Result<()> {
        if file_ops.file_exists(path).await {
            self.start_file(path, source)?;
            self.parse_file_on_stack(path, follow_includes, file_ops)
                .await
                .with_context(|| format!("Error parsing buckconfig `{}`", path))?;
            self.finish_file();
        }
        Ok(())
    }

    fn push_file(&mut self, line: usize, path: &AbsNormPath) -> anyhow::Result<()> {
        let include_source = ConfigFileLocationWithLine {
                source_file: self.current_file.dupe().unwrap_or_else(|| panic!("push_file() called without any files on the include stack. top-level files should use start_file()")),
                line,
            };

        self.include_stack.push(include_source.clone());

        let source_file = Arc::new(ConfigFileLocation {
            path: path.to_string(),
            include_source: Some(Location::File(include_source)),
        });
        self.current_file = Some(source_file);
        Ok(())
    }

    fn start_file(&mut self, path: &AbsNormPath, source: Option<Location>) -> anyhow::Result<()> {
        let source_file = Arc::new(ConfigFileLocation {
            path: path.to_string(),
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

    fn location(&self, line_number: usize) -> ConfigFileLocationWithLine {
        ConfigFileLocationWithLine {
            source_file: self
                .current_file
                .dupe()
                .unwrap_or_else(|| panic!("tried to get location without any current file.")),
            // Our line numbers at this point are 0-based, but most people expect file line numbers to be 1-based.
            line: line_number + 1,
        }
    }

    pub(crate) fn apply_config_arg(
        &mut self,
        config_pair: &ResolvedConfigFlag,
        current_cell_path: AbsNormPathBuf,
    ) -> anyhow::Result<()> {
        for banned_section in ["repositories", "cells"] {
            if config_pair.section == banned_section {
                return Err(
                    ConfigArgumentParseError::CellOverrideViaCliConfig(banned_section).into(),
                );
            };
        }
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

    fn parse_file_on_stack<'a>(
        &'a mut self,
        path: &'a AbsNormPath,
        parse_includes: bool,
        file_ops: &'a mut dyn ConfigParserFileOps,
    ) -> BoxFuture<'a, anyhow::Result<()>> {
        async move {
            let parent = path
                .parent()
                .context("parent should give directory containing the config file")?;
            let file_lines = file_ops.read_file_lines(path).await?;
            self.parse_lines(parent, file_lines, parse_includes, file_ops)
                .await
        }
        .boxed()
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

    async fn parse_lines<T, E>(
        &mut self,
        dir: &AbsNormPath,
        lines: T,
        parse_includes: bool,
        file_ops: &mut dyn ConfigParserFileOps,
    ) -> anyhow::Result<()>
    where
        T: IntoIterator<Item = Result<String, E>>,
        E: std::error::Error + Send + Sync + 'static,
    {
        let lines: Vec<String> = lines.into_iter().collect::<Result<Vec<_>, _>>()?;

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

                    match (optional, file_ops.file_exists(&include_file).await) {
                        (_, true) => {
                            self.push_file(i, &include_file)?;
                            self.parse_file_on_stack(&include_file, parse_includes, file_ops)
                                .await?;
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

    pub(crate) fn finish(self) -> anyhow::Result<LegacyBuckConfig> {
        let LegacyConfigParser { values, .. } = self;

        let values = ConfigResolver::resolve(values)?;

        Ok(LegacyBuckConfig(Arc::new(ConfigData { values })))
    }
}
