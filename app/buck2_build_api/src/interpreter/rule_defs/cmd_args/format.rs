/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::borrow::Cow;
use std::sync::Arc;

use buck2_artifact::artifact::artifact_type::Artifact;
use buck2_core::cells::cell_path::CellPathRef;
use buck2_core::content_hash::ContentBasedPathHash;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_error::BuckErrorContext;
use buck2_execute::artifact::artifact_dyn::ArtifactDyn;
use buck2_execute::artifact::fs::ExecutorFs;
use buck2_interpreter::types::regex::StarlarkBuckRegex;
use dupe::Dupe;
use either::Either;
use regex::Regex;
use starlark::values::ValueOfUnchecked;

use crate::interpreter::rule_defs::cmd_args::CommandLineLocation;
use crate::interpreter::rule_defs::cmd_args::options::CommandLineOptionsRef;
use crate::interpreter::rule_defs::cmd_args::options::OptionsReplacementsRef;
use crate::interpreter::rule_defs::cmd_args::options::QuoteStyle;
use crate::interpreter::rule_defs::cmd_args::options::RelativeOrigin;
use crate::interpreter::rule_defs::cmd_args::options::RelativeOriginArtifactPathMapper;
use crate::interpreter::rule_defs::cmd_args::regex::CmdArgsRegex;
use crate::interpreter::rule_defs::cmd_args::shlex_quote::shlex_quote;
use crate::interpreter::rule_defs::cmd_args::traits::ArtifactPathMapper;
use crate::interpreter::rule_defs::cmd_args::traits::CommandLineBuilder;

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
enum CommandLineOptionsError {
    #[error("Path `{0}` does not have {1} parent(s)")]
    TooManyParentCalls(String, u32),
    #[error("Command line args in this position do not support write-to-file macros")]
    WriteToFileMacroNotSupported,
}

pub(crate) fn compute_relative_to_path<'v>(
    t: ValueOfUnchecked<'v, RelativeOrigin<'v>>,
    parents: u32,
    fs: &ExecutorFs<'_>,
    artifact_path_mapping: &'_ dyn ArtifactPathMapper,
) -> buck2_error::Result<ProjectRelativePathBuf> {
    let origin = t
        .unpack()
        .internal_error("Must be a valid RelativeOrigin as this was checked in the setter")?;
    let mut path = match origin {
        RelativeOrigin::OutputArtifact(artifact) => {
            let value = match artifact.unpack() {
                Either::Right(value) => value,
                Either::Left(_) => {
                    return Err(buck2_error::internal_error!(
                        "Non-frozen output artifacts can't be added to CLIs"
                    ));
                }
            };
            value
                .inner()
                .artifact
                .resolve_path(fs.fs(), Some(&ContentBasedPathHash::for_output_artifact()))?
        }
        RelativeOrigin::Artifact(artifact) => {
            let artifact = artifact.get_bound_artifact()?;
            let mapper = RelativeOriginArtifactPathMapper::new(artifact_path_mapping);
            artifact.resolve_path(fs.fs(), mapper.get(&artifact))?
        }
        RelativeOrigin::CellRoot(cell_root) => fs.fs().resolve_cell_path(cell_root.cell_path())?,
        RelativeOrigin::ProjectRoot(_) => ProjectRelativePath::empty().to_owned(),
    };

    let path_str = path.to_string();
    for _ in 0..parents {
        if !path.pop() {
            return Err(CommandLineOptionsError::TooManyParentCalls(path_str, parents).into());
        }
    }
    Ok(path)
}

struct ArtifactOptions<'v> {
    relative_to: Option<Arc<ProjectRelativePathBuf>>,
    absolute_prefix: Option<&'v str>,
    absolute_suffix: Option<&'v str>,
    parent: u32,
}

const DEFAULT_ARTIFACT_OPTIONS: ArtifactOptions<'_> = ArtifactOptions {
    relative_to: None,
    absolute_prefix: None,
    absolute_suffix: None,
    parent: 0,
};

enum FormatRegex<'v> {
    Starlark(&'v StarlarkBuckRegex),
    Owned(regex::Regex),
}

impl<'v> FormatRegex<'v> {
    fn make(o: OptionsReplacementsRef<'v, '_>) -> Vec<(FormatRegex<'v>, &'v str)> {
        o.iter()
            .map(|(pattern, replacement)| {
                let regex = match pattern {
                    // We checked that regex is valid in replace_regex(), so unwrap is safe.
                    CmdArgsRegex::Str(s) => FormatRegex::Owned(Regex::new(s.as_str()).unwrap()),
                    CmdArgsRegex::Regex(r) => FormatRegex::Starlark(r.as_ref()),
                };
                (regex, replacement.as_str())
            })
            .collect()
    }
}

enum DelimiterJoinState {
    Unstarted,
    Started(String),
}

impl DelimiterJoinState {
    fn push(&mut self, s: String, delim: &str) {
        match self {
            DelimiterJoinState::Unstarted => {
                *self = DelimiterJoinState::Started(s);
            }
            DelimiterJoinState::Started(cur) => {
                cur.reserve(delim.len() + s.len());
                cur.push_str(delim);
                cur.push_str(s.as_str());
            }
        }
    }

    fn finish(self) -> String {
        match self {
            Self::Unstarted => String::new(),
            Self::Started(s) => s,
        }
    }
}

#[derive(Default)]
struct StringOptions<'v> {
    /// The options are generally immutable, but this is the one exception. The `DelimiterJoinState`
    /// is responsible for accumulating the delimited parts into a single string
    delimiter: Option<(&'v str, DelimiterJoinState)>,
    format: Option<&'v str>,
    prepend: Option<&'v str>,
    quote: Option<QuoteStyle>,
    replacements: Vec<(FormatRegex<'v>, &'v str)>,
}

pub struct CommandLineFormatter<'v, 'a> {
    sink: &'a mut dyn CommandLineBuilder,
    artifact_path_mapping: &'a dyn ArtifactPathMapper,
    fs: &'a ExecutorFs<'a>,
    absolute: bool,
    // Stacks of currently in-scope actions; these are pushed/popped as scopes are entered/exited.
    //
    // Note that these two stacks work differently; the artifact options stack is cumulative, ie the
    // top entry in the stack represents the combined effects of all previous entries and so is the
    // only one that is "applied," while the same is not true of the string options.
    artifact_options_stack: Vec<ArtifactOptions<'v>>,
    string_options_stack: Vec<StringOptions<'v>>,

    // Paths where write-to-file macros go. Note that order matters and this list is in *reverse*
    // order (so that we can pop off the end).
    write_to_file_macro_paths: Option<Vec<ProjectRelativePathBuf>>,
}

impl<'v, 'a> CommandLineFormatter<'v, 'a> {
    pub fn new(
        sink: &'a mut dyn CommandLineBuilder,
        artifact_path_mapping: &'a dyn ArtifactPathMapper,
        fs: &'a ExecutorFs<'a>,
    ) -> Self {
        Self::new_with_options(sink, artifact_path_mapping, fs, false, None)
    }

    pub fn new_with_options(
        sink: &'a mut dyn CommandLineBuilder,
        artifact_path_mapping: &'a dyn ArtifactPathMapper,
        fs: &'a ExecutorFs<'a>,
        absolute: bool,
        mut write_to_file_macro_paths: Option<Vec<ProjectRelativePathBuf>>,
    ) -> Self {
        if let Some(write_to_file_macro_paths) = &mut write_to_file_macro_paths {
            write_to_file_macro_paths.reverse();
        }
        Self {
            sink,
            artifact_path_mapping,
            fs,
            absolute,
            artifact_options_stack: Vec::new(),
            string_options_stack: Vec::new(),
            write_to_file_macro_paths,
        }
    }

    pub(crate) fn push_scope(
        &mut self,
        opts: &CommandLineOptionsRef<'v, '_>,
    ) -> buck2_error::Result<()> {
        let CommandLineOptionsRef {
            relative_to,
            absolute_prefix,
            absolute_suffix,
            parent,
            ignore_artifacts: _,
            delimiter,
            format,
            prepend,
            quote,
            replacements,
        } = opts;

        let string_opts = StringOptions {
            delimiter: delimiter.map(|s| (s.as_str(), DelimiterJoinState::Unstarted)),
            format: format.map(|s| s.as_str()),
            prepend: prepend.map(|s| s.as_str()),
            quote: *quote,
            replacements: FormatRegex::make(*replacements),
        };

        let artifact_opts = ArtifactOptions {
            relative_to: relative_to
                .as_ref()
                .map(|(origin, parents)| {
                    compute_relative_to_path(*origin, *parents, self.fs, self.artifact_path_mapping)
                        .map(Arc::new)
                })
                .transpose()?,
            absolute_prefix: absolute_prefix.map(|s| s.as_str()),
            absolute_suffix: absolute_suffix.map(|s| s.as_str()),
            parent: *parent,
        };

        self.merge_and_push_scope(string_opts, artifact_opts);
        Ok(())
    }

    /// Equivalent to `push_scope` with just a delimiter set, but doesn't require constructing starlark values
    pub fn push_scope_delimiter(&mut self, delimiter: &'v str) {
        self.merge_and_push_scope(
            StringOptions {
                delimiter: Some((delimiter, DelimiterJoinState::Unstarted)),
                ..Default::default()
            },
            DEFAULT_ARTIFACT_OPTIONS,
        )
    }

    /// Equivalent to `push_scope`, but doesn't require constructing starlark values
    pub fn push_scope_format(&mut self, format: &'v str) {
        self.merge_and_push_scope(
            StringOptions {
                format: Some(format),
                ..Default::default()
            },
            DEFAULT_ARTIFACT_OPTIONS,
        )
    }

    /// Equivalent to `push_scope`, but doesn't require constructing starlark values
    pub fn push_scope_relative_to(&mut self, relative_to: ProjectRelativePathBuf) {
        self.merge_and_push_scope(
            StringOptions::default(),
            ArtifactOptions {
                relative_to: Some(Arc::new(relative_to)),
                ..DEFAULT_ARTIFACT_OPTIONS
            },
        )
    }

    fn merge_and_push_scope(
        &mut self,
        string_opts: StringOptions<'v>,
        artifact_opts: ArtifactOptions<'v>,
    ) {
        self.string_options_stack.push(string_opts);

        let prev = self
            .artifact_options_stack
            .last()
            .unwrap_or(&DEFAULT_ARTIFACT_OPTIONS);
        let artifact_opts = ArtifactOptions {
            relative_to: artifact_opts
                .relative_to
                .or_else(|| prev.relative_to.dupe()),
            absolute_prefix: artifact_opts.absolute_prefix.or(prev.absolute_prefix),
            absolute_suffix: artifact_opts.absolute_suffix.or(prev.absolute_suffix),
            parent: artifact_opts.parent + prev.parent,
        };
        self.artifact_options_stack.push(artifact_opts);
    }

    pub fn pop_scope(&mut self) {
        self.artifact_options_stack.pop();
        if let Some(string_options) = self.string_options_stack.pop() {
            if let Some((_, delim_state)) = string_options.delimiter {
                self.write_str(delim_state.finish());
            }
        }
    }

    pub fn push_str(&mut self, s: &str) {
        self.write_str(s.to_owned());
    }

    pub fn push_string(&mut self, s: String) {
        self.write_str(s);
    }

    pub fn push_artifact(&mut self, artifact: &Artifact) -> buck2_error::Result<()> {
        self.write_project_path(
            artifact.resolve_path(self.fs.fs(), self.artifact_path_mapping.get(artifact))?,
        )
    }

    pub fn push_output_artifact(&mut self, artifact: &Artifact) -> buck2_error::Result<()> {
        self.write_project_path(artifact.resolve_path(
            self.fs.fs(),
            Some(&ContentBasedPathHash::for_output_artifact()),
        )?)
    }

    pub(crate) fn push_next_write_to_file_macro_path(&mut self) -> buck2_error::Result<()> {
        let Some(write_to_file_macro_paths) = &mut self.write_to_file_macro_paths else {
            return Err(CommandLineOptionsError::WriteToFileMacroNotSupported.into());
        };
        let p = write_to_file_macro_paths.pop().ok_or_else(|| {
            buck2_error::internal_error!("Inconsistent number of macro artifacts")
        })?;
        self.write_project_path(p)
    }

    pub fn push_cell_path(&mut self, path: CellPathRef) -> buck2_error::Result<()> {
        self.write_project_path(self.fs.fs().resolve_cell_path(path)?)
    }

    pub fn push_project_path(&mut self, path: ProjectRelativePathBuf) -> buck2_error::Result<()> {
        self.write_project_path(path)
    }

    fn write_project_path(&mut self, mut path: ProjectRelativePathBuf) -> buck2_error::Result<()> {
        let ArtifactOptions {
            relative_to,
            absolute_prefix,
            absolute_suffix,
            parent,
        } = self
            .artifact_options_stack
            .last()
            .unwrap_or(&DEFAULT_ARTIFACT_OPTIONS);

        let components = path.iter().count();
        if (components as u32) < *parent {
            return Err(
                CommandLineOptionsError::TooManyParentCalls(path.to_string(), *parent).into(),
            );
        }
        for _ in 0..*parent {
            path.pop();
        }

        let mut rendered = if let Some(relative_to) = relative_to {
            let p = relative_to
                .as_forward_relative_path()
                .as_relative_path()
                .relative(path.as_forward_relative_path().as_relative_path());
            CommandLineLocation::format(&p, &self.fs).into_owned()
        } else if self.absolute {
            // Note that for obvious reasons we ignore `absolute` in the case where there's a
            // `relative_to` provided
            CommandLineLocation::format_absolute(&path, &self.fs).into_owned()
        } else {
            CommandLineLocation::format(path.as_ref(), &self.fs).into_owned()
        };

        if let Some(absolute_prefix) = absolute_prefix {
            rendered.insert_str(0, absolute_prefix);
        }
        if let Some(absolute_suffix) = absolute_suffix {
            rendered.push_str(absolute_suffix);
        }

        self.write_str(rendered);
        Ok(())
    }

    fn write_str(&mut self, v: String) {
        Self::write_str_inner(v, self.sink, &mut self.string_options_stack);
    }

    fn write_str_inner(
        v: String,
        sink: &mut dyn CommandLineBuilder,
        string_options: &mut [StringOptions<'v>],
    ) {
        let mut cur = v;
        // See below for why we do this
        let mut string_options_iter = string_options.iter_mut();
        while let Some(options) = string_options_iter.next_back() {
            for (reg, rep) in options.replacements.iter() {
                let new = match reg {
                    FormatRegex::Starlark(reg) => reg.replace_all(cur.as_ref(), rep),
                    FormatRegex::Owned(reg) => reg.replace_all(cur.as_ref(), *rep),
                };
                if new.as_ref() != cur {
                    cur = new.into_owned();
                }
            }
            if let Some(format) = options.format {
                cur = format.replace("{}", &cur);
            }
            if let Some(QuoteStyle::Shell) = options.quote {
                let quoted = shlex_quote(&cur);
                cur = quoted.into_owned();
            }
            // `prepend` has pretty different semantics between the cases where there's a delimiter
            // in the same options and when there isn't
            if let Some((delim, delim_state)) = &mut options.delimiter {
                if let Some(prepend) = options.prepend {
                    cur.insert_str(0, prepend);
                }
                delim_state.push(cur, delim);
                // We won't know how to finish applying the options on anything lower in the stack
                // until we finish accumulating this string, so we exit now; we'll handle the lower
                // options when popping this scope
                return;
            } else {
                if let Some(prepend) = options.prepend {
                    // We need to write both `prepend` and `cur` into the command line, however any
                    // options lower in the stack than the current one still need to be applied. We
                    // implement via recursion. That's not ideal, but note that repeated use of
                    // `prepend` results in exponential growth of the command line, so its not very easy
                    // to avoid either
                    Self::write_str_inner(
                        prepend.to_owned(),
                        sink,
                        string_options_iter.as_mut_slice(),
                    );
                }
            }
        }
        sink.push_arg(Cow::Owned(cur));
    }
}
