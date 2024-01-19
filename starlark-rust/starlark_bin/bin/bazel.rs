/*
 * Copyright 2019 The Starlark in Rust Authors.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

//! This file provides an implementation of `LspContext` specifically aimed at
//! the use in a Bazel project. You can invoke it by using `starlark --lsp --bazel`.
//! Note that only `--lsp` mode is supported.
//!
//! This module is temporary, for the purpose of rapid iteration while the LSP
//! interface develops. After the API of the `LspContext` trait stabilizes, this
//! module will be removed, and extracted to its own project.

use std::borrow::Cow;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fs;
use std::io;
use std::iter;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;

use either::Either;
use lsp_types::CompletionItemKind;
use lsp_types::Url;
use starlark::analysis::find_call_name::AstModuleFindCallName;
use starlark::analysis::AstModuleLint;
use starlark::docs::get_registered_starlark_docs;
use starlark::docs::render_docs_as_code;
use starlark::docs::Doc;
use starlark::docs::DocItem;
use starlark::docs::DocModule;
use starlark::environment::FrozenModule;
use starlark::environment::Module;
use starlark::errors::EvalMessage;
use starlark::eval::Evaluator;
use starlark::syntax::AstModule;
use starlark_lsp::completion::StringCompletionResult;
use starlark_lsp::completion::StringCompletionType;
use starlark_lsp::error::eval_message_to_lsp_diagnostic;
use starlark_lsp::server::LspContext;
use starlark_lsp::server::LspEvalResult;
use starlark_lsp::server::LspUrl;
use starlark_lsp::server::StringLiteralResult;

use crate::eval::dialect;
use crate::eval::globals;
use crate::eval::ContextMode;
use crate::eval::EvalResult;

#[derive(Debug, thiserror::Error)]
enum ContextError {
    /// The provided Url was not absolute and it needs to be.
    #[error("Path for URL `{}` was not absolute", .0)]
    NotAbsolute(LspUrl),
    /// The scheme provided was not correct or supported.
    #[error("Url `{}` was expected to be of type `{}`", .1, .0)]
    WrongScheme(String, LspUrl),
}

/// Errors when [`LspContext::resolve_load()`] cannot resolve a given path.
#[derive(thiserror::Error, Debug)]
enum ResolveLoadError {
    /// Attempted to resolve a relative path, but no current_file_path was provided,
    /// so it is not known what to resolve the path against.
    #[error("Relative path `{}` provided, but current_file_path could not be determined", .0)]
    MissingCurrentFilePath(String),
    /// The scheme provided was not correct or supported.
    #[error("Url `{}` was expected to be of type `{}`", .1, .0)]
    WrongScheme(String, LspUrl),
    /// Received a load for an absolute path from the root of the workspace, but the
    /// path to the workspace root was not provided.
    #[error("Path `//{}` is absolute from the root of the workspace, but no workspace root was provided", .0)]
    MissingWorkspaceRoot(String),
    /// Unable to parse the given path.
    #[error("Unable to parse the load path `{}`", .0)]
    CannotParsePath(String),
    /// The path contained a repository name that is not known to Bazel.
    #[error("Cannot resolve path `{}` because the repository `{}` is unknown", .0, .1)]
    UnknownRepository(String, String),
    /// The path contained a target name that does not resolve to an existing file.
    #[error("Cannot resolve path `{}` because the file does not exist", .0)]
    TargetNotFound(String),
}

/// Errors when [`LspContext::render_as_load()`] cannot render a given path.
#[derive(thiserror::Error, Debug)]
enum RenderLoadError {
    /// Attempted to get the filename of a path that does not seem to contain a filename.
    #[error("Path `{}` provided, which does not seem to contain a filename", .0.display())]
    MissingTargetFilename(PathBuf),
    /// The scheme provided was not correct or supported.
    #[error("Urls `{}` and `{}` was expected to be of type `{}`", .1, .2, .0)]
    WrongScheme(String, LspUrl, LspUrl),
}

/// Starting point for resolving filesystem completions.
#[derive(Debug, Clone, PartialEq, Eq)]
enum FilesystemCompletionRoot<'a> {
    /// A resolved path, e.g. from an opened document.
    Path(&'a Path),
    /// An unresolved path, e.g. from a string literal in a `load` statement.
    String(&'a str),
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum FilesystemFileCompletionOptions {
    All,
    OnlyLoadable,
    None,
}

/// Options for resolving filesystem completions.
#[derive(Debug, Clone, PartialEq, Eq)]
struct FilesystemCompletionOptions {
    /// Whether to include directories in the results.
    directories: bool,
    /// Whether to include files in the results.
    files: FilesystemFileCompletionOptions,
    /// Whether to include target names from BUILD files.
    targets: bool,
}

pub(crate) fn main(
    lsp: bool,
    print_non_none: bool,
    is_interactive: bool,
    prelude: &[PathBuf],
) -> anyhow::Result<()> {
    if !lsp {
        return Err(anyhow::anyhow!("Bazel mode only supports `--lsp`"));
    }

    // NOTE: Copied from `main.rs`
    let mut ctx = BazelContext::new(ContextMode::Check, print_non_none, prelude, is_interactive)?;

    ctx.mode = ContextMode::Check;
    starlark_lsp::server::stdio_server(ctx)?;

    Ok(())
}

pub(crate) struct BazelContext {
    pub(crate) workspace_name: Option<String>,
    pub(crate) external_output_base: Option<PathBuf>,
    pub(crate) mode: ContextMode,
    pub(crate) print_non_none: bool,
    pub(crate) prelude: Vec<FrozenModule>,
    pub(crate) module: Option<Module>,
    pub(crate) builtin_docs: HashMap<LspUrl, String>,
    pub(crate) builtin_symbols: HashMap<String, LspUrl>,
}

impl BazelContext {
    const DEFAULT_WORKSPACE_NAME: &'static str = "__main__";
    const BUILD_FILE_NAMES: [&'static str; 2] = ["BUILD", "BUILD.bazel"];
    const LOADABLE_EXTENSIONS: [&'static str; 1] = ["bzl"];

    pub(crate) fn new(
        mode: ContextMode,
        print_non_none: bool,
        prelude: &[PathBuf],
        module: bool,
    ) -> anyhow::Result<Self> {
        let globals = globals();
        let prelude: Vec<_> = prelude
            .iter()
            .map(|x| {
                let env = Module::new();
                {
                    let mut eval = Evaluator::new(&env);
                    let module = AstModule::parse_file(x, &dialect())
                        .map_err(starlark::Error::into_anyhow)?;
                    eval.eval_module(module, &globals)
                        .map_err(starlark::Error::into_anyhow)?;
                }
                env.freeze()
            })
            .collect::<anyhow::Result<_>>()?;

        let module = if module {
            Some(Self::new_module(&prelude))
        } else {
            None
        };
        let mut builtins: HashMap<LspUrl, Vec<Doc>> = HashMap::new();
        let mut builtin_symbols: HashMap<String, LspUrl> = HashMap::new();
        for doc in get_registered_starlark_docs() {
            let uri = Self::url_for_doc(&doc);
            builtin_symbols.insert(doc.id.name.clone(), uri.clone());
            builtins.entry(uri).or_default().push(doc);
        }
        let builtin_docs = builtins
            .into_iter()
            .map(|(u, ds)| (u, render_docs_as_code(&ds)))
            .collect();

        let mut raw_command = Command::new("bazel");
        let mut command = raw_command.arg("info");
        command = command.current_dir(std::env::current_dir()?);

        let output = command.output()?;
        if !output.status.success() {
            return Err(anyhow::anyhow!("Command `bazel info` failed"));
        }

        let output = String::from_utf8(output.stdout)?;
        let mut execroot = None;
        let mut output_base = None;
        for line in output.lines() {
            if let Some((key, value)) = line.split_once(": ") {
                match key {
                    "execution_root" => execroot = Some(value),
                    "output_base" => output_base = Some(value),
                    _ => {}
                }
            }
        }

        Ok(Self {
            mode,
            print_non_none,
            prelude,
            module,
            builtin_docs,
            builtin_symbols,
            workspace_name: execroot.and_then(|execroot| {
                match PathBuf::from(execroot)
                    .file_name()?
                    .to_string_lossy()
                    .to_string()
                {
                    name if name == Self::DEFAULT_WORKSPACE_NAME => None,
                    name => Some(name),
                }
            }),
            external_output_base: output_base
                .map(|output_base| PathBuf::from(output_base).join("external")),
        })
    }

    // Convert an anyhow over iterator of EvalMessage, into an iterator of EvalMessage
    fn err(
        file: &str,
        result: starlark::Result<EvalResult<impl Iterator<Item = EvalMessage>>>,
    ) -> EvalResult<impl Iterator<Item = EvalMessage>> {
        match result {
            Err(e) => EvalResult {
                messages: Either::Left(iter::once(EvalMessage::from_error(Path::new(file), &e))),
                ast: None,
            },
            Ok(res) => EvalResult {
                messages: Either::Right(res.messages),
                ast: res.ast,
            },
        }
    }

    fn url_for_doc(doc: &Doc) -> LspUrl {
        let url = match &doc.item {
            DocItem::Module(_) => Url::parse("starlark:/native/builtins.bzl").unwrap(),
            DocItem::Object(_) => {
                Url::parse(&format!("starlark:/native/builtins/{}.bzl", doc.id.name)).unwrap()
            }
            DocItem::Function(_) | DocItem::Property(_) => {
                Url::parse("starlark:/native/builtins.bzl").unwrap()
            }
        };
        LspUrl::try_from(url).unwrap()
    }

    fn new_module(prelude: &[FrozenModule]) -> Module {
        let module = Module::new();
        for p in prelude {
            module.import_public_symbols(p);
        }
        module
    }

    fn go(&self, file: &str, ast: AstModule) -> EvalResult<impl Iterator<Item = EvalMessage>> {
        let mut warnings = Either::Left(iter::empty());
        let mut errors = Either::Left(iter::empty());
        let final_ast = match self.mode {
            ContextMode::Check => {
                warnings = Either::Right(self.check(&ast));
                Some(ast)
            }
            ContextMode::Run => {
                errors = Either::Right(self.run(file, ast).messages);
                None
            }
        };
        EvalResult {
            messages: warnings.chain(errors),
            ast: final_ast,
        }
    }

    fn run(&self, file: &str, ast: AstModule) -> EvalResult<impl Iterator<Item = EvalMessage>> {
        let new_module;
        let module = match self.module.as_ref() {
            Some(module) => module,
            None => {
                new_module = Self::new_module(&self.prelude);
                &new_module
            }
        };
        let mut eval = Evaluator::new(module);
        eval.enable_terminal_breakpoint_console();
        let globals = globals();
        Self::err(
            file,
            eval.eval_module(ast, &globals)
                .map(|v| {
                    if self.print_non_none && !v.is_none() {
                        println!("{}", v);
                    }
                    EvalResult {
                        messages: iter::empty(),
                        ast: None,
                    }
                })
                .map_err(Into::into),
        )
    }

    fn check(&self, module: &AstModule) -> impl Iterator<Item = EvalMessage> {
        let globals = if self.prelude.is_empty() {
            None
        } else {
            let mut globals = HashSet::new();
            for modu in &self.prelude {
                for name in modu.names() {
                    globals.insert(name.as_str().to_owned());
                }
            }

            for global_symbol in self.builtin_symbols.keys() {
                globals.insert(global_symbol.to_owned());
            }

            Some(globals)
        };

        module
            .lint(globals.as_ref())
            .into_iter()
            .map(EvalMessage::from)
    }
    pub(crate) fn file_with_contents(
        &self,
        filename: &str,
        content: String,
    ) -> EvalResult<impl Iterator<Item = EvalMessage>> {
        Self::err(
            filename,
            AstModule::parse(filename, content, &dialect())
                .map(|module| self.go(filename, module))
                .map_err(Into::into),
        )
    }

    fn get_repository_for_path<'a>(&'a self, path: &'a Path) -> Option<(Cow<'a, str>, &'a Path)> {
        self.external_output_base
            .as_ref()
            .and_then(|external_output_base| path.strip_prefix(external_output_base).ok())
            .and_then(|path| {
                let mut path_components = path.components();

                let repository_name = path_components.next()?.as_os_str().to_string_lossy();
                let repository_path = path_components.as_path();

                Some((repository_name, repository_path))
            })
    }

    fn get_repository_path(&self, repository_name: &str) -> Option<PathBuf> {
        self.external_output_base
            .as_ref()
            .map(|external_output_base| external_output_base.join(repository_name))
    }

    fn resolve_folder<'a>(
        &self,
        path: &'a str,
        current_file: &LspUrl,
        workspace_root: Option<&Path>,
        resolved_filename: &mut Option<&'a str>,
    ) -> anyhow::Result<PathBuf> {
        let original_path = path;
        if let Some((repository, path)) = path.split_once("//") {
            // The repository may be prefixed with an '@', but it's optional in Buck2.
            let repository = if let Some(without_at) = repository.strip_prefix('@') {
                without_at
            } else {
                repository
            };

            // Find the root we're resolving from. There's quite a few cases to consider here:
            // - `repository` is empty, and we're resolving from the workspace root.
            // - `repository` is empty, and we're resolving from a known remote repository.
            // - `repository` is not empty, and refers to the current repository (the workspace).
            // - `repository` is not empty, and refers to a known remote repository.
            //
            // Also with all of these cases, we need to consider if we have build system
            // information or not. If not, we can't resolve any remote repositories, and we can't
            // know whether a repository name refers to the workspace or not.
            let resolve_root = match (repository, current_file) {
                // Repository is empty, and we know what file we're resolving from. Use the build
                // system information to check if we're in a known remote repository, and what the
                // root is. Fall back to the `workspace_root` otherwise.
                ("", LspUrl::File(current_file)) => {
                    if let Some((repository_name, _)) = self.get_repository_for_path(current_file) {
                        self.get_repository_path(&repository_name).map(Cow::Owned)
                    } else {
                        workspace_root.map(Cow::Borrowed)
                    }
                }
                // No repository in the load path, and we don't have build system information, or
                // an `LspUrl` we can't use to check the root. Use the workspace root.
                ("", _) => workspace_root.map(Cow::Borrowed),
                // We have a repository name and build system information. Check if the repository
                // name refers to the workspace, and if so, use the workspace root. If not, check
                // if it refers to a known remote repository, and if so, use that root.
                // Otherwise, fail with an error.
                (repository, _) => {
                    if matches!(self.workspace_name.as_ref(), Some(name) if name == repository) {
                        workspace_root.map(Cow::Borrowed)
                    } else if let Some(remote_repository_root) =
                        self.get_repository_path(repository).map(Cow::Owned)
                    {
                        Some(remote_repository_root)
                    } else {
                        return Err(ResolveLoadError::UnknownRepository(
                            original_path.to_owned(),
                            repository.to_owned(),
                        )
                        .into());
                    }
                }
            };

            // Resolve from the root of the repository.
            match (path.split_once(':'), resolve_root) {
                (Some((subfolder, filename)), Some(resolve_root)) => {
                    resolved_filename.replace(filename);
                    Ok(resolve_root.join(subfolder))
                }
                (None, Some(resolve_root)) => Ok(resolve_root.join(path)),
                (Some(_), None) => {
                    Err(ResolveLoadError::MissingWorkspaceRoot(original_path.to_owned()).into())
                }
                (None, _) => {
                    Err(ResolveLoadError::CannotParsePath(original_path.to_owned()).into())
                }
            }
        } else if let Some((folder, filename)) = path.split_once(':') {
            resolved_filename.replace(filename);

            // Resolve relative paths from the current file.
            match current_file {
                LspUrl::File(current_file_path) => {
                    let current_file_dir = current_file_path.parent();
                    match current_file_dir {
                        Some(current_file_dir) => Ok(current_file_dir.join(folder)),
                        None => {
                            Err(ResolveLoadError::MissingCurrentFilePath(path.to_owned()).into())
                        }
                    }
                }
                _ => Err(
                    ResolveLoadError::WrongScheme("file://".to_owned(), current_file.clone())
                        .into(),
                ),
            }
        } else {
            Err(ResolveLoadError::CannotParsePath(path.to_owned()).into())
        }
    }

    fn get_repository_names(&self) -> Vec<Cow<str>> {
        let mut names = Vec::new();
        if let Some(workspace_name) = &self.workspace_name {
            names.push(Cow::Borrowed(workspace_name.as_str()));
        }

        if let Some(external_output_base) = self.external_output_base.as_ref() {
            // Look for existing folders in `external_output_base`.
            if let Ok(entries) = std::fs::read_dir(external_output_base) {
                for entry in entries.flatten() {
                    if let Ok(file_type) = entry.file_type() {
                        if file_type.is_dir() {
                            if let Some(name) = entry.file_name().to_str() {
                                names.push(Cow::Owned(name.to_owned()));
                            }
                        }
                    }
                }
            }
        }

        names
    }

    fn get_filesystem_entries(
        &self,
        from: FilesystemCompletionRoot,
        current_file: &LspUrl,
        workspace_root: Option<&Path>,
        options: &FilesystemCompletionOptions,
        results: &mut Vec<StringCompletionResult>,
    ) -> anyhow::Result<()> {
        // Find the actual folder on disk we're looking at.
        let (from_path, render_base) = match from {
            FilesystemCompletionRoot::Path(path) => (path.to_owned(), path.to_string_lossy()),
            FilesystemCompletionRoot::String(str) => (
                self.resolve_folder(str, current_file, workspace_root, &mut None)?,
                Cow::Borrowed(str),
            ),
        };

        for entry in fs::read_dir(from_path)? {
            let entry = entry?;
            let path = entry.path();
            // NOTE: Safe to `unwrap()` here, because we know that `path` is a file system path. And
            // since it's an entry in a directory, it must have a file name.
            let file_name = path.file_name().unwrap().to_string_lossy();
            if path.is_dir() && options.directories {
                results.push(StringCompletionResult {
                    value: file_name.to_string(),
                    insert_text: Some(format!(
                        "{}{}",
                        if render_base.ends_with('/') || render_base.is_empty() {
                            ""
                        } else {
                            "/"
                        },
                        file_name
                    )),
                    insert_text_offset: render_base.len(),
                    kind: CompletionItemKind::FOLDER,
                });
            } else if path.is_file() {
                if Self::BUILD_FILE_NAMES.contains(&file_name.as_ref()) {
                    if options.targets {
                        if let Some(targets) = self.query_buildable_targets(
                            &format!(
                                "{render_base}{}",
                                if render_base.ends_with(':') { "" } else { ":" }
                            ),
                            workspace_root,
                        ) {
                            results.extend(targets.into_iter().map(|target| {
                                StringCompletionResult {
                                    value: target.to_owned(),
                                    insert_text: Some(format!(
                                        "{}{}",
                                        if render_base.ends_with(':') { "" } else { ":" },
                                        target
                                    )),
                                    insert_text_offset: render_base.len(),
                                    kind: CompletionItemKind::PROPERTY,
                                }
                            }));
                        }
                    }
                    continue;
                } else if options.files != FilesystemFileCompletionOptions::None {
                    // Check if it's in the list of allowed extensions. If we have a list, and it
                    // doesn't contain the extension, or the file has no extension, skip this file.
                    if options.files == FilesystemFileCompletionOptions::OnlyLoadable {
                        let extension = path.extension().map(|ext| ext.to_string_lossy());
                        match extension {
                            Some(extension) => {
                                if !Self::LOADABLE_EXTENSIONS.contains(&extension.as_ref()) {
                                    continue;
                                }
                            }
                            None => {
                                continue;
                            }
                        }
                    }

                    results.push(StringCompletionResult {
                        value: file_name.to_string(),
                        insert_text: Some(format!(
                            "{}{}",
                            if render_base.ends_with(':') || render_base.is_empty() {
                                ""
                            } else {
                                ":"
                            },
                            file_name
                        )),
                        insert_text_offset: render_base.len(),
                        kind: CompletionItemKind::FILE,
                    });
                }
            }
        }

        Ok(())
    }

    fn query_buildable_targets(
        &self,
        module: &str,
        workspace_dir: Option<&Path>,
    ) -> Option<Vec<String>> {
        let mut raw_command = Command::new("bazel");
        let mut command = raw_command.arg("query").arg(format!("{module}*"));
        if let Some(workspace_dir) = workspace_dir {
            command = command.current_dir(workspace_dir);
        }

        let output = command.output().ok()?;
        if !output.status.success() {
            return None;
        }

        let output = String::from_utf8(output.stdout).ok()?;
        Some(
            output
                .lines()
                .filter_map(|line| line.strip_prefix(module).map(|str| str.to_owned()))
                .collect(),
        )
    }
}

impl LspContext for BazelContext {
    fn parse_file_with_contents(&self, uri: &LspUrl, content: String) -> LspEvalResult {
        match uri {
            LspUrl::File(uri) => {
                let EvalResult { messages, ast } =
                    self.file_with_contents(&uri.to_string_lossy(), content);
                LspEvalResult {
                    diagnostics: messages.map(eval_message_to_lsp_diagnostic).collect(),
                    ast,
                }
            }
            _ => LspEvalResult::default(),
        }
    }

    fn resolve_load(
        &self,
        path: &str,
        current_file: &LspUrl,
        workspace_root: Option<&std::path::Path>,
    ) -> anyhow::Result<LspUrl> {
        let mut presumed_filename = None;
        let folder =
            self.resolve_folder(path, current_file, workspace_root, &mut presumed_filename)?;

        // Try the presumed filename first, and check if it exists.
        if let Some(presumed_filename) = presumed_filename {
            let path = folder.join(presumed_filename);
            if path.exists() {
                return Ok(Url::from_file_path(path).unwrap().try_into()?);
            }
        } else {
            return Err(ResolveLoadError::CannotParsePath(path.to_owned()).into());
        }

        // If the presumed filename doesn't exist, try to find a build file from the build system
        // and use that instead.
        for build_file_name in Self::BUILD_FILE_NAMES {
            let path = folder.join(build_file_name);
            if path.exists() {
                return Ok(Url::from_file_path(path).unwrap().try_into()?);
            }
        }

        Err(ResolveLoadError::TargetNotFound(path.to_owned()).into())
    }

    fn render_as_load(
        &self,
        target: &LspUrl,
        current_file: &LspUrl,
        workspace_root: Option<&Path>,
    ) -> anyhow::Result<String> {
        match (target, current_file) {
            // Check whether the target and the current file are in the same package.
            (LspUrl::File(target_path), LspUrl::File(current_file_path)) if matches!((target_path.parent(), current_file_path.parent()), (Some(a), Some(b)) if a == b) =>
            {
                // Then just return a relative path.
                let target_filename = target_path.file_name();
                match target_filename {
                    Some(filename) => Ok(format!(":{}", filename.to_string_lossy())),
                    None => Err(RenderLoadError::MissingTargetFilename(target_path.clone()).into()),
                }
            }
            (LspUrl::File(target_path), _) => {
                // Try to find a repository that contains the target, as well as the path to the
                // target relative to the repository root. If we can't find a repository, we'll
                // try to resolve the target relative to the workspace root. If we don't have a
                // workspace root, we'll just use the target path as-is.
                let (repository, target_path) = &self
                    .get_repository_for_path(target_path)
                    .map(|(repository, target_path)| (Some(repository), target_path))
                    .or_else(|| {
                        workspace_root
                            .and_then(|root| target_path.strip_prefix(root).ok())
                            .map(|path| (None, path))
                    })
                    .unwrap_or((None, target_path));

                let target_filename = target_path.file_name();
                match target_filename {
                    Some(filename) => Ok(format!(
                        "@{}//{}:{}",
                        repository.as_ref().unwrap_or(&Cow::Borrowed("")),
                        target_path
                            .parent()
                            .map(|path| path.to_string_lossy())
                            .unwrap_or_default(),
                        filename.to_string_lossy()
                    )),
                    None => Err(
                        RenderLoadError::MissingTargetFilename(target_path.to_path_buf()).into(),
                    ),
                }
            }
            _ => Err(RenderLoadError::WrongScheme(
                "file://".to_owned(),
                target.clone(),
                current_file.clone(),
            )
            .into()),
        }
    }

    fn resolve_string_literal(
        &self,
        literal: &str,
        current_file: &LspUrl,
        workspace_root: Option<&Path>,
    ) -> anyhow::Result<Option<StringLiteralResult>> {
        self.resolve_load(literal, current_file, workspace_root)
            .map(|url| {
                let original_target_name = Path::new(literal).file_name();
                let path_file_name = url.path().file_name();
                let same_filename = original_target_name == path_file_name;

                Some(StringLiteralResult {
                    url: url.clone(),
                    // If the target name is the same as the original target name, we don't need to
                    // do anything. Otherwise, we need to find the function call in the target file
                    // that has a `name` parameter with the same value as the original target name.
                    location_finder: if same_filename {
                        None
                    } else {
                        let literal = literal.to_owned();
                        Some(Box::new(move |ast| {
                            Ok(ast.find_function_call_with_name(&literal))
                        }))
                    },
                })
            })
    }

    fn get_load_contents(&self, uri: &LspUrl) -> anyhow::Result<Option<String>> {
        match uri {
            LspUrl::File(path) => match path.is_absolute() {
                true => match fs::read_to_string(path) {
                    Ok(contents) => Ok(Some(contents)),
                    Err(e) if e.kind() == io::ErrorKind::NotFound => Ok(None),
                    Err(e) => Err(e.into()),
                },
                false => Err(ContextError::NotAbsolute(uri.clone()).into()),
            },
            LspUrl::Starlark(_) => Ok(self.builtin_docs.get(uri).cloned()),
            _ => Err(ContextError::WrongScheme("file://".to_owned(), uri.clone()).into()),
        }
    }

    fn get_environment(&self, _uri: &LspUrl) -> DocModule {
        DocModule::default()
    }

    fn get_url_for_global_symbol(
        &self,
        _current_file: &LspUrl,
        symbol: &str,
    ) -> anyhow::Result<Option<LspUrl>> {
        Ok(self.builtin_symbols.get(symbol).cloned())
    }

    fn get_string_completion_options(
        &self,
        document_uri: &LspUrl,
        kind: StringCompletionType,
        current_value: &str,
        workspace_root: Option<&Path>,
    ) -> anyhow::Result<Vec<StringCompletionResult>> {
        let offer_repository_names = current_value.is_empty()
            || current_value == "@"
            || (current_value.starts_with('@') && !current_value.contains('/'))
            || (!current_value.contains('/') && !current_value.contains(':'));

        let mut names = if offer_repository_names {
            self.get_repository_names()
                .into_iter()
                .map(|name| {
                    let name_with_at = format!("@{}", name);
                    let insert_text = format!("{}//", &name_with_at);

                    StringCompletionResult {
                        value: name_with_at,
                        insert_text: Some(insert_text),
                        insert_text_offset: 0,
                        kind: CompletionItemKind::MODULE,
                    }
                })
                .collect()
        } else {
            vec![]
        };

        // Complete filenames if we're not in the middle of typing a repository name:
        // "@foo" -> don't complete filenames (still typing repository)
        // "@foo/" -> don't complete filenames (need two separating slashes)
        // "@foo//", "@foo//bar -> complete directories (from `@foo//`)
        // "@foo//bar/baz" -> complete directories (from `@foo//bar`)
        // "@foo//bar:baz" -> complete filenames (from `@foo//bar`), and target names if `kind` is `String`
        // "foo" -> complete directories and filenames (ambiguous, might be a relative path or a repository)
        let complete_directories = (!current_value.starts_with('@')
            || current_value.contains("//"))
            && !current_value.contains(':');
        let complete_filenames =
            // Still typing repository
            (!current_value.starts_with('@') || current_value.contains("//")) &&
            // Explicitly typing directory
            (!current_value.contains('/') || current_value.contains(':'));
        let complete_targets = kind == StringCompletionType::String && complete_filenames;
        if complete_directories || complete_filenames || complete_targets {
            if let Some(completion_root) = if complete_directories && complete_filenames {
                // This must mean we don't have a `/` or `:` separator, so we're completing a relative path.
                // Use the document URI's directory as the base.
                document_uri
                    .path()
                    .parent()
                    .map(FilesystemCompletionRoot::Path)
            } else {
                // Complete from the last `:` or `/` in the current value.
                current_value
                    // NOTE: Can't use `rsplit_once` as we need the value _including_ the value
                    // we're splitting on.
                    .rfind(if complete_directories { '/' } else { ':' })
                    .map(|pos| &current_value[..pos + 1])
                    .map(FilesystemCompletionRoot::String)
            } {
                self.get_filesystem_entries(
                    completion_root,
                    document_uri,
                    workspace_root,
                    &FilesystemCompletionOptions {
                        directories: complete_directories,
                        files: match (kind, complete_filenames) {
                            (StringCompletionType::LoadPath, _) => {
                                FilesystemFileCompletionOptions::OnlyLoadable
                            }
                            (StringCompletionType::String, true) => {
                                FilesystemFileCompletionOptions::All
                            }
                            (StringCompletionType::String, false) => {
                                FilesystemFileCompletionOptions::None
                            }
                        },
                        targets: complete_targets,
                    },
                    &mut names,
                )?;
            }
        }

        Ok(names)
    }
}
