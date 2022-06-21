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

use std::fs;
use std::io;
use std::iter;
use std::path::Path;
use std::path::PathBuf;

use gazebo::prelude::*;
use itertools::Either;
use lsp_types::Diagnostic;
use lsp_types::Url;
use starlark::environment::FrozenModule;
use starlark::environment::Globals;
use starlark::environment::Module;
use starlark::errors::EvalMessage;
use starlark::eval::Evaluator;
use starlark::lsp::server::LoadContentsError;
use starlark::lsp::server::LspContext;
use starlark::lsp::server::LspEvalResult;
use starlark::lsp::server::ResolveLoadError;
use starlark::syntax::AstModule;
use starlark::syntax::Dialect;

#[derive(Debug)]
pub(crate) enum ContextMode {
    Check,
    Run,
}

#[derive(Debug)]
pub(crate) struct Context {
    pub(crate) mode: ContextMode,
    pub(crate) print_non_none: bool,
    pub(crate) prelude: Vec<FrozenModule>,
    pub(crate) module: Option<Module>,
}

/// The outcome of evaluating (checking, parsing or running) given starlark code.
pub(crate) struct EvalResult<T: Iterator<Item = EvalMessage>> {
    /// The diagnostic and error messages from evaluating a given piece of starlark code.
    pub messages: T,
    /// If the code is only parsed, not run, and there were no errors, this will contain
    /// the parsed module. Otherwise, it will be `None`
    pub ast: Option<AstModule>,
}

impl Context {
    pub(crate) fn new(
        mode: ContextMode,
        print_non_none: bool,
        prelude: &[PathBuf],
        module: bool,
    ) -> anyhow::Result<Self> {
        let globals = globals();
        let prelude = prelude.try_map(|x| {
            let env = Module::new();

            let mut eval = Evaluator::new(&env);
            let module = AstModule::parse_file(x, &dialect())?;
            eval.eval_module(module, &globals)?;
            env.freeze()
        })?;

        let module = if module {
            Some(Self::new_module(&prelude))
        } else {
            None
        };

        Ok(Self {
            mode,
            print_non_none,
            prelude,
            module,
        })
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

    // Convert an anyhow over iterator of EvalMessage, into an iterator of EvalMessage
    fn err(
        file: &str,
        result: anyhow::Result<EvalResult<impl Iterator<Item = EvalMessage>>>,
    ) -> EvalResult<impl Iterator<Item = EvalMessage>> {
        match result {
            Err(e) => EvalResult {
                messages: Either::Left(iter::once(EvalMessage::from_anyhow(file, e))),
                ast: None,
            },
            Ok(res) => EvalResult {
                messages: Either::Right(res.messages),
                ast: res.ast,
            },
        }
    }

    pub(crate) fn expression(
        &self,
        content: String,
    ) -> EvalResult<impl Iterator<Item = EvalMessage>> {
        let file = "expression";
        Self::err(
            file,
            AstModule::parse(file, content, &dialect()).map(|module| self.go(file, module)),
        )
    }

    pub(crate) fn file(&self, file: &Path) -> EvalResult<impl Iterator<Item = EvalMessage>> {
        let filename = &file.to_string_lossy();
        Self::err(
            filename,
            fs::read_to_string(file)
                .map(|content| self.file_with_contents(filename, content))
                .map_err(|e| e.into()),
        )
    }

    pub(crate) fn file_with_contents(
        &self,
        filename: &str,
        content: String,
    ) -> EvalResult<impl Iterator<Item = EvalMessage>> {
        Self::err(
            filename,
            AstModule::parse(filename, content, &dialect()).map(|module| self.go(filename, module)),
        )
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
            eval.eval_module(ast, &globals).map(|v| {
                if self.print_non_none && !v.is_none() {
                    println!("{}", v);
                }
                EvalResult {
                    messages: iter::empty(),
                    ast: None,
                }
            }),
        )
    }

    fn check(&self, module: &AstModule) -> impl Iterator<Item = EvalMessage> {
        let mut globals = Vec::new();
        for x in &self.prelude {
            globals.extend(x.names());
        }
        let globals = if self.prelude.is_empty() {
            None
        } else {
            Some(globals.as_slice())
        };

        module.lint(globals).into_iter().map(EvalMessage::from)
    }
}

impl LspContext for Context {
    fn parse_file_with_contents(&self, uri: &Url, content: String) -> LspEvalResult {
        let EvalResult { messages, ast } = self.file_with_contents(uri.path(), content);
        LspEvalResult {
            diagnostics: messages.map(Diagnostic::from).collect(),
            ast,
        }
    }

    fn resolve_load(&self, path: &str, current_file: &Path) -> anyhow::Result<Url> {
        let path = PathBuf::from(path);
        let current_file_dir = current_file.parent();
        let absolute_path = match (current_file_dir, path.is_absolute()) {
            (_, true) => Ok(path),
            (Some(current_file_dir), false) => Ok(current_file_dir.join(&path)),
            (None, false) => Err(ResolveLoadError::MissingCurrentFilePath(path)),
        }?;
        Ok(Url::from_file_path(absolute_path).unwrap())
    }

    fn get_load_contents(&self, uri: &Url) -> anyhow::Result<Option<String>> {
        let path = PathBuf::from(uri.path());
        match path.is_absolute() {
            true => match fs::read_to_string(&path) {
                Ok(contents) => Ok(Some(contents)),
                Err(e)
                    if e.kind() == io::ErrorKind::NotFound
                        || e.kind() == io::ErrorKind::NotADirectory =>
                {
                    Ok(None)
                }
                Err(e) => Err(e.into()),
            },
            false => Err(LoadContentsError::NotAbsolute(uri.clone()).into()),
        }
    }
}

pub(crate) fn globals() -> Globals {
    Globals::extended()
}

pub(crate) fn dialect() -> Dialect {
    Dialect::Extended
}
