/*
 * Copyright 2018 The Starlark in Rust Authors.
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

//! Evaluate some code, typically done by creating an [`Evaluator`], then calling
//! [`eval_module`](Evaluator::eval_module).

pub(crate) mod bc;
pub(crate) mod compiler;
pub(crate) mod runtime;

use std::collections::HashMap;
use std::mem;
use std::time::Instant;

use dupe::Dupe;
pub use runtime::arguments::Arguments;
pub use runtime::before_stmt::BeforeStmtFuncDyn;
pub use runtime::evaluator::Evaluator;
pub use runtime::file_loader::FileLoader;
pub use runtime::file_loader::ReturnFileLoader;
pub use runtime::params::ParametersParser;
pub use runtime::params::ParametersSpec;
pub use runtime::params::ParametersSpecBuilder;
pub use runtime::profile::data::ProfileData;
pub use runtime::profile::ProfileMode;
pub use starlark_syntax::call_stack::CallStack;
use starlark_syntax::eval_exception::EvalException;
use starlark_syntax::slice_vec_ext::SliceExt;
use starlark_syntax::syntax::module::AstModule;

use crate::collections::symbol_map::Symbol;
use crate::docs::DocString;
use crate::environment::Globals;
use crate::eval::compiler::def::DefInfo;
use crate::eval::compiler::scope::scope_resolver_globals::ScopeResolverGlobals;
use crate::eval::compiler::scope::ModuleScopes;
use crate::eval::compiler::scope::ScopeId;
use crate::eval::compiler::Compiler;
use crate::eval::runtime::arguments::ArgNames;
use crate::eval::runtime::arguments::ArgumentsFull;
use crate::syntax::DialectTypes;
use crate::values::Value;

impl<'v, 'a> Evaluator<'v, 'a> {
    /// Evaluate an [`AstModule`] with this [`Evaluator`], modifying the in-scope
    /// [`Module`](crate::environment::Module) as appropriate.
    pub fn eval_module(&mut self, ast: AstModule, globals: &Globals) -> anyhow::Result<Value<'v>> {
        let start = Instant::now();

        let AstModule {
            codemap,
            statement,
            dialect,
            allow_string_literals_in_type_expr,
        } = ast;

        let codemap = self
            .module_env
            .frozen_heap()
            .alloc_any_display_from_debug(codemap.dupe());

        let globals = self
            .module_env
            .frozen_heap()
            .alloc_any_display_from_type_name(globals.dupe());

        if let Some(docstring) = DocString::extract_raw_starlark_docstring(&statement) {
            self.module_env.set_docstring(docstring)
        }

        let ModuleScopes {
            cst,
            module_slot_count,
            scope_data,
            top_level_stmt_count,
        } = ModuleScopes::check_module_err(
            self.module_env.mutable_names(),
            self.module_env.frozen_heap(),
            &HashMap::new(),
            statement,
            ScopeResolverGlobals {
                globals: Some(globals),
            },
            codemap,
            &dialect,
        )?;

        let scope_names = scope_data.get_scope(ScopeId::module());
        let local_names = self
            .frozen_heap()
            .alloc_any_slice_display_from_debug(&scope_names.used);

        self.module_env.slots().ensure_slots(module_slot_count);
        let old_def_info = mem::replace(
            &mut self.module_def_info,
            self.module_env.frozen_heap().alloc_any(DefInfo::for_module(
                codemap,
                local_names,
                self.module_env
                    .frozen_heap()
                    .alloc_any_slice_display_from_debug(&scope_names.parent),
                globals,
            )),
        );

        // Set up the world to allow evaluation (do NOT use ? from now on)

        self.call_stack.push(Value::new_none(), None).unwrap();

        // Evaluation
        let mut compiler = Compiler {
            scope_data,
            locals: Vec::new(),
            globals,
            codemap,
            eval: self,
            check_types: dialect.enable_types == DialectTypes::Enable,
            top_level_stmt_count,
            allow_string_literals_in_type_expr,
        };

        let res = compiler.eval_module(cst, local_names);

        // Clean up the world, putting everything back
        self.call_stack.pop();

        self.module_def_info = old_def_info;

        self.module_env.add_eval_duration(start.elapsed());

        // Return the result of evaluation
        res.map_err(EvalException::into_anyhow)
    }

    /// Evaluate a function stored in a [`Value`], passing in `positional` and `named` arguments.
    pub fn eval_function(
        &mut self,
        function: Value<'v>,
        positional: &[Value<'v>],
        named: &[(&str, Value<'v>)],
    ) -> anyhow::Result<Value<'v>> {
        let names = named.map(|(s, _)| (Symbol::new(s), self.heap().alloc_str(s)));
        let named = named.map(|x| x.1);
        let params = Arguments(ArgumentsFull {
            pos: positional,
            named: &named,
            names: ArgNames::new(&names),
            args: None,
            kwargs: None,
        });
        // eval_module pushes an "empty" call stack frame. other places expect that first frame to be ignorable, and
        // so we push an empty frame too (otherwise things would ignore this function's own frame).
        self.with_call_stack(Value::new_none(), None, |this| {
            function.invoke(&params, this)
        })
    }
}
