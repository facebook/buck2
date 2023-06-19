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

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::fmt::Display;

use dupe::Dupe;

use crate::codemap::CodeMap;
use crate::codemap::FileSpanRef;
use crate::codemap::Span;
use crate::environment::names::MutableNames;
use crate::environment::Globals;
use crate::eval::compiler::scope::BindingId;
use crate::eval::compiler::scope::ModuleScopes;
use crate::eval::compiler::EvalException;
use crate::syntax::ast::Visibility;
use crate::syntax::AstModule;
use crate::syntax::Dialect;
use crate::typing::bindings::Bindings;
use crate::typing::bindings::BindingsCollect;
use crate::typing::bindings::Interface;
use crate::typing::ctx::TypingContext;
use crate::typing::oracle::traits::TypingOracle;
use crate::typing::ty::Approximation;
use crate::typing::ty::Ty;
use crate::values::FrozenHeap;

// Things which are None in the map have type void - they are never constructed
fn solve_bindings(
    oracle: &dyn TypingOracle,
    bindings: Bindings,
    codemap: &CodeMap,
) -> (
    Vec<EvalException>,
    HashMap<BindingId, Ty>,
    Vec<Approximation>,
) {
    let mut types = bindings
        .expressions
        .keys()
        .map(|x| (*x, Ty::Void))
        .collect::<HashMap<_, _>>();
    for (k, ty) in bindings.types {
        types.insert(k, ty);
    }
    // FIXME: Should be a fixed point, just do 10 iterations since that probably converges
    let mut changed = false;
    let mut ctx = TypingContext {
        codemap,
        oracle,
        errors: RefCell::new(Vec::new()),
        approximoations: RefCell::new(Vec::new()),
        types,
    };
    const ITERATIONS: usize = 100;
    for _iteration in 0..ITERATIONS {
        changed = false;
        ctx.errors.borrow_mut().clear();
        for (name, exprs) in &bindings.expressions {
            for expr in exprs {
                let ty = ctx.expression_bind_type(expr);
                let t = ctx.types.get_mut(name).unwrap();
                let new = Ty::union2(t.clone(), ty);
                if &new != t {
                    changed = true;
                    *t = new;
                }
            }
        }
        if !changed {
            break;
        }
    }
    if changed {
        ctx.approximoations.borrow_mut().push(Approximation::new(
            "Fixed point didn't converge",
            ITERATIONS,
        ));
    }
    // Make sure we check every expression, looking for failures
    for x in &bindings.check {
        ctx.expression_type(x);
    }
    for (span, e, require) in &bindings.check_type {
        let ty = match e {
            None => Ty::None,
            Some(x) => ctx.expression_type(x),
        };
        ctx.validate_type(&ty, require, *span);
    }
    (
        ctx.errors.into_inner(),
        ctx.types,
        ctx.approximoations.into_inner(),
    )
}

/// Structure containing all the inferred types.
#[derive(Debug)]
pub struct TypeMap {
    codemap: CodeMap,
    bindings: HashMap<BindingId, (String, Span, Ty)>,
}

impl Display for TypeMap {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Iteration in unstable order - but that's fine because this is just for diagnostics
        for (name, span, ty) in self.bindings.values() {
            writeln!(
                f,
                "{} ({}) = {}",
                name,
                FileSpanRef {
                    file: &self.codemap,
                    span: *span
                },
                ty
            )?;
        }
        Ok(())
    }
}

impl AstModule {
    /// Typecheck a module
    pub fn typecheck(
        self,
        oracle: &dyn TypingOracle,
        globals: &Globals,
        loads: &HashMap<String, Interface>,
    ) -> (Vec<anyhow::Error>, TypeMap, Interface, Vec<Approximation>) {
        let codemap = self.codemap.dupe();
        let names = MutableNames::new();
        let frozen_heap = FrozenHeap::new();
        let (cst, scope) = ModuleScopes::enter_module(
            &names,
            &frozen_heap,
            loads,
            self.statement,
            frozen_heap.alloc_any_display_from_debug(globals.dupe()),
            frozen_heap.alloc_any_display_from_debug(self.codemap.dupe()),
            &Dialect::Extended,
        );
        let bindings = BindingsCollect::collect(&cst);
        let mut approximations = bindings.approximations;
        let (solve_errors, types, solve_approximations) =
            solve_bindings(oracle, bindings.bindings, &codemap);

        approximations.extend(solve_approximations);

        let mut typemap = HashMap::with_capacity(types.len());
        for (id, ty) in &types {
            let binding = scope.scope_data.get_binding(*id);
            let name = binding.name.as_str().to_owned();
            let span = binding.span.unwrap_or_default();
            typemap.insert(*id, (name, span, ty.clone()));
        }
        let typemap = TypeMap {
            bindings: typemap,
            codemap: codemap.dupe(),
        };

        let errors = scope
            .errors
            .into_iter()
            .chain(solve_errors)
            .map(EvalException::into_anyhow)
            .collect();

        let mut res = HashMap::new();
        for (name, vis) in names.all_names_and_visibilities() {
            if vis == Visibility::Public {
                let ty = types[scope.module_bindings.get(name.as_str()).unwrap()].clone();
                res.insert(name.as_str().to_owned(), ty);
            }
        }
        let interface = Interface::new(res);

        (errors, typemap, interface, approximations)
    }
}
