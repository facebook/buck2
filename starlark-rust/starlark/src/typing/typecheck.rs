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
use gazebo::prelude::*;

use crate::codemap::CodeMap;
use crate::codemap::FileSpanRef;
use crate::codemap::Span;
use crate::environment::names::MutableNames;
use crate::environment::Globals;
use crate::eval::compiler::scope::BindingId;
use crate::eval::compiler::scope::CompilerAstMap;
use crate::eval::compiler::scope::CstStmt;
use crate::eval::compiler::scope::Scope;
use crate::eval::compiler::scope::ScopeData;
use crate::syntax::ast::Visibility;
use crate::syntax::AstModule;
use crate::syntax::Dialect;
use crate::typing::bindings::*;
use crate::typing::ctx::*;
use crate::typing::oracle::traits::TypingOracle;
use crate::typing::ty::*;
use crate::values::FrozenHeap;
use crate::values::FrozenRef;

/// Give every identifier a unique name, using dotted names to note values in nested scopes
fn unique_identifiers<'f>(
    frozen_heap: &'f FrozenHeap,
    ast: AstModule,
    names: &'f MutableNames,
) -> (CstStmt, Scope<'f>) {
    let mut scope_data = ScopeData::new();
    let root_scope_id = scope_data.new_scope().0;
    let mut cst = ast
        .statement
        .into_map_payload(&mut CompilerAstMap(&mut scope_data));
    let codemap = frozen_heap.alloc_any_display_from_debug(ast.codemap.dupe());
    let scope = Scope::enter_module(
        names,
        frozen_heap,
        root_scope_id,
        scope_data,
        &mut cst,
        FrozenRef::new(Globals::empty()),
        codemap,
        &Dialect::Extended,
    );
    (cst, scope)
}

// Things which are None in the map have type void - they are never constructed
fn solve_bindings(
    oracle: &dyn TypingOracle,
    bindings: Bindings,
    codemap: &CodeMap,
) -> (Vec<TypingError>, HashMap<BindingId, Ty>, Vec<Approximation>) {
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
        codemap: codemap.dupe(),
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
        loads: &HashMap<String, Interface>,
    ) -> (Vec<anyhow::Error>, TypeMap, Interface, Vec<Approximation>) {
        let codemap = self.codemap.dupe();
        let names = MutableNames::new();
        let frozen_heap = FrozenHeap::new();
        let (cst, scope) = unique_identifiers(&frozen_heap, self, &names);
        let bindings = Bindings::collect(&cst, loads);
        let descriptions = bindings.descriptions.clone();
        let mut approximations = bindings.approximations.clone();
        let (errors, types, solve_approximations) = solve_bindings(oracle, bindings, &codemap);

        approximations.extend(solve_approximations);

        let mut typemap = HashMap::with_capacity(types.len());
        for (id, ty) in &types {
            let (name, span) = match descriptions.get(id) {
                None => ("{unknown}".to_owned(), Span::default()),
                Some(i) => (i.0.clone(), i.span),
            };
            typemap.insert(*id, (name, span, ty.clone()));
        }
        let typemap = TypeMap {
            bindings: typemap,
            codemap: codemap.dupe(),
        };

        let errors = errors.into_map(|x| anyhow::anyhow!(x));

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
