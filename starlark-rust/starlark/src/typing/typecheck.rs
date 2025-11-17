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
use starlark_map::unordered_map::UnorderedMap;
use starlark_syntax::slice_vec_ext::VecExt;
use starlark_syntax::syntax::ast::Visibility;
use starlark_syntax::syntax::module::AstModuleFields;
use starlark_syntax::syntax::top_level_stmts::top_level_stmts_mut;

use crate::codemap::CodeMap;
use crate::codemap::FileSpanRef;
use crate::codemap::Span;
use crate::codemap::Spanned;
use crate::environment::Globals;
use crate::environment::names::MutableNames;
use crate::eval::compiler::scope::BindingId;
use crate::eval::compiler::scope::BindingSource;
use crate::eval::compiler::scope::ModuleScopes;
use crate::eval::compiler::scope::payload::CstStmt;
use crate::eval::compiler::scope::scope_resolver_globals::ScopeResolverGlobals;
use crate::syntax::AstModule;
use crate::syntax::Dialect;
use crate::typing::bindings::Bindings;
use crate::typing::bindings::BindingsCollect;
use crate::typing::ctx::BindingType;
use crate::typing::ctx::TypingContext;
use crate::typing::error::InternalError;
use crate::typing::error::TypingError;
use crate::typing::error::TypingOrInternalError;
use crate::typing::fill_types_for_lint::ModuleVarTypes;
use crate::typing::fill_types_for_lint::fill_types_for_lint_typechecker;
use crate::typing::interface::Interface;
use crate::typing::mode::TypecheckMode;
use crate::typing::oracle::ctx::TypingOracleCtx;
use crate::typing::ty::Approximation;
use crate::typing::ty::Ty;
use crate::values::FrozenHeap;

/// Recursive function type-checker.
///
/// You can call `solve_bindings` on as big or as little input as you like, but it
/// is O(n * m) where n is the number of expressions assigned to bindings, and m is
/// a measure of complexity of the assignments (`a=1; b=a; b=""; c=b; a=c` is
/// deliberately complex). If you lump all bindings in an entire module together and
/// solve them all at once, `m` is set to the most complex function and `n` is large.
///
/// Fortunately, because of variable shadowing and no `global` keyword, we only ever
/// need to look at a function body to determine the types of its local bindings from
/// the assignments made to it.
///
/// ```python
/// x = 5
/// def child():
///     x = "string"   # completely different binding, no relation to outer `x`
/// ```
///
/// In some circumstances child scopes could influence parent scopes via mutation
/// methods, but it's not a big loss if we don't infer in these cases.
///
/// ```python
/// xs = list()
/// def child():
///     xs.push(5)  # could influence type of xs, but we will not support this
/// ```
///
/// So we can solve bindings for every scope individually. That's what this does.
/// As for ordering, a parent scope is finished and solved before we check any of
/// its child scopes, so child scopes have access to parent binding solutions.
///
/// ```python
/// x = 5
/// def child():
///     y = x    # y solves to `int | str`
/// x = "string"
/// ```
///
pub(crate) struct TypeChecker<'a> {
    pub(crate) oracle: TypingOracleCtx<'a>,
    pub(crate) typecheck_mode: TypecheckMode,
    pub(crate) module_var_types: &'a ModuleVarTypes,
    pub(crate) approximations: &'a mut Vec<Approximation>,
    pub(crate) all_solved_types: HashMap<BindingId, Ty>,
}

impl<'a> TypeChecker<'a> {
    fn codemap(&self) -> &'a CodeMap {
        self.oracle.codemap
    }

    /// Typecheck an entire module.
    ///
    /// To just immediately return on encountering a type error, pass `&mut Err` as the error
    /// handler. To collect errors and continue, be sure to return Ok from the error handler.
    pub(crate) fn check_module_scope(
        &mut self,
        module: &CstStmt,
        eh: &mut dyn FnMut(TypingError) -> Result<(), TypingError>,
    ) -> Result<(), TypingOrInternalError> {
        self.check_scope(module, &Ty::any(), &HashMap::default(), eh)
    }

    /// Recursive scope check. Checks the scope's bindings, and then all child defs.
    pub(crate) fn check_scope(
        &mut self,
        body: &CstStmt,
        return_type: &Ty,
        visible: &HashMap<BindingId, Ty>,
        eh: &mut dyn FnMut(TypingError) -> Result<(), TypingError>,
    ) -> Result<(), TypingOrInternalError> {
        let mut children = Vec::new();
        let bindings = BindingsCollect::collect_scope(
            body,
            return_type,
            visible,
            self.typecheck_mode,
            self.codemap(),
            self.approximations,
            &mut children,
        )?;
        let (errors, solved, mut approx) =
            solve_bindings(bindings, self.oracle, self.module_var_types)?;

        self.approximations.append(&mut approx);

        for error in errors {
            eh(error)?;
        }

        // Save all solved types to the output
        let solved_copy = solved.iter().map(|(&b, ty)| (b, ty.dupe()));
        self.all_solved_types.extend(solved_copy.clone());

        for child in children {
            let mut child_visible = visible.clone();
            child_visible.extend(solved_copy.clone());
            child_visible.extend(child.param_types);
            self.check_scope(child.body, &child.return_type, &child_visible, eh)?;
        }
        Ok(())
    }
}

// Things which are None in the map have type void - they are never constructed
pub(crate) fn solve_bindings(
    mut bindings: Bindings,
    oracle: TypingOracleCtx,
    module_var_types: &ModuleVarTypes,
) -> Result<(Vec<TypingError>, HashMap<BindingId, Ty>, Vec<Approximation>), InternalError> {
    let mut types: UnorderedMap<BindingId, BindingType> = UnorderedMap::new();

    // No need to (expensively) solve over bound expressions where the binding's type
    // is already provided by user. Move these into check_type, to check all assignments
    // match the type annotation.
    for (k, ty) in bindings.types {
        types.insert(k, BindingType::Annotated(ty.clone()));

        if let Some(exprs) = bindings.expressions.get_mut(&k) {
            for expr in std::mem::take(exprs) {
                bindings
                    .check_type
                    .push((expr.span(), Some(expr), ty.dupe()));
            }
        }
    }
    // So we don't have to shift_remove N times, just call retain at the end
    bindings.expressions.retain(|_, exprs| !exprs.is_empty());

    // Initialize unsolved types
    bindings.expressions.keys().for_each(|x| {
        types.insert(*x, BindingType::Solver(Ty::never()));
    });

    // FIXME: Should be a fixed point, just do 10 iterations since that probably converges
    let mut changed = false;
    let mut ctx = TypingContext {
        oracle,
        errors: RefCell::new(Vec::new()),
        approximoations: RefCell::new(Vec::new()),
        types,
        module_var_types,
    };
    const ITERATIONS: usize = 100;

    for _iteration in 0..ITERATIONS {
        changed = false;
        ctx.errors.borrow_mut().clear();
        for (name, exprs) in &bindings.expressions {
            for expr in exprs {
                let ty = ctx.expression_bind_type(expr)?;
                let BindingType::Solver(t) = ctx.types.get_mut(name).unwrap() else {
                    // unreachable
                    continue;
                };
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
        ctx.expression_type(x)?;
    }
    for (span, e, require) in &bindings.check_type {
        let ty = match e {
            None => Ty::none(),
            Some(x) => ctx.expression_bind_type(x)?,
        };
        ctx.validate_type(
            Spanned {
                node: &ty,
                span: *span,
            },
            require,
        )?;
    }
    // Put binding errors first as the compiler fails with the first error and otherwise they
    // never show up in tests.
    bindings.errors.append(&mut ctx.errors.into_inner());
    Ok((
        bindings.errors,
        ctx.types
            .into_entries_unordered()
            .map(|(k, v)| (k, v.into_inner()))
            .collect(),
        ctx.approximoations.into_inner(),
    ))
}

/// Structure containing all the inferred types.
#[derive(Debug)]
pub struct TypeMap {
    codemap: CodeMap,
    bindings: UnorderedMap<BindingId, (String, Span, Ty)>,
}

impl Display for TypeMap {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Iteration in unstable order - but that's fine because this is just for diagnostics
        for (_binding_id, (name, span, ty)) in self.bindings.entries_sorted() {
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

impl TypeMap {
    #[cfg(test)]
    pub(crate) fn find_bindings_by_name<'a>(&'a self, name: &str) -> Vec<&'a Ty> {
        self.bindings
            .entries_sorted()
            .into_iter()
            .filter_map(
                |(_binding_id, (n, _span, ty))| {
                    if name == n { Some(ty) } else { None }
                },
            )
            .collect()
    }

    #[cfg(test)]
    pub(crate) fn find_first_binding<'a>(&'a self) -> Option<&'a Ty> {
        self.bindings
            .entries_unordered()
            .min_by_key(|(id, _)| *id)
            .map(|(_, (_, _, ty))| ty)
    }
}

/// Typecheck a module.
pub trait AstModuleTypecheck {
    /// Typecheck a module.
    fn typecheck(
        self,
        globals: &Globals,
        loads: &HashMap<String, Interface>,
    ) -> (Vec<crate::Error>, TypeMap, Interface, Vec<Approximation>);
}

impl AstModuleTypecheck for AstModule {
    fn typecheck(
        self,
        globals: &Globals,
        loads: &HashMap<String, Interface>,
    ) -> (Vec<crate::Error>, TypeMap, Interface, Vec<Approximation>) {
        let (codemap, statement, _dialect, _) = self.into_parts();
        let names = MutableNames::new();
        let frozen_heap = FrozenHeap::new();
        let (
            scope_errors,
            ModuleScopes {
                mut cst,
                scope_data,
                ..
            },
        ) = ModuleScopes::check_module(
            &names,
            &frozen_heap,
            loads,
            statement,
            ScopeResolverGlobals {
                globals: Some(frozen_heap.alloc_any(globals.dupe())),
            },
            frozen_heap.alloc_any(codemap.dupe()),
            &Dialect::AllOptionsInternal,
        );
        let scope_errors = scope_errors.into_map(TypingError::from_eval_exception);
        // We don't really need to properly unpack top-level statements,
        // but make it safe against future changes.
        let mut cst_toplevel: Vec<&mut CstStmt> = top_level_stmts_mut(&mut cst);
        let oracle = TypingOracleCtx { codemap: &codemap };

        let mut approximations = Vec::new();
        let (fill_types_errors, module_var_types) = match fill_types_for_lint_typechecker(
            &mut cst_toplevel,
            oracle,
            &scope_data,
            &mut approximations,
        ) {
            Ok(fill_types_errors) => fill_types_errors,
            Err(e) => {
                return (
                    vec![InternalError::into_error(e)],
                    TypeMap {
                        codemap,
                        bindings: UnorderedMap::new(),
                    },
                    Interface::default(),
                    Vec::new(),
                );
            }
        };

        let mut typemap = UnorderedMap::new();
        let oracle = TypingOracleCtx { codemap: &codemap };
        let mut type_checker = TypeChecker {
            oracle,
            typecheck_mode: TypecheckMode::Lint,
            module_var_types: &module_var_types,
            approximations: &mut approximations,
            all_solved_types: HashMap::new(),
        };
        let mut all_solve_errors = Vec::new();

        let mut error_handler = |error| {
            all_solve_errors.push(error);
            // continue checking
            Ok(())
        };
        if let Err(unrecoverable) = type_checker.check_module_scope(&cst, &mut error_handler) {
            // This is generally an internal error, since most typing errors are handled
            // by error_handler. Except some type errors that can't (yet?) be recovered.
            return (
                vec![unrecoverable.into_error()],
                TypeMap {
                    codemap,
                    bindings: UnorderedMap::new(),
                },
                Interface::default(),
                Vec::new(),
            );
        }

        for (id, ty) in &type_checker.all_solved_types {
            let binding = scope_data.get_binding(*id);
            let name = binding.name.as_str().to_owned();
            let span = match binding.source {
                BindingSource::Source(span) => span,
                BindingSource::FromModule => Span::default(),
            };
            typemap.insert(*id, (name, span, ty.clone()));
        }

        let typemap = TypeMap {
            bindings: typemap,
            codemap: codemap.dupe(),
        };

        let errors = [scope_errors, fill_types_errors, all_solve_errors]
            .into_iter()
            .flatten()
            .map(TypingError::into_error)
            .collect();

        let mut res = HashMap::new();
        for (name, module_slot_id, vis) in names.all_names_slots_and_visibilities() {
            if vis == Visibility::Public {
                let ty = module_var_types
                    .types
                    .get(&module_slot_id)
                    .cloned()
                    .unwrap_or_else(Ty::any);
                res.insert(name.as_str().to_owned(), ty);
            }
        }
        let interface = Interface::new(res);

        (errors, typemap, interface, approximations)
    }
}
