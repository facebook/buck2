/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_interpreter::path::StarlarkPath;
use buck2_interpreter_for_build::interpreter::build_context::BuildContext;
use buck2_interpreter_for_build::interpreter::functions::host_info::register_host_info;
use buck2_interpreter_for_build::interpreter::functions::read_config::register_read_config;
use buck2_interpreter_for_build::interpreter::natives::register_module_natives;
use buck2_interpreter_for_build::super_package::package_value::register_read_package_value;
use either::Either;
use itertools::Itertools;
use starlark::collections::SmallMap;
use starlark::docs::DocString;
use starlark::docs::DocStringKind;
use starlark::environment::GlobalsBuilder;
use starlark::eval::Evaluator;
use starlark::values::Value;
use starlark_map::small_set::SmallSet;

use crate::interpreter::rule_defs::provider::callable::UserProviderCallable;
use crate::interpreter::rule_defs::transitive_set::TransitiveSetDefinition;
use crate::interpreter::rule_defs::transitive_set::TransitiveSetError;
use crate::interpreter::rule_defs::transitive_set::TransitiveSetOperations;
use crate::interpreter::rule_defs::transitive_set::TransitiveSetProjectionKind;
use crate::interpreter::rule_defs::transitive_set::TransitiveSetProjectionSpec;

#[derive(Debug, thiserror::Error)]
enum NativesError {
    #[error("non-unique field names: [{}]", .0.iter().map(|s| format!("`{}`", s)).join(", "))]
    NonUniqueFields(Vec<String>),
    #[error("`transitive_set()` can only be used in `bzl` files")]
    TransitiveSetOnlyInBzl,
}

#[starlark_module]
pub fn register_provider(builder: &mut GlobalsBuilder) {
    /// Create a `"provider"` type that can be returned from `rule` implementations.
    /// Used to pass information from a rule to the things that depend on it.
    /// Typically named with an `Info` suffix.
    ///
    /// ```python
    /// GroovyLibraryInfo(fields = [
    ///     "objects",  # a list of artifacts
    ///     "options",  # a string containing compiler options
    /// ])
    /// ```
    ///
    /// Given a dependency you can obtain the provider with `my_dep[GroovyLibraryInfo]`
    /// which returns either `None` or a value of type `GroovyLibraryInfo`.
    ///
    /// For providers that accumulate upwards a transitive set is often a good choice.
    fn provider(
        #[starlark(require=named, default = "")] doc: &str,
        #[starlark(require=named)] fields: Either<Vec<String>, SmallMap<&str, &str>>,
        eval: &mut Evaluator,
    ) -> anyhow::Result<UserProviderCallable> {
        let docstring = DocString::from_docstring(DocStringKind::Starlark, doc);
        let path = BuildContext::from_context(eval)?.starlark_path().path();

        let (field_names, field_docs) = match fields {
            Either::Left(f) => {
                let docs = vec![None; f.len()];
                let field_names: SmallSet<String> = f.iter().cloned().collect();
                if field_names.len() != f.len() {
                    return Err(NativesError::NonUniqueFields(f).into());
                }
                (field_names, docs)
            }
            Either::Right(fields_with_docs) => {
                let mut field_names = SmallSet::with_capacity(fields_with_docs.len());
                let mut field_docs = Vec::with_capacity(fields_with_docs.len());
                for (name, docs) in fields_with_docs {
                    let inserted = field_names.insert(name.to_owned());
                    assert!(inserted);
                    field_docs.push(DocString::from_docstring(DocStringKind::Starlark, docs));
                }
                (field_names, field_docs)
            }
        };
        Ok(UserProviderCallable::new(
            path.into_owned(),
            docstring,
            field_docs,
            field_names,
        ))
    }
}

#[starlark_module]
pub fn register_transitive_set(builder: &mut GlobalsBuilder) {
    fn transitive_set<'v>(
        args_projections: Option<SmallMap<String, Value<'v>>>,
        json_projections: Option<SmallMap<String, Value<'v>>>,
        reductions: Option<SmallMap<String, Value<'v>>>,
        eval: &mut Evaluator,
    ) -> anyhow::Result<TransitiveSetDefinition<'v>> {
        let build_context = BuildContext::from_context(eval)?;
        // TODO(cjhopman): Reductions could do similar signature checking.
        let projections: SmallMap<_, _> = args_projections
            .into_iter()
            .flat_map(|v| v.into_iter())
            .map(|(k, v)| {
                (
                    k,
                    TransitiveSetProjectionSpec {
                        kind: TransitiveSetProjectionKind::Args,
                        projection: v,
                    },
                )
            })
            .chain(
                json_projections
                    .into_iter()
                    .flat_map(|v| v.into_iter())
                    .map(|(k, v)| {
                        (
                            k,
                            TransitiveSetProjectionSpec {
                                kind: TransitiveSetProjectionKind::Json,
                                projection: v,
                            },
                        )
                    }),
            )
            .collect();

        // Both kinds of projections take functions with the same signature.
        for (name, spec) in projections.iter() {
            // We should probably be able to require that the projection returns a parameters_spec, but
            // we don't depend on this type-checking and we'd just error out later when calling it if it
            // were wrong.
            if let Some(v) = spec.projection.parameters_spec() {
                if v.len() != 1 {
                    return Err(TransitiveSetError::ProjectionSignatureError {
                        name: name.clone(),
                    }
                    .into());
                }
            };
        }

        Ok(TransitiveSetDefinition::new(
            match build_context.starlark_path() {
                StarlarkPath::LoadFile(import_path) => import_path.clone(),
                _ => return Err(NativesError::TransitiveSetOnlyInBzl.into()),
            },
            TransitiveSetOperations {
                projections,
                reductions: reductions.unwrap_or_default(),
            },
        ))
    }
}

/// Natives for `BUCK` and `bzl` files.
pub(crate) fn register_build_bzl_natives(builder: &mut GlobalsBuilder) {
    register_provider(builder);
    register_transitive_set(builder);
    register_module_natives(builder);
    register_host_info(builder);
    register_read_config(builder);
    register_read_package_value(builder);
}
