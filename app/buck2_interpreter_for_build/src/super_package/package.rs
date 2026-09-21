/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::cell::RefCell;

use buck2_core::cells::CellAliasResolver;
use buck2_core::cells::CellResolver;
use buck2_core::cells::name::CellName;
use buck2_core::pattern::pattern::ParsedPattern;
use buck2_interpreter::paths::package::PackageFilePath;
use buck2_node::visibility::StarlarkTargetNameGlob;
use buck2_node::visibility::VisibilityPattern;
use buck2_node::visibility::VisibilitySpecification;
use buck2_node::visibility::VisibilityWithinViewBuilder;
use buck2_node::visibility::WithinViewSpecification;
use either::Either;
use starlark::environment::GlobalsBuilder;
use starlark::eval::Evaluator;
use starlark::starlark_module;
use starlark::values::list_or_tuple::UnpackListOrTuple;
use starlark::values::none::NoneOr;
use starlark::values::none::NoneType;

use crate::interpreter::build_context::BuildContext;
use crate::super_package::eval_ctx::PackageFileEvalCtx;
use crate::super_package::eval_ctx::PackageFileVisibilityFields;

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
enum PackageFileError {
    #[error("`package()` function can be used at most once per `PACKAGE` file")]
    AtMostOnce,
    #[error("`{0}()` function can be used at most once per `PACKAGE` file")]
    EnforceIntersectionAtMostOnce(&'static str),
    #[error("`{0}()` can only be called from a `PACKAGE` file")]
    EnforceIntersectionMustBeDirect(&'static str),
}

fn add_visibility_pattern<'v>(
    builder: &mut VisibilityWithinViewBuilder,
    value: Either<&'v str, &'v StarlarkTargetNameGlob>,
    cell_name: CellName,
    cell_resolver: &CellResolver,
    cell_alias_resolver: &CellAliasResolver,
) -> buck2_error::Result<()> {
    match value {
        Either::Left(s) => {
            if s == VisibilityPattern::PUBLIC {
                builder.add_public();
            } else {
                builder.add(VisibilityPattern::Parsed(ParsedPattern::parse_precise(
                    s,
                    cell_name,
                    cell_resolver,
                    cell_alias_resolver,
                )?));
            }
        }
        Either::Right(target_name_glob) => {
            let record = target_name_glob.coerce(|s| {
                ParsedPattern::parse_precise(s, cell_name, cell_resolver, cell_alias_resolver)
            })?;
            builder.add(VisibilityPattern::TargetNameGlob(record));
        }
    }
    Ok(())
}

fn parse_visibility<'v>(
    patterns: &[Either<&'v str, &'v StarlarkTargetNameGlob>],
    cell_name: CellName,
    cell_resolver: &CellResolver,
    cell_alias_resolver: &CellAliasResolver,
) -> buck2_error::Result<VisibilitySpecification> {
    let mut builder = VisibilityWithinViewBuilder::with_capacity(patterns.len());
    for &pattern in patterns {
        add_visibility_pattern(
            &mut builder,
            pattern,
            cell_name,
            cell_resolver,
            cell_alias_resolver,
        )?;
    }
    Ok(builder.build_visibility())
}

fn parse_within_view<'v>(
    patterns: &[Either<&'v str, &'v StarlarkTargetNameGlob>],
    cell_name: CellName,
    cell_resolver: &CellResolver,
    cell_alias_resolver: &CellAliasResolver,
) -> buck2_error::Result<WithinViewSpecification> {
    let mut builder = VisibilityWithinViewBuilder::with_capacity(patterns.len());
    for &pattern in patterns {
        add_visibility_pattern(
            &mut builder,
            pattern,
            cell_name,
            cell_resolver,
            cell_alias_resolver,
        )?;
    }
    Ok(builder.build_within_view())
}

/// Shared body of the `enforce_*_intersection()` functions: the call must come
/// directly from a `PACKAGE` file (not from a `bzl` it loads) and may happen at
/// most once per file; on success the file's opt-in flag is set.
fn enforce_intersection(
    eval: &mut Evaluator,
    function_name: &'static str,
    opted_in: fn(&PackageFileEvalCtx) -> &RefCell<bool>,
) -> starlark::Result<NoneType> {
    let build_context = BuildContext::from_context(eval)?;
    let package_file_eval_ctx = build_context
        .additional
        .require_package_file(function_name)?;

    let direct_package_call = eval.call_stack_top_location().is_some_and(|loc| {
        let filename = std::path::Path::new(loc.filename());
        PackageFilePath::package_file_names().any(|pkg| filename.ends_with(pkg))
    });
    if !direct_package_call {
        return Err(
            buck2_error::Error::from(PackageFileError::EnforceIntersectionMustBeDirect(
                function_name,
            ))
            .into(),
        );
    }

    let mut enforces = opted_in(package_file_eval_ctx).borrow_mut();
    if *enforces {
        return Err(
            buck2_error::Error::from(PackageFileError::EnforceIntersectionAtMostOnce(
                function_name,
            ))
            .into(),
        );
    }
    *enforces = true;
    Ok(NoneType)
}

/// Globals for `PACKAGE` files and `bzl` files included from `PACKAGE` files.
#[starlark_module]
pub(crate) fn register_package_function(globals: &mut GlobalsBuilder) {
    /// DO NOT USE THIS FUNCTION!
    ///
    /// It controls which test config to use in downstream systems. Mostly likely you don't want to specify it by yourself.
    fn test_config_unification_rollout(
        enabled: bool,
        eval: &mut Evaluator,
    ) -> starlark::Result<NoneType> {
        let build_context = BuildContext::from_context(eval)?;
        let package_file_eval_ctx = build_context.additional.require_package_file("package")?;
        *package_file_eval_ctx
            .test_config_unification_rollout
            .borrow_mut() = Some(enabled);
        Ok(NoneType)
    }

    fn package<'v>(
        #[starlark(require=named, default=false)] inherit: bool,
        #[starlark(require=named, default=NoneOr::None)] visibility: NoneOr<
            UnpackListOrTuple<Either<&'v str, &'v StarlarkTargetNameGlob>>,
        >,
        #[starlark(require=named, default=UnpackListOrTuple::default())]
        within_view: UnpackListOrTuple<Either<&'v str, &'v StarlarkTargetNameGlob>>,
        eval: &mut Evaluator,
    ) -> starlark::Result<NoneType> {
        let build_context = BuildContext::from_context(eval)?;
        let package_file_eval_ctx = build_context.additional.require_package_file("package")?;
        let visibility_provided = visibility.into_option();
        let visibility = parse_visibility(
            visibility_provided
                .as_ref()
                .map_or(&[], |v| v.items.as_slice()),
            build_context.cell_info().name().name(),
            build_context.cell_info().cell_resolver(),
            build_context.cell_info().cell_alias_resolver(),
        )?;
        let within_view = parse_within_view(
            &within_view.items,
            build_context.cell_info().name().name(),
            build_context.cell_info().cell_resolver(),
            build_context.cell_info().cell_alias_resolver(),
        )?;

        match &mut *package_file_eval_ctx.visibility.borrow_mut() {
            Some(_) => return Err(buck2_error::Error::from(PackageFileError::AtMostOnce).into()),
            x => {
                *x = Some(PackageFileVisibilityFields {
                    visibility,
                    within_view,
                    inherit,
                    visibility_was_set: visibility_provided.is_some(),
                })
            }
        };

        Ok(NoneType)
    }

    /// Opts this PACKAGE and its descendants into intersection-based
    /// visibility: every target's effective visibility is ANDed with a
    /// propagating cap built from each opted-in ancestor PACKAGE's
    /// explicit `package(visibility=...)` list. `"PUBLIC"` is the
    /// identity, so `visibility=["PUBLIC"]` targets are silently clipped
    /// rather than rejected. Calling this without a non-`None`
    /// `package(visibility=...)` (omitted or `visibility=None`) adds
    /// nothing to the cap — the parent's cap propagates unchanged.
    ///
    /// Can only be called from a `PACKAGE` file.
    fn enforce_visibility_intersection(eval: &mut Evaluator) -> starlark::Result<NoneType> {
        enforce_intersection(eval, "enforce_visibility_intersection", |ctx| {
            &ctx.enforces_visibility_intersection
        })
    }

    /// Opts this PACKAGE and its descendants into intersection-based
    /// `within_view`: every target's effective `within_view` is ANDed with a
    /// propagating cap built from each opted-in ancestor PACKAGE's
    /// `package(within_view=...)` list. The cap only tightens: a target
    /// declaring a broader `within_view`, or a descendant PACKAGE
    /// replacing the inherited default with `within_view=["PUBLIC"]`, still
    /// cannot depend on anything outside the cap. `"PUBLIC"` is the
    /// identity, so calling this without `package(within_view=...)` adds
    /// nothing to the cap — the parent's cap propagates unchanged.
    ///
    /// Can only be called from a `PACKAGE` file.
    fn enforce_within_view_intersection(eval: &mut Evaluator) -> starlark::Result<NoneType> {
        enforce_intersection(eval, "enforce_within_view_intersection", |ctx| {
            &ctx.enforces_within_view_intersection
        })
    }
}
