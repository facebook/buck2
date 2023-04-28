/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::cells::name::CellName;
use buck2_core::cells::CellResolver;
use buck2_core::pattern::ParsedPattern;
use buck2_node::visibility::VisibilityPattern;
use buck2_node::visibility::VisibilitySpecification;
use buck2_node::visibility::WithinViewSpecification;
use starlark::environment::GlobalsBuilder;
use starlark::eval::Evaluator;
use starlark::starlark_module;
use starlark::values::none::NoneType;

use crate::interpreter::build_context::BuildContext;
use crate::interpreter::build_context::PerFileTypeContext;
use crate::super_package::eval_ctx::PackageFileVisibilityFields;

#[derive(Debug, thiserror::Error)]
enum PackageFileError {
    #[error(
        "`package()` can only be called in `PACKAGE` files \
        or in `bzl` files included from `PACKAGE` files"
    )]
    NotPackage,
    #[error("`package()` function can be used at most once per `PACKAGE` file")]
    AtMostOnce,
}

fn parse_visibility(
    patterns: &[String],
    cell_name: CellName,
    cell_resolver: &CellResolver,
) -> anyhow::Result<VisibilitySpecification> {
    let mut specs: Option<Vec<_>> = None;
    for pattern in patterns {
        if pattern == VisibilityPattern::PUBLIC {
            return Ok(VisibilitySpecification::Public);
        }
        specs
            .get_or_insert_with(|| Vec::with_capacity(patterns.len()))
            .push(VisibilityPattern(ParsedPattern::parse_precise(
                pattern,
                cell_name,
                cell_resolver,
            )?));
    }
    Ok(match specs {
        Some(specs) => VisibilitySpecification::VisibleTo(specs.into_iter().collect()),
        None => VisibilitySpecification::Default,
    })
}

fn parse_within_view(
    patterns: &[String],
    cell_name: CellName,
    cell_resolver: &CellResolver,
) -> anyhow::Result<WithinViewSpecification> {
    let mut specs: Option<Vec<_>> = None;
    for pattern in patterns {
        if pattern == VisibilityPattern::PUBLIC {
            return Ok(WithinViewSpecification::Public);
        }
        specs
            .get_or_insert_with(|| Vec::with_capacity(patterns.len()))
            .push(VisibilityPattern(ParsedPattern::parse_precise(
                pattern,
                cell_name,
                cell_resolver,
            )?));
    }
    Ok(match specs {
        Some(specs) => WithinViewSpecification::VisibleTo(specs.into_iter().collect()),
        None => WithinViewSpecification::Default,
    })
}

/// Globals for `PACKAGE` files and `bzl` files included from `PACKAGE` files.
#[starlark_module]
pub(crate) fn register_package_function(globals: &mut GlobalsBuilder) {
    fn package(
        #[starlark(require=named, default=false)] inherit: bool,
        #[starlark(require=named, default=Vec::new())] visibility: Vec<String>,
        #[starlark(require=named, default=Vec::new())] within_view: Vec<String>,
        eval: &mut Evaluator,
    ) -> anyhow::Result<NoneType> {
        let build_context = BuildContext::from_context(eval)?;
        let package_file_eval_ctx = match &build_context.additional {
            PerFileTypeContext::Package(_, package_file_eval_ctx) => package_file_eval_ctx,
            _ => return Err(PackageFileError::NotPackage.into()),
        };
        let visibility = parse_visibility(
            &visibility,
            build_context.cell_info().name().name(),
            build_context.cell_info().cell_resolver(),
        )?;
        let within_view = parse_within_view(
            &within_view,
            build_context.cell_info().name().name(),
            build_context.cell_info().cell_resolver(),
        )?;

        match &mut *package_file_eval_ctx.visibility.borrow_mut() {
            Some(_) => return Err(PackageFileError::AtMostOnce.into()),
            x => {
                *x = Some(PackageFileVisibilityFields {
                    visibility,
                    within_view,
                    inherit,
                })
            }
        };

        Ok(NoneType)
    }
}
