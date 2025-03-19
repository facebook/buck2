/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use buck2_build_api_derive::internal_provider;
use buck2_core::cells::name::CellName;
use buck2_core::cells::CellAliasResolver;
use buck2_core::cells::CellResolver;
use buck2_core::error::validate_logview_category;
use buck2_core::pattern::pattern::ParsedPattern;
use buck2_core::pattern::pattern_type::TargetPatternExtra;
use buck2_util::arc_str::ArcStr;
use dupe::Dupe;
use starlark::any::ProvidesStaticType;
use starlark::collections::SmallMap;
use starlark::environment::GlobalsBuilder;
use starlark::values::dict::DictRef;
use starlark::values::dict::DictType;
use starlark::values::list::ListRef;
use starlark::values::list::ListType;
use starlark::values::Coerce;
use starlark::values::Freeze;
use starlark::values::FreezeResult;
use starlark::values::Trace;
use starlark::values::ValueLifetimeless;
use starlark::values::ValueLike;
use starlark::values::ValueOf;
use starlark::values::ValueOfUncheckedGeneric;

use crate as buck2_build_api;

/// A provider for defining custom soft error categories for dep-only incompatible targets.
/// This can be used to get finer-grained data on whether it is safe to enable
/// `error_on_dep_only_incompatible` for a given target.
///
/// To use this, first define rule returning this provider.
/// ```python   
/// def _impl(ctx: AnalysisContext) -> list[Provider]:
///    return [
///       DefaultInfo(),
///       DepOnlyIncompatibleInfo(custom_soft_errors = {
///          "dep_only_incompatible_foo": ["root//foo/..."],
///          "dep_only_incompatible_bar": ["root//bar/..."],
///       })
///    ]
/// ```
///
/// Then register a target of this rule in root .buckconfig under `buck2.dep_only_incompatible_info`
/// buckconfig key. Once registered, soft errors will be fired under category "dep_only_incompatible_foo"
/// when a target in `root//foo/...` is dep-only incompatible and likewise `dep_only_incompatible_bar` for
/// a target in `root//bar/...`.
#[internal_provider(dep_only_incompatible_info_creator)]
#[derive(Clone, Debug, Trace, Coerce, Freeze, ProvidesStaticType, Allocative)]
#[repr(C)]
pub struct DepOnlyIncompatibleInfoGen<V: ValueLifetimeless> {
    pub custom_soft_errors: ValueOfUncheckedGeneric<V, DictType<String, ListType<String>>>,
}

#[starlark_module]
fn dep_only_incompatible_info_creator(globals: &mut GlobalsBuilder) {
    #[starlark(as_type = FrozenDepOnlyIncompatibleInfo)]
    fn DepOnlyIncompatibleInfo<'v>(
        #[starlark(require = named)] custom_soft_errors: ValueOf<
            'v,
            DictType<&'v str, ListType<&'v str>>,
        >,
    ) -> starlark::Result<DepOnlyIncompatibleInfo<'v>> {
        let custom_soft_errors_dict = DictRef::from_value(custom_soft_errors.to_value())
            .ok_or_else(|| {
                starlark::Error::from(anyhow::anyhow!("Expected dict via type checking"))
                    .into_internal_error()
            })?;
        for category in custom_soft_errors_dict.keys() {
            let category = category.to_value().unpack_str().ok_or_else(|| {
                starlark::Error::from(anyhow::anyhow!("Expected string via type checking"))
                    .into_internal_error()
            })?;
            validate_logview_category(category)?;
        }
        Ok(DepOnlyIncompatibleInfo {
            custom_soft_errors: custom_soft_errors.as_unchecked().cast(),
        })
    }
}

pub type DepOnlyIncompatibleCustomSoftErrors =
    SmallMap<ArcStr, Box<[ParsedPattern<TargetPatternExtra>]>>;

impl FrozenDepOnlyIncompatibleInfo {
    pub fn custom_soft_errors(
        &self,
        root_cell: CellName,
        cell_resolver: &CellResolver,
        cell_alias_resolver: &CellAliasResolver,
    ) -> buck2_error::Result<DepOnlyIncompatibleCustomSoftErrors> {
        let custom_soft_errors: SmallMap<_, Box<[_]>> =
            DictRef::from_value(self.custom_soft_errors.get().to_value())
                .expect("Internal error: expected to be a dict")
                .iter()
                .map(|(k, v)| {
                    let k = ArcStr::from(k.to_value().to_str());
                    let mut target_patterns = Vec::new();
                    for target_pattern in ListRef::from_value(v)
                        .expect("Internal error: expected to be a list")
                        .iter()
                    {
                        let target_pattern = target_pattern.to_value().to_str();
                        target_patterns.push(ParsedPattern::parse_precise(
                            &target_pattern,
                            root_cell.dupe(),
                            &cell_resolver,
                            &cell_alias_resolver,
                        )?);
                    }
                    let v: Box<[_]> = target_patterns.into();
                    Ok((k, v.into()))
                })
                .collect::<buck2_error::Result<_>>()?;

        Ok(custom_soft_errors)
    }
}
