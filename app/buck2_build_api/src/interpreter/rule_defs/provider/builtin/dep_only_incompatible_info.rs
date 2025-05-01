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
use buck2_core::cells::CellAliasResolver;
use buck2_core::cells::CellResolver;
use buck2_core::cells::name::CellName;
use buck2_core::error::validate_logview_category;
use buck2_core::pattern::pattern::ParsedPattern;
use buck2_core::pattern::pattern_type::TargetPatternExtra;
use buck2_core::target::label::label::TargetLabel;
use buck2_util::arc_str::ArcStr;
use dupe::Dupe;
use starlark::any::ProvidesStaticType;
use starlark::collections::SmallMap;
use starlark::environment::GlobalsBuilder;
use starlark::eval::Evaluator;
use starlark::values::Coerce;
use starlark::values::Freeze;
use starlark::values::FreezeResult;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::values::ValueLifetimeless;
use starlark::values::ValueLike;
use starlark::values::ValueOf;
use starlark::values::ValueOfUnchecked;
use starlark::values::ValueOfUncheckedGeneric;
use starlark::values::dict::DictRef;
use starlark::values::dict::DictType;
use starlark::values::dict::UnpackDictEntries;

use crate as buck2_build_api;
use crate::interpreter::rule_defs::provider::builtin::dep_only_incompatible_rollout::DepOnlyIncompatibleRollout;
use crate::interpreter::rule_defs::provider::builtin::dep_only_incompatible_rollout::FrozenDepOnlyIncompatibleRollout;

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
///          "dep_only_incompatible_foo": DepOnlyIncompatibleRollout(
///             target_patterns = ["root//foo/..."],
///             exclusions = ["root//foo/excluded/..."],
///          ),
///          "dep_only_incompatible_bar": DepOnlyIncompatibleRollout(
///             target_patterns = ["root//bar/..."],
///             exclusions = [],
///          ),
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
    pub custom_soft_errors:
        ValueOfUncheckedGeneric<V, DictType<String, FrozenDepOnlyIncompatibleRollout>>,
}

#[starlark_module]
fn dep_only_incompatible_info_creator(globals: &mut GlobalsBuilder) {
    #[starlark(as_type = FrozenDepOnlyIncompatibleInfo)]
    fn DepOnlyIncompatibleInfo<'v>(
        #[starlark(require = named)] custom_soft_errors: UnpackDictEntries<
            ValueOf<'v, &'v str>,
            ValueOf<'v, &'v DepOnlyIncompatibleRollout<'v>>,
        >,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> starlark::Result<DepOnlyIncompatibleInfo<'v>> {
        let mut result = SmallMap::with_capacity(custom_soft_errors.entries.len());
        for (category, value) in custom_soft_errors.entries {
            let category_str = category.to_value().unpack_str().ok_or_else(|| {
                starlark::Error::from(anyhow::anyhow!("Expected string via type checking"))
                    .into_internal_error()
            })?;
            validate_logview_category(category_str)?;
            result.insert(category_str, value.value);
        }
        Ok(DepOnlyIncompatibleInfo {
            custom_soft_errors: ValueOfUnchecked::new(eval.heap().alloc(result)),
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Allocative)]
pub struct DepOnlyIncompatibleRolloutPatterns {
    target_patterns: Box<[ParsedPattern<TargetPatternExtra>]>,
    exclusions: Box<[ParsedPattern<TargetPatternExtra>]>,
}

impl DepOnlyIncompatibleRolloutPatterns {
    pub fn matches(&self, target: &TargetLabel) -> bool {
        for exclusion in &self.exclusions {
            if exclusion.matches(target) {
                return false;
            }
        }
        for target_pattern in &self.target_patterns {
            if target_pattern.matches(target) {
                return true;
            }
        }
        false
    }
}

pub type DepOnlyIncompatibleCustomSoftErrors = SmallMap<ArcStr, DepOnlyIncompatibleRolloutPatterns>;

impl FrozenDepOnlyIncompatibleInfo {
    pub fn custom_soft_errors(
        &self,
        root_cell: CellName,
        cell_resolver: &CellResolver,
        cell_alias_resolver: &CellAliasResolver,
    ) -> buck2_error::Result<DepOnlyIncompatibleCustomSoftErrors> {
        let value_to_target_pattern =
            |target_pattern: Value<'_>| -> buck2_error::Result<ParsedPattern<TargetPatternExtra>> {
                ParsedPattern::parse_precise(
                    &target_pattern.to_value().to_str(),
                    root_cell.dupe(),
                    cell_resolver,
                    cell_alias_resolver,
                )
            };

        let custom_soft_errors: SmallMap<_, _> =
            DictRef::from_value(self.custom_soft_errors.get().to_value())
                .expect("Internal error: expected to be a dict")
                .iter()
                .map(|(k, v)| {
                    let k = ArcStr::from(k.unpack_str().expect("Type checked to be string"));
                    let v = DepOnlyIncompatibleRollout::from_value(v).expect(
                        "Internal error: type checked to be a DepOnlyIncompatibleRollout provider",
                    );
                    let target_patterns = v
                        .target_patterns()
                        .iter()
                        .map(value_to_target_pattern)
                        .collect::<buck2_error::Result<Vec<_>>>()?
                        .into_boxed_slice();
                    let exclusions = v
                        .exclusions()
                        .iter()
                        .map(value_to_target_pattern)
                        .collect::<buck2_error::Result<Vec<_>>>()?
                        .into_boxed_slice();
                    let v = DepOnlyIncompatibleRolloutPatterns {
                        target_patterns,
                        exclusions,
                    };
                    Ok((k, v))
                })
                .collect::<buck2_error::Result<_>>()?;

        Ok(custom_soft_errors)
    }
}
