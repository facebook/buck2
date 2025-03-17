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
use starlark::any::ProvidesStaticType;
use starlark::environment::GlobalsBuilder;
use starlark::values::dict::DictType;
use starlark::values::list::ListType;
use starlark::values::Coerce;
use starlark::values::Freeze;
use starlark::values::FreezeResult;
use starlark::values::Trace;
use starlark::values::ValueLifetimeless;
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
        Ok(DepOnlyIncompatibleInfo {
            custom_soft_errors: custom_soft_errors.as_unchecked().cast(),
        })
    }
}
