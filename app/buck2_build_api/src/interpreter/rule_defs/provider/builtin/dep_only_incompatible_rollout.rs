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
use starlark::values::Coerce;
use starlark::values::Freeze;
use starlark::values::FreezeResult;
use starlark::values::Trace;
use starlark::values::ValueLifetimeless;
use starlark::values::ValueOf;
use starlark::values::ValueOfUncheckedGeneric;
use starlark::values::list::ListRef;
use starlark::values::list::ListType;

use crate as buck2_build_api;

/// A provider that is used to signal the targets that we want to soft error on
/// within the `DepOnlyIncompatibleInfo` provider.
#[internal_provider(dep_only_incompatible_custom_soft_error_creator)]
#[derive(Clone, Debug, Trace, Coerce, Freeze, ProvidesStaticType, Allocative)]
#[repr(C)]
pub struct DepOnlyIncompatibleRolloutGen<V: ValueLifetimeless> {
    /// A list of target patterns (minus exclusions) that we want to soft error on.
    pub target_patterns: ValueOfUncheckedGeneric<V, ListType<String>>,
    /// A list of target patterns that we want to exclude from the soft error.
    pub exclusions: ValueOfUncheckedGeneric<V, ListType<String>>,
}

impl<'v> DepOnlyIncompatibleRollout<'v> {
    pub fn target_patterns(&'v self) -> &'v ListRef<'v> {
        ListRef::from_value(self.target_patterns.get())
            .expect("internal error: type checked as list")
    }

    pub fn exclusions(&'v self) -> &'v ListRef<'v> {
        ListRef::from_value(self.exclusions.get()).expect("internal error: type checked as list")
    }
}

#[starlark_module]
fn dep_only_incompatible_custom_soft_error_creator(globals: &mut GlobalsBuilder) {
    #[starlark(as_type = FrozenDepOnlyIncompatibleRollout)]
    fn DepOnlyIncompatibleRollout<'v>(
        #[starlark(require = named)] target_patterns: ValueOf<'v, ListType<&'v str>>,
        #[starlark(require = named)] exclusions: ValueOf<'v, ListType<&'v str>>,
    ) -> starlark::Result<DepOnlyIncompatibleRollout<'v>> {
        Ok(DepOnlyIncompatibleRollout {
            target_patterns: target_patterns.as_unchecked().cast(),
            exclusions: exclusions.as_unchecked().cast(),
        })
    }
}
