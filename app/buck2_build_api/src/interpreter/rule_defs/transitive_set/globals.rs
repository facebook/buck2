/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use starlark::environment::GlobalsBuilder;
use starlark::values::starlark_value_as_type::StarlarkValueAsType;
use starlark::values::FrozenValue;

use crate::interpreter::rule_defs::transitive_set::transitive_set::TransitiveSetGen;
use crate::interpreter::rule_defs::transitive_set::transitive_set_args_projection::TransitiveSetArgsProjectionGen;
use crate::interpreter::rule_defs::transitive_set::transitive_set_json_projection::TransitiveSetJsonProjectionGen;
use crate::interpreter::rule_defs::transitive_set::traversal::TransitiveSetProjectionTraversalGen;
use crate::interpreter::rule_defs::transitive_set::traversal::TransitiveSetTraversalGen;
use crate::interpreter::rule_defs::transitive_set::FrozenTransitiveSetDefinition;

#[starlark_module]
pub(crate) fn register_transitive_set_types(globals: &mut GlobalsBuilder) {
    const TransitiveSet: StarlarkValueAsType<TransitiveSetGen<FrozenValue>> =
        StarlarkValueAsType::new();
    const TransitiveSetArgsProjection: StarlarkValueAsType<
        TransitiveSetArgsProjectionGen<FrozenValue>,
    > = StarlarkValueAsType::new();
    const TransitiveSetDefinition: StarlarkValueAsType<FrozenTransitiveSetDefinition> =
        StarlarkValueAsType::new();
    const TransitiveSetJsonProjection: StarlarkValueAsType<
        TransitiveSetJsonProjectionGen<FrozenValue>,
    > = StarlarkValueAsType::new();
    const TransitiveSetIterator: StarlarkValueAsType<TransitiveSetTraversalGen<FrozenValue>> =
        StarlarkValueAsType::new();
    const TransitiveSetArgsProjectionIterator: StarlarkValueAsType<
        TransitiveSetProjectionTraversalGen<FrozenValue>,
    > = StarlarkValueAsType::new();
}
