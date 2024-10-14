/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_interpreter::downstream_crate_starlark_defs::REGISTER_BUCK2_ACTION_IMPL_GLOBALS;

use crate::dynamic::attrs_starlark::register_dynamic_attrs;
use crate::dynamic::dynamic_actions_globals::register_dynamic_actions;
use crate::dynamic::dynamic_value::register_dynamic_value;
use crate::dynamic::resolved_dynamic_value::register_resolved_dynamic_value;

pub(crate) fn init_register_buck2_action_impl_globals() {
    REGISTER_BUCK2_ACTION_IMPL_GLOBALS.init(|globals| {
        register_dynamic_actions(globals);
        register_dynamic_value(globals);
        register_dynamic_attrs(globals);
        register_resolved_dynamic_value(globals);
    });
}
