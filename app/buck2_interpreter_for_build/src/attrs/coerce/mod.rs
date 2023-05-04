/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_node::attrs::attr_type::attr_literal::AttrLiteral;
use buck2_node::attrs::coercion_context::AttrCoercionContext;
use buck2_node::attrs::configurable::AttrIsConfigurable;
use starlark::values::Value;

mod arc_str_interner;
pub mod attr_type;
pub mod coerced_attr;
pub mod ctx;
pub mod error;
mod interner;
pub mod query_functions;
pub(crate) mod str_hash;
pub mod testing;

pub trait AttrTypeCoerce {
    fn coerce_item(
        &self,
        configurable: AttrIsConfigurable,
        _ctx: &dyn AttrCoercionContext,
        value: Value,
    ) -> anyhow::Result<AttrLiteral>;

    fn starlark_type(&self) -> String;
}
