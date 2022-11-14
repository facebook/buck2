/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::ops::Deref;

use allocative::Allocative;
use buck2_node::attrs::attr::Attribute;
use gazebo::any::ProvidesStaticType;
use starlark::starlark_simple_value;
use starlark::starlark_type;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;

#[derive(
    derive_more::Display,
    Debug,
    ProvidesStaticType,
    NoSerialize,
    Allocative
)]
#[display(fmt = "{}", .0)]
pub struct AttributeAsStarlarkValue(pub Attribute);

starlark_simple_value!(AttributeAsStarlarkValue);

impl<'v> StarlarkValue<'v> for AttributeAsStarlarkValue {
    starlark_type!("attribute");
}

impl Deref for AttributeAsStarlarkValue {
    type Target = Attribute;

    fn deref(&self) -> &Attribute {
        &self.0
    }
}
