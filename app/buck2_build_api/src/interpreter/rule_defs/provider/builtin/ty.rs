/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use starlark::environment::GlobalsBuilder;
use starlark::typing::Ty;
use starlark::values::function::NativeFunction;
use starlark::values::StarlarkValue;

pub(crate) fn builtin_provider_typechecker_ty(
    creator_func: for<'a> fn(&'a mut GlobalsBuilder),
) -> Ty {
    let globals = GlobalsBuilder::new().with(creator_func).build();
    let mut iter = globals.iter();
    let Some(first) = iter.next() else {
        panic!("empty globals");
    };
    if iter.next().is_some() {
        panic!("more then one global in creator func globals");
    }
    if first.1.to_value().get_type() != NativeFunction::TYPE {
        panic!("creator func is not a function");
    }
    Ty::of_value(first.1.to_value())
}
