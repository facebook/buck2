/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::hash::Hash;
use std::hash::Hasher;

use buck2_node::call_stack::StarlarkCallStackImpl;
use buck2_node::call_stack::StarlarkTargetCallStackRoot;
use cmp_any::PartialEqAny;
use starlark::eval::CallStack;

// I can't implement a trait for a type that is not of this crate, so I wrap type here
#[derive(Debug, derive_more::Display, PartialEq)]
pub struct StarlarkCallStackWrapper(pub CallStack);

impl StarlarkCallStackImpl for StarlarkCallStackWrapper {
    fn eq_token(&self) -> PartialEqAny {
        PartialEqAny::new(self)
    }

    fn hash(&self, mut state: &mut dyn Hasher) {
        self.0.hash(&mut state);
    }

    fn root_location(&self) -> Option<StarlarkTargetCallStackRoot> {
        self.0
            .frames
            .first()
            .and_then(|l| l.location.as_ref())
            .map(|l| l.resolve().begin_file_line())
            .map(|l| StarlarkTargetCallStackRoot {
                file: l.file.clone(),
                line: l.line,
            })
    }
}
