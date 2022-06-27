/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::any::Any;
use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hash;
use std::hash::Hasher;

use gazebo::cmp::PartialEqAny;

/// Untyped version of `starlark::eval::CallStack`.
pub trait StarlarkCallStackImpl: Display + Debug + Send + Sync + 'static {
    fn eq_token(&self) -> PartialEqAny;
    fn hash(&self, hashed: &mut dyn Hasher);
}

impl<S: Display + Debug + Hash + Eq + Send + Any + Sync + Sized + 'static> StarlarkCallStackImpl
    for S
{
    fn eq_token(&self) -> PartialEqAny {
        PartialEqAny::new(self)
    }

    fn hash(&self, mut state: &mut dyn Hasher) {
        self.hash(&mut state);
    }
}

/// `buck2_node` crate does not depend on `starlark`, but need to store Starlark call stack.
#[derive(Debug)]
pub struct StarlarkCallStack {
    /// Actually `starlark::eval::CallStack`.
    call_stack: Box<dyn StarlarkCallStackImpl>,
}

impl Display for StarlarkCallStack {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.call_stack, f)
    }
}

impl PartialEq for StarlarkCallStack {
    fn eq(&self, other: &Self) -> bool {
        self.call_stack.eq_token() == other.call_stack.eq_token()
    }
}

impl Eq for StarlarkCallStack {}

impl Hash for StarlarkCallStack {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.call_stack.hash(state);
    }
}

impl StarlarkCallStack {
    pub fn new(call_stack: impl StarlarkCallStackImpl) -> StarlarkCallStack {
        StarlarkCallStack {
            call_stack: box call_stack,
        }
    }
}
