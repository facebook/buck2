/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hash;
use std::hash::Hasher;

use allocative::Allocative;
use cmp_any::PartialEqAny;

// Duplicate of starlark_syntax::codemap::ResolvedFileLine
pub struct StarlarkTargetCallStackRoot {
    /// File name.
    pub file: String,
    /// Line number is 0-based
    pub line: usize,
}

/// Untyped version of `starlark::eval::CallStack`.
pub trait StarlarkCallStackImpl: Display + Debug + Send + Sync + 'static {
    fn eq_token(&self) -> PartialEqAny;
    fn hash(&self, hashed: &mut dyn Hasher);
    fn root_location(&self) -> Option<StarlarkTargetCallStackRoot>;
}

/// `buck2_node` crate does not depend on `starlark`, but need to store Starlark call stack.
#[derive(Debug, Allocative)]
pub struct StarlarkCallStack {
    /// Actually `starlark::eval::CallStack`.
    // We don't care about much call stack size because it is used only when debugging.
    #[allocative(skip)]
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
            call_stack: Box::new(call_stack),
        }
    }

    pub fn root_location(&self) -> Option<StarlarkTargetCallStackRoot> {
        self.call_stack.root_location()
    }
}
