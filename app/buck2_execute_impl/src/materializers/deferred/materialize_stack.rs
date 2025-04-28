/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Display;
use std::fmt::Formatter;

use buck2_core::fs::project_rel_path::ProjectRelativePath;
use dupe::Dupe;
use itertools::Itertools;

#[derive(Copy, Clone, Dupe)]
pub(crate) enum MaterializeStack<'a> {
    Empty,
    Child(&'a MaterializeStack<'a>, &'a ProjectRelativePath),
}

impl<'a> Display for MaterializeStack<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let MaterializeStack::Empty = self {
            return write!(f, "(empty)");
        }

        // Avoid recursion because we are fighting with stack overflow here,
        // and we do not want another stack overflow when producing error message.
        let mut stack = Vec::new();
        let mut current = *self;
        while let MaterializeStack::Child(parent, path) = current {
            stack.push(path);
            current = *parent;
        }
        write!(f, "{}", stack.iter().rev().join(" -> "))
    }
}

#[test]
fn test_materialize_stack_display() {
    let s = MaterializeStack::Empty;
    assert_eq!("(empty)", s.to_string());
    let s = MaterializeStack::Child(&s, ProjectRelativePath::new("foo").unwrap());
    assert_eq!("foo", s.to_string());
    let s = MaterializeStack::Child(&s, ProjectRelativePath::new("bar/baz").unwrap());
    assert_eq!("foo -> bar/baz", s.to_string());
}
