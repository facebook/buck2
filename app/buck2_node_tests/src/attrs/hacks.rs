/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::cells::name::CellName;
use buck2_core::cells::paths::CellRelativePath;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePath;
use buck2_core::package::PackageLabel;
use buck2_interpreter_for_build::attrs::coerce::attr_type::AttrTypeExt;
use buck2_interpreter_for_build::attrs::coerce::ctx::BuildAttrCoercionContext;
use buck2_interpreter_for_build::interpreter::testing::cells;
use buck2_node::attrs::attr_type::AttrType;
use buck2_node::attrs::configurable::AttrIsConfigurable;
use buck2_node::attrs::hacks;
use dupe::Dupe;
use starlark::values::Heap;

#[test]
fn stringifies_correctly() -> anyhow::Result<()> {
    let heap = Heap::new();
    let coercer_ctx = BuildAttrCoercionContext::new_no_package(
        cells(None)?.1,
        cells(None)?.0.resolve_self(),
        cells(None)?.0,
    );
    let coercer = AttrType::string();
    let coerced = coercer
        .coerce(
            AttrIsConfigurable::Yes,
            &coercer_ctx,
            heap.alloc("Hello, world!"),
        )
        .unwrap();

    let package = PackageLabel::new(
        CellName::testing_new("root"),
        CellRelativePath::new(ForwardRelativePath::new("foo/bar").unwrap()),
    );

    assert_eq!(
        "Hello, world!".to_owned(),
        hacks::value_to_string(&coerced, package.dupe())?
    );

    let list = AttrType::list(coercer).coerce(
        AttrIsConfigurable::Yes,
        &coercer_ctx,
        heap.alloc(vec!["Hello, world!"]),
    )?;
    assert!(hacks::value_to_string(&list, package.dupe()).is_err());
    Ok(())
}
