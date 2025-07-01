/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use buck2_core::cells::name::CellName;
use buck2_core::cells::paths::CellRelativePath;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePath;
use buck2_core::package::PackageLabel;
use buck2_node::attrs::attr_type::list::ListLiteral;
use buck2_node::attrs::attr_type::string::StringLiteral;
use buck2_node::attrs::coerced_attr::CoercedAttr;
use buck2_node::attrs::hacks;
use buck2_util::arc_str::ArcSlice;
use buck2_util::arc_str::ArcStr;
use dupe::Dupe;

#[test]
fn stringifies_correctly() -> buck2_error::Result<()> {
    let coerced = CoercedAttr::String(StringLiteral(ArcStr::from("Hello, world!")));

    let package = PackageLabel::new(
        CellName::testing_new("root"),
        CellRelativePath::new(ForwardRelativePath::new("foo/bar").unwrap()),
    )?;

    assert_eq!(
        "Hello, world!".to_owned(),
        hacks::value_to_string(&coerced, package.dupe())?
    );

    let list = CoercedAttr::List(ListLiteral(ArcSlice::new([CoercedAttr::String(
        StringLiteral(ArcStr::from("Hello, world!")),
    )])));
    assert!(hacks::value_to_string(&list, package.dupe()).is_err());
    Ok(())
}
