/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

pub mod calculation;
pub mod lookup;

use buck2_node::attrs::coerced_attr::CoercedAttr;

/// Utility functions to introspect coerced values. This will go away once we have more of the value
/// coercion tooling done. For now, it handles things like stringification for the `targets` command,
/// converting to JSON, etc.
pub mod hacks {
    use buck2_core::package::PackageLabel;
    use buck2_node::attrs::fmt_context::AttrFmtContext;

    use super::*;

    pub fn value_to_json(
        value: &CoercedAttr,
        pkg: PackageLabel,
    ) -> anyhow::Result<serde_json::Value> {
        value.to_json(&AttrFmtContext { package: Some(pkg) })
    }

    pub fn value_to_string(value: &CoercedAttr, pkg: PackageLabel) -> anyhow::Result<String> {
        match value_to_json(value, pkg)?.as_str() {
            Some(s) => Ok(s.to_owned()),
            None => Err(anyhow::Error::msg("Expected a string, did not get one")),
        }
    }
}

#[cfg(test)]
mod tests {

    use buck2_core::bzl::ImportPath;
    use buck2_core::cells::name::CellName;
    use buck2_core::cells::paths::CellRelativePath;
    use buck2_core::fs::paths::forward_rel_path::ForwardRelativePath;
    use buck2_core::package::PackageLabel;
    use buck2_interpreter_for_build::attrs::coerce::attr_type::AttrTypeExt;
    use buck2_interpreter_for_build::attrs::coerce::ctx::BuildAttrCoercionContext;
    use buck2_interpreter_for_build::interpreter::testing::cells;
    use buck2_node::attrs::attr_type::AttrType;
    use buck2_node::attrs::configurable::AttrIsConfigurable;
    use buck2_node::rule_type::StarlarkRuleType;
    use dupe::Dupe;
    use starlark::values::Heap;

    use super::*;

    #[test]
    fn function_id_has_useful_string() {
        let import = ImportPath::testing_new("root//some/subdir:foo.bzl");
        let name = "foo_binary".to_owned();

        assert_eq!(
            "root//some/subdir/foo.bzl:foo_binary",
            &StarlarkRuleType {
                import_path: import,
                name,
            }
            .to_string()
        );
    }

    #[test]
    fn stringifies_correctly() -> anyhow::Result<()> {
        let heap = Heap::new();
        let coercer_ctx =
            BuildAttrCoercionContext::new_no_package(cells(None)?.1, cells(None)?.0.resolve_self());
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
}
