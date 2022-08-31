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
    use super::*;

    pub fn value_to_json(value: &CoercedAttr) -> anyhow::Result<serde_json::Value> {
        value.to_json()
    }

    pub fn value_to_string(value: &CoercedAttr) -> anyhow::Result<String> {
        match value_to_json(value)?.as_str() {
            Some(s) => Ok(s.to_owned()),
            None => Err(anyhow::Error::msg("Expected a string, did not get one")),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use buck2_core::bzl::ImportPath;
    use buck2_interpreter_for_build::attrs::coerce::attr_type::AttrTypeExt;
    use buck2_interpreter_for_build::attrs::coerce::ctx::BuildAttrCoercionContext;
    use buck2_node::attrs::attr_type::AttrType;
    use buck2_node::attrs::configurable::AttrIsConfigurable;
    use buck2_node::rule_type::StarlarkRuleType;
    use starlark::values::Heap;

    use super::*;
    use crate::interpreter::testing::cells;
    use crate::query::analysis::environment::ConfiguredGraphQueryEnvironment;

    #[test]
    fn function_id_has_useful_string() {
        let import = ImportPath::unchecked_new("root", "some/subdir", "foo.bzl");
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
        let coercer_ctx = BuildAttrCoercionContext::new_no_package(
            cells(None)?.0,
            Arc::new(ConfiguredGraphQueryEnvironment::functions()),
        );
        let coercer = AttrType::string();
        let coerced = coercer
            .coerce(
                AttrIsConfigurable::Yes,
                &coercer_ctx,
                heap.alloc("Hello, world!"),
            )
            .unwrap();

        assert_eq!(
            "Hello, world!".to_owned(),
            hacks::value_to_string(&coerced)?
        );

        let list = AttrType::list(coercer).coerce(
            AttrIsConfigurable::Yes,
            &coercer_ctx,
            heap.alloc(vec!["Hello, world!"]),
        )?;
        assert!(hacks::value_to_string(&list).is_err());
        Ok(())
    }
}
