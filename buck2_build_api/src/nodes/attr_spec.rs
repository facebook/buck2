/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;

use anyhow::Context;
use buck2_core::target::TargetName;
use buck2_node::attrs::attr::CoercedValue;
use buck2_node::attrs::attr_type::attr_literal::AttrLiteral;
use buck2_node::attrs::coerced_attr::CoercedAttr;
use buck2_node::attrs::internal::attr_is_configurable;
use buck2_node::attrs::internal::NAME_ATTRIBUTE_FIELD;
use buck2_node::attrs::spec::AttributeSpec;
use buck2_node::attrs::values::AttrValues;
use gazebo::prelude::*;
use starlark::eval::ParametersParser;
use starlark::eval::ParametersSpec;
use starlark::values::docs::DocString;
use starlark::values::Value;

use crate::interpreter::module_internals::ModuleInternals;
use crate::interpreter::rule_defs::attr::AttributeExt;

pub(crate) trait AttributeSpecExt {
    fn parse_params<'v>(
        &self,
        param_parser: ParametersParser<'v, '_>,
        arg_count: usize,
        internals: &ModuleInternals,
    ) -> anyhow::Result<(TargetName, AttrValues)>;

    /// Returns a starlark Parameters for the rule callable.
    fn signature(&self, rule_name: String) -> ParametersSpec<Value<'_>>;

    fn starlark_types(&self) -> Vec<String>;

    fn docstrings(&self) -> HashMap<String, Option<DocString>>;
}

impl AttributeSpecExt for AttributeSpec {
    /// Parses params extracting the TargetName and the attribute values to store in the TargetNode.
    fn parse_params<'v>(
        &self,
        mut param_parser: ParametersParser<'v, '_>,
        arg_count: usize,
        internals: &ModuleInternals,
    ) -> anyhow::Result<(TargetName, AttrValues)> {
        let mut attr_values = AttrValues::with_capacity(arg_count);

        let mut indices = self.attr_specs();
        let name = match indices.next() {
            Some((name_name, attr_idx, _attr))
                if name_name == NAME_ATTRIBUTE_FIELD && attr_idx.index_in_attribute_spec == 0 =>
            {
                let name: &str = param_parser.next(NAME_ATTRIBUTE_FIELD)?;

                attr_values.push_sorted(
                    attr_idx,
                    CoercedAttr::Literal(AttrLiteral::String(name.to_owned())),
                );

                TargetName::new(name)?
            }
            _ => panic!("First attribute is `name`, it is known"),
        };

        for (attr_name, attr_idx, attribute) in indices {
            let configurable = attr_is_configurable(attr_name);

            let user_value: Option<Value> = match attribute.default {
                Some(_) => param_parser.next_opt(attr_name)?,
                None => Some(param_parser.next(attr_name)?),
            };

            if let Some(v) = user_value {
                let coerced = attribute
                    .coerce(
                        attr_name,
                        configurable,
                        internals.attr_coercion_context(),
                        Some(v),
                    )
                    .with_context(|| {
                        format!(
                            "when coercing attributes of {}:{}",
                            internals.buildfile_path().package(),
                            name,
                        )
                    })?;
                match coerced {
                    CoercedValue::Custom(v) => {
                        attr_values.push_sorted(attr_idx, v);
                    }
                    CoercedValue::Default => {}
                }
            }
        }

        attr_values.shrink_to_fit();
        Ok((name, attr_values))
    }

    /// Returns a starlark Parameters for the rule callable.
    fn signature(&self, rule_name: String) -> ParametersSpec<Value<'_>> {
        let mut signature = ParametersSpec::with_capacity(rule_name, self.indices.len());
        signature.no_more_positional_args();
        for (name, _idx, attribute) in self.attr_specs() {
            match attribute.default {
                Some(_) => signature.optional(name),
                None => signature.required(name),
            };
        }
        signature.finish()
    }

    fn starlark_types(&self) -> Vec<String> {
        self.attributes.map(|a| a.starlark_type())
    }

    fn docstrings(&self) -> HashMap<String, Option<DocString>> {
        self.attr_specs()
            .map(|(name, _idx, attr)| (name.to_owned(), attr.docstring()))
            .collect()
    }
}
