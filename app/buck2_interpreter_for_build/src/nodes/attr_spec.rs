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
use buck2_core::target::label::TargetLabelRef;
use buck2_core::target::name::TargetName;
use buck2_node::attrs::attr::CoercedValue;
use buck2_node::attrs::attr_type::string::StringLiteral;
use buck2_node::attrs::coerced_attr::CoercedAttr;
use buck2_node::attrs::configurable::AttrIsConfigurable;
use buck2_node::attrs::inspect_options::AttrInspectOptions;
use buck2_node::attrs::internal::attr_is_configurable;
use buck2_node::attrs::internal::NAME_ATTRIBUTE_FIELD;
use buck2_node::attrs::internal::VISIBILITY_ATTRIBUTE_FIELD;
use buck2_node::attrs::internal::WITHIN_VIEW_ATTRIBUTE_FIELD;
use buck2_node::attrs::spec::AttributeSpec;
use buck2_node::attrs::values::AttrValues;
use buck2_util::arc_str::ArcStr;
use dupe::Dupe;
use starlark::docs::DocString;
use starlark::eval::ParametersParser;
use starlark::eval::ParametersSpec;
use starlark::typing::Param;
use starlark::typing::Ty;
use starlark::typing::TyFunction;
use starlark::values::Value;

use crate::attrs::AttributeCoerceExt;
use crate::interpreter::module_internals::ModuleInternals;
use crate::nodes::check_within_view::check_within_view;

#[derive(Debug, thiserror::Error)]
enum AttributeSpecError {
    #[error("`within_view` coerced incorrectly (internal error)")]
    WithinViewCoercedIncorrectly,
}

pub trait AttributeSpecExt {
    fn parse_params<'v>(
        &self,
        param_parser: ParametersParser<'v, '_>,
        arg_count: usize,
        internals: &ModuleInternals,
    ) -> anyhow::Result<(TargetName, AttrValues)>;

    /// Returns a starlark Parameters for the rule callable.
    fn signature(&self, rule_name: String) -> ParametersSpec<Value<'_>>;

    fn ty_function(&self) -> TyFunction;

    fn starlark_types(&self) -> Vec<Ty>;

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
                    CoercedAttr::String(StringLiteral(ArcStr::from(name))),
                );

                TargetName::new(name)?
            }
            _ => panic!("First attribute is `name`, it is known"),
        };

        let target_label = TargetLabelRef::new(internals.buildfile_path().package(), name.as_ref());

        for (attr_name, attr_idx, attribute) in indices {
            let configurable = attr_is_configurable(attr_name);

            let user_value: Option<Value> = match attribute.default() {
                Some(_) => param_parser.next_opt(attr_name)?,
                None => Some(param_parser.next(attr_name)?),
            };

            let attr_is_visibility = attr_name == VISIBILITY_ATTRIBUTE_FIELD;
            let attr_is_within_view = attr_name == WITHIN_VIEW_ATTRIBUTE_FIELD;
            if let Some(v) = user_value {
                let mut coerced = attribute
                    .coerce(
                        attr_name,
                        configurable,
                        internals.attr_coercion_context(),
                        v,
                    )
                    .with_context(|| {
                        format!(
                            "Error coercing attribute `{}` of `{}`",
                            attr_name, target_label,
                        )
                    })?;

                if attr_is_visibility {
                    if coerced == CoercedValue::Default {
                        coerced = CoercedValue::Custom(CoercedAttr::Visibility(
                            internals.super_package.visibility().dupe(),
                        ));
                    }
                } else if attr_is_within_view {
                    if coerced == CoercedValue::Default {
                        coerced = CoercedValue::Custom(CoercedAttr::WithinView(
                            internals.super_package.within_view().dupe(),
                        ));
                    }
                }

                match coerced {
                    CoercedValue::Custom(v) => {
                        attr_values.push_sorted(attr_idx, v);
                    }
                    CoercedValue::Default => {}
                }
            } else if attr_is_visibility {
                attr_values.push_sorted(
                    attr_idx,
                    CoercedAttr::Visibility(internals.super_package.visibility().dupe()),
                );
            } else if attr_is_within_view {
                attr_values.push_sorted(
                    attr_idx,
                    CoercedAttr::WithinView(internals.super_package.within_view().dupe()),
                );
            }
        }

        attr_values.shrink_to_fit();

        // For now `within_view` is always set, but let's make code more robust.
        if let Some(within_view) = attr_values.get(AttributeSpec::within_view_attr_id()) {
            let within_view = match within_view {
                CoercedAttr::WithinView(within_view) => within_view,
                _ => return Err(AttributeSpecError::WithinViewCoercedIncorrectly.into()),
            };
            for a in self.attrs(&attr_values, AttrInspectOptions::DefinedOnly) {
                check_within_view(
                    a.value,
                    internals.buildfile_path().package(),
                    a.attr.coercer(),
                    within_view,
                )
                .with_context(|| {
                    format!(
                        "checking `within_view` for attribute `{}` of `{}`",
                        a.name, target_label,
                    )
                })?;
            }
        }

        Ok((name, attr_values))
    }

    /// Returns a starlark Parameters for the rule callable.
    fn signature(&self, rule_name: String) -> ParametersSpec<Value<'_>> {
        let mut signature = ParametersSpec::with_capacity(rule_name, self.len());
        signature.no_more_positional_args();
        for (name, _idx, attribute) in self.attr_specs() {
            match attribute.default() {
                Some(_) => signature.optional(name),
                None => signature.required(name),
            };
        }
        signature.finish()
    }

    fn ty_function(&self) -> TyFunction {
        let mut params = Vec::with_capacity(self.attr_specs().len());
        for (name, _idx, attribute) in self.attr_specs() {
            let ty = match attr_is_configurable(name) {
                AttrIsConfigurable::Yes => attribute.starlark_type().to_ty_with_select(),
                AttrIsConfigurable::No => attribute.starlark_type().to_ty(),
            };
            let param = Param::name_only(name, ty);
            let param = match attribute.default() {
                Some(_) => param.optional(),
                None => param,
            };
            params.push(param);
        }
        TyFunction::new(params, Ty::none())
    }

    fn starlark_types(&self) -> Vec<Ty> {
        self.attr_specs()
            .map(|(_, _, a)| a.starlark_type().to_ty())
            .collect()
    }

    fn docstrings(&self) -> HashMap<String, Option<DocString>> {
        self.attr_specs()
            .map(|(name, _idx, attr)| (name.to_owned(), attr.docstring()))
            .collect()
    }
}
