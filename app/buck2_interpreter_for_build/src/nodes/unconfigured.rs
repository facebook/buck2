/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Display;
use std::hash::Hash;
use std::hash::Hasher;
use std::sync::Arc;

use buck2_core::target::label::label::TargetLabel;
use buck2_core::target::name::TargetNameRef;
use buck2_node::attrs::coerced_deps_collector::CoercedDeps;
use buck2_node::attrs::coerced_deps_collector::CoercedDepsCollector;
use buck2_node::attrs::inspect_options::AttrInspectOptions;
use buck2_node::attrs::internal::NAME_ATTRIBUTE_FIELD;
use buck2_node::attrs::values::AttrValues;
use buck2_node::call_stack::StarlarkCallStack;
use buck2_node::call_stack::StarlarkCallStackImpl;
use buck2_node::call_stack::StarlarkTargetCallStackRoot;
use buck2_node::nodes::unconfigured::TargetNode;
use buck2_node::package::Package;
use buck2_node::rule::Rule;
use dupe::Dupe;
use starlark::eval::CallStack;
use starlark::eval::ParametersParser;
use starlark::values::Value;

use crate::interpreter::module_internals::ModuleInternals;
use crate::nodes::attr_spec::AttributeSpecExt;

pub trait TargetNodeExt: Sized {
    fn from_params_ignore_attrs_for_profiling<'v>(
        rule: Arc<Rule>,
        package: Arc<Package>,
        internals: &ModuleInternals,
        param_parser: ParametersParser<'v, '_>,
    ) -> anyhow::Result<Self>;

    fn from_params<'v>(
        rule: Arc<Rule>,
        package: Arc<Package>,
        internals: &ModuleInternals,
        param_parser: ParametersParser<'v, '_>,
        arg_count: usize,
        ignore_attrs_for_profiling: bool,
        call_stack: Option<CallStack>,
    ) -> anyhow::Result<Self>;
}

impl TargetNodeExt for TargetNode {
    /// Extract only the name attribute from rule arguments, ignore the others.
    fn from_params_ignore_attrs_for_profiling<'v>(
        rule: Arc<Rule>,
        package: Arc<Package>,
        internals: &ModuleInternals,
        mut param_parser: ParametersParser<'v, '_>,
    ) -> anyhow::Result<Self> {
        for (attr_name, _attr_idx, _attr) in rule.attributes.attr_specs() {
            let value: Value = param_parser.next(attr_name)?;
            if attr_name == NAME_ATTRIBUTE_FIELD {
                let label = TargetLabel::new(
                    internals.buildfile_path().package().dupe(),
                    TargetNameRef::new(value.unpack_str().unwrap()).unwrap(),
                );
                return Ok(TargetNode::new(
                    rule.dupe(),
                    package,
                    label,
                    AttrValues::with_capacity(0),
                    CoercedDeps::default(),
                    None,
                ));
            }
        }
        unreachable!("`name` attribute not found");
    }

    /// The body of the callable returned by `rule()`. Records the target in this package's `TargetMap`
    #[allow(clippy::box_collection)] // Parameter `call_stack`, because this is the field type.
    fn from_params<'v>(
        rule: Arc<Rule>,
        package: Arc<Package>,
        internals: &ModuleInternals,
        param_parser: ParametersParser<'v, '_>,
        arg_count: usize,
        ignore_attrs_for_profiling: bool,
        call_stack: Option<CallStack>,
    ) -> anyhow::Result<Self> {
        if ignore_attrs_for_profiling {
            return Self::from_params_ignore_attrs_for_profiling(
                rule,
                package,
                internals,
                param_parser,
            );
        }

        let (target_name, attr_values) =
            rule.attributes
                .parse_params(param_parser, arg_count, internals)?;
        let package_name = internals.buildfile_path().package();

        let label = TargetLabel::new(package_name.dupe(), target_name);
        let mut deps_cache = CoercedDepsCollector::new();

        for a in rule.attributes.attrs(&attr_values, AttrInspectOptions::All) {
            a.traverse(label.pkg(), &mut deps_cache)?;
        }

        Ok(TargetNode::new(
            rule,
            package,
            label,
            attr_values,
            CoercedDeps::from(deps_cache),
            call_stack
                .map(StarlarkCallStackWrapper)
                .map(StarlarkCallStack::new),
        ))
    }
}

// I can't implement a trait for a type that is not of this crate, so I wrap type here
#[derive(Debug)]
pub struct StarlarkCallStackWrapper(pub CallStack);

use cmp_any::PartialEqAny;

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

impl Display for StarlarkCallStackWrapper {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.0, f)
    }
}

impl PartialEq for StarlarkCallStackWrapper {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}
