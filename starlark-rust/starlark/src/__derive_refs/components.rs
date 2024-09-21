/*
 * Copyright 2018 The Starlark in Rust Authors.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

use std::collections::HashMap;

use dupe::IterDupedExt;

use crate::__derive_refs::param_spec::NativeCallableParam;
use crate::__derive_refs::param_spec::NativeCallableParamDefaultValue;
use crate::__derive_refs::param_spec::NativeCallableParamSpec;
use crate::docs::DocFunction;
use crate::docs::DocStringKind;
use crate::docs::DocType;
use crate::eval::ParametersSpec;
use crate::eval::ParametersSpecBuilder;
use crate::typing::Ty;
use crate::values::FrozenValue;

/// A wrapper for the parameters to `GlobalsBuilder::set_function` and `MethodBuilder::set_method`
pub struct NativeCallableComponents {
    pub speculative_exec_safe: bool,
    pub rust_docstring: Option<&'static str>,
    pub param_spec: NativeCallableParamSpec,
    pub return_type: Ty,
}

impl NativeCallableComponents {
    fn param_spec_for_docs(&self) -> ParametersSpec<FrozenValue> {
        let NativeCallableParamSpec {
            pos_only,
            pos_or_named,
            args,
            named_only,
            kwargs,
        } = &self.param_spec;
        let mut parameters_spec = ParametersSpec::new("not used".to_owned());

        fn add_param(spec: &mut ParametersSpecBuilder<FrozenValue>, param: &NativeCallableParam) {
            match param.required {
                None => spec.required(param.name),
                Some(NativeCallableParamDefaultValue::Value(default)) => {
                    spec.defaulted(param.name, default)
                }
                Some(NativeCallableParamDefaultValue::Optional) => spec.optional(param.name),
            }
        }

        for pos_only in pos_only {
            add_param(&mut parameters_spec, pos_only);
        }
        parameters_spec.no_more_positional_only_args();
        for pos_or_named in pos_or_named {
            add_param(&mut parameters_spec, pos_or_named);
        }
        if let Some(_args) = args {
            parameters_spec.args();
        } else {
            parameters_spec.no_more_positional_args();
        }
        for named_only in named_only {
            add_param(&mut parameters_spec, named_only);
        }
        if let Some(_kwargs) = kwargs {
            parameters_spec.kwargs();
        }
        parameters_spec.finish()
    }

    pub(crate) fn into_docs(self, as_type: Option<(Ty, DocType)>) -> DocFunction {
        // TODO(JakobDegen): Use the docs
        let as_type = as_type.map(|x| x.0);
        DocFunction::from_docstring(
            DocStringKind::Rust,
            self.param_spec_for_docs().documentation(
                self.param_spec.param_types().duped().collect(),
                HashMap::new(),
            ),
            self.return_type.clone(),
            self.rust_docstring,
            as_type,
        )
    }
}
