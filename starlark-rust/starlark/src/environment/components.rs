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

use crate::docs::DocFunction;
use crate::docs::DocStringKind;
use crate::docs::DocType;
use crate::eval::ParametersSpec;
use crate::typing::Ty;
use crate::values::FrozenValue;

/// A wrapper for the parameters to `GlobalsBuilder::set_function` and `MethodBuilder::set_method`
#[allow(missing_docs)]
pub struct NativeCallableComponents {
    pub speculative_exec_safe: bool,
    pub rust_docstring: Option<&'static str>,
    pub signature: ParametersSpec<FrozenValue>,
    pub parameter_types: Vec<Ty>,
    pub return_type: Ty,
}

impl NativeCallableComponents {
    pub(crate) fn into_docs(self, as_type: Option<(Ty, DocType)>) -> DocFunction {
        // TODO(JakobDegen): Use the docs
        let as_type = as_type.map(|x| x.0);
        DocFunction::from_docstring(
            DocStringKind::Rust,
            self.signature
                .documentation(self.parameter_types.clone(), HashMap::new()),
            self.return_type.clone(),
            self.rust_docstring,
            as_type,
        )
    }
}
