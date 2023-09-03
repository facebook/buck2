/*
 * Copyright 2019 The Starlark in Rust Authors.
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

use lsp_types::NumberOrString;
use lsp_types::Range;
use starlark::analysis::EvalMessage;
use starlark::analysis::EvalSeverity;

pub fn eval_message_to_lsp_diagnostic(eval_message: EvalMessage) -> lsp_types::Diagnostic {
    let range = match eval_message.span {
        Some(s) => s.into(),
        _ => Range::default(),
    };
    lsp_types::Diagnostic::new(
        range,
        Some(eval_severity_to_lsp_diagnostic_severity(
            eval_message.severity,
        )),
        Some(NumberOrString::String(eval_message.name)),
        None,
        eval_message.description,
        None,
        None,
    )
}

fn eval_severity_to_lsp_diagnostic_severity(
    eval_severity: EvalSeverity,
) -> lsp_types::DiagnosticSeverity {
    match eval_severity {
        EvalSeverity::Error => lsp_types::DiagnosticSeverity::ERROR,
        EvalSeverity::Warning => lsp_types::DiagnosticSeverity::WARNING,
        EvalSeverity::Advice => lsp_types::DiagnosticSeverity::HINT,
        EvalSeverity::Disabled => lsp_types::DiagnosticSeverity::INFORMATION,
    }
}
