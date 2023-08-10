/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use buck2_interpreter_for_build::interpreter::build_defs::starlark_library_extensions_for_buck2;
use starlark::docs::get_registered_starlark_docs;
use starlark::environment::Globals;
use starlark::typing::*;

pub(crate) fn oracle_buck(globals: &Globals) -> Arc<dyn TypingOracle + Send + Sync> {
    let registered_docs = get_registered_starlark_docs();
    let mut docs = OracleDocs::new();
    docs.add_module(&globals.documentation());
    docs.add_docs(&registered_docs);

    let mut docs2 = OracleDocs::new();
    docs2.add_docs(&registered_docs);

    Arc::new(OracleSeq(vec![
        Box::new(OracleStandard::new(starlark_library_extensions_for_buck2()))
            as Box<dyn TypingOracle + Send + Sync>,
        Box::new(CustomBuck),
        Box::new(docs),
        Box::new(AddErrors(docs2)),
    ]))
}

struct CustomBuck;

impl TypingOracle for CustomBuck {
    fn as_function(&self, ty: &TyName) -> Option<Result<TyFunction, ()>> {
        match ty.as_str() {
            "provider_callable" => Some(Ok(TyFunction {
                type_attr: None,
                params: vec![Param::kwargs(Ty::any())],
                // TODO(nga): this should be more precise.
                result: Box::new(Ty::any()),
            })),
            "rule" => Some(Ok(TyFunction {
                type_attr: None,
                params: vec![Param::kwargs(Ty::any())],
                result: Box::new(Ty::none()),
            })),
            _ => None,
        }
    }

    fn subtype(&self, require: &TyName, got: &TyName) -> bool {
        match require.as_str() {
            "provider" => got.as_str().ends_with("Info"),
            _ => false,
        }
    }
}

struct AddErrors(OracleDocs);

impl TypingOracle for AddErrors {
    fn attribute(&self, ty: &TyBasic, _attr: TypingAttr) -> Option<Result<Ty, ()>> {
        if self.0.known_object(ty.as_name()?) {
            Some(Err(()))
        } else {
            None
        }
    }
    fn as_function(&self, _ty: &TyName) -> Option<Result<TyFunction, ()>> {
        Some(Err(()))
    }
}
