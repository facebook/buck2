/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use starlark::docs::get_registered_starlark_docs;
use starlark::environment::Globals;
use starlark::environment::LibraryExtension;
use starlark::typing::*;

pub(crate) fn oracle_buck(globals: &Globals) -> Arc<dyn TypingOracle + Send + Sync> {
    let registered_docs = get_registered_starlark_docs();
    let mut docs = OracleDocs::new();
    docs.add_module(&globals.documentation());
    docs.add_docs(&registered_docs);

    let mut docs2 = OracleDocs::new();
    docs2.add_docs(&registered_docs);

    Arc::new(OracleSeq(vec![
        // TODO(nga): this should only accept globals from enabled extensions, not all extensions.
        Box::new(OracleStandard::new(LibraryExtension::all()))
            as Box<dyn TypingOracle + Send + Sync>,
        Box::new(CustomBuck),
        Box::new(docs),
        Box::new(AddErrors(docs2)),
    ]))
}

struct CustomBuck;

impl TypingOracle for CustomBuck {
    fn attribute(&self, ty: &Ty, attr: TypingAttr) -> Option<Result<Ty, ()>> {
        match ty.as_name()? {
            "dependency" => match attr {
                // We can index by providers, which either appear as functions (for builtin providers)
                // or as Any (for user providers)
                TypingAttr::Index => {
                    Some(Ok(Ty::function(vec![Param::pos_only(Ty::Any)], Ty::Any)))
                }
                TypingAttr::BinOp(TypingBinOp::In) => {
                    Some(Ok(Ty::function(vec![Param::pos_only(Ty::Any)], Ty::bool())))
                }
                _ => None,
            },
            _ => None,
        }
    }

    fn as_function(&self, ty: &TyName) -> Option<Result<TyFunction, ()>> {
        match ty.as_str() {
            "provider_callable" => Some(Ok(TyFunction {
                name: "provider_callable".to_owned(),
                type_attr: "".to_owned(),
                params: vec![Param::kwargs(Ty::Any)],
                // TODO(nga): this should be more precise.
                result: Box::new(Ty::Any),
            })),
            "rule" => Some(Ok(TyFunction {
                name: "rule".to_owned(),
                type_attr: "".to_owned(),
                params: vec![Param::kwargs(Ty::Any)],
                result: Box::new(Ty::None),
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
    fn attribute(&self, ty: &Ty, _attr: TypingAttr) -> Option<Result<Ty, ()>> {
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
