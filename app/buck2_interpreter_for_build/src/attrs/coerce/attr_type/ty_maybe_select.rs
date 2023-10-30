/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use gazebo::prelude::SliceExt;
use starlark::typing::Ty;
use starlark::values::type_repr::StarlarkTypeRepr;
use starlark::values::Value;

use crate::interpreter::selector::StarlarkSelectorGen;

pub enum TyMaybeSelect {
    /// No inner selects.
    Basic(Ty),
    List(Box<TyMaybeSelect>),
    Dict(Box<TyMaybeSelect>, Box<TyMaybeSelect>),
    Tuple(Vec<TyMaybeSelect>),
    Union(Vec<TyMaybeSelect>),
}

impl TyMaybeSelect {
    pub(crate) fn to_ty(&self) -> Ty {
        match self {
            TyMaybeSelect::Basic(ty) => ty.clone(),
            TyMaybeSelect::List(x) => Ty::list(x.to_ty()),
            TyMaybeSelect::Dict(k, v) => Ty::dict(k.to_ty(), v.to_ty()),
            TyMaybeSelect::Tuple(x) => Ty::tuple(x.map(|x| x.to_ty())),
            TyMaybeSelect::Union(x) => Ty::unions(x.map(|x| x.to_ty())),
        }
    }

    pub(crate) fn to_ty_with_select(&self) -> Ty {
        fn with_select(ty: Ty) -> Ty {
            Ty::union2(ty, StarlarkSelectorGen::<Value>::starlark_type_repr())
        }

        match self {
            TyMaybeSelect::Basic(ty) => with_select(ty.clone()),
            TyMaybeSelect::List(x) => with_select(Ty::list(x.to_ty_with_select())),
            TyMaybeSelect::Dict(k, v) => {
                with_select(Ty::dict(k.to_ty_with_select(), v.to_ty_with_select()))
            }
            TyMaybeSelect::Tuple(x) => with_select(Ty::tuple(x.map(|x| x.to_ty_with_select()))),
            TyMaybeSelect::Union(x) => with_select(Ty::unions(x.map(|x| x.to_ty_with_select()))),
        }
    }
}
