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

use std::collections::BTreeMap;
use std::fmt;
use std::fmt::Display;
use std::fmt::Formatter;

use allocative::Allocative;

use crate::typing::custom::TyCustomImpl;
use crate::typing::Param;
use crate::typing::Ty;
use crate::typing::TypingAttr;
use crate::typing::TypingBinOp;

/// Struct type.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Allocative)]
pub struct TyStruct {
    /// The fields that are definitely present in the struct, with their types.
    pub(crate) fields: BTreeMap<String, Ty>,
    /// [`true`] if there might be additional fields not captured above,
    /// [`false`] if this struct has no extra members.
    pub(crate) extra: bool,
}

impl TyStruct {
    /// Any struct.
    pub fn any() -> TyStruct {
        TyStruct {
            fields: BTreeMap::new(),
            extra: true,
        }
    }
}

impl TyCustomImpl for TyStruct {
    fn as_name(&self) -> Option<&str> {
        Some("struct")
    }

    fn attribute(&self, attr: TypingAttr) -> Option<Result<Ty, ()>> {
        match attr {
            TypingAttr::Regular(attr) => match self.fields.get(attr) {
                Some(ty) => Some(Ok(ty.clone())),
                None if self.extra => Some(Ok(Ty::any())),
                _ => Some(Err(())),
            },
            TypingAttr::BinOp(TypingBinOp::Less) => Some(Ok(Ty::function(
                vec![Param::pos_only(Ty::custom(TyStruct::any()))],
                Ty::bool(),
            ))),
            _ => Some(Err(())),
        }
    }

    fn union2(a: Box<Self>, b: Box<Self>) -> Result<Box<Self>, (Box<Self>, Box<Self>)> {
        if a == b {
            // Fast path.
            Ok(a)
        } else if a.extra == b.extra && itertools::equal(a.fields.keys(), b.fields.keys()) {
            let mut fields = BTreeMap::new();
            for ((a_k, a_v), (b_k, b_v)) in a.fields.into_iter().zip(b.fields) {
                assert_eq!(a_k, b_k);
                fields.insert(a_k, Ty::union2(a_v, b_v));
            }
            Ok(Box::new(TyStruct {
                fields,
                extra: a.extra,
            }))
        } else {
            Err((a, b))
        }
    }
}

impl Display for TyStruct {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let TyStruct { fields, extra } = self;
        display_container::fmt_container(
            f,
            "struct(",
            ")",
            display_container::iter_display_chain(
                fields.iter().map(|(k, v)| format!("{} = {}", k, v)),
                extra.then_some(".."),
            ),
        )
    }
}
