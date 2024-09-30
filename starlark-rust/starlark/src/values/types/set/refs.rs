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

use std::cell::Ref;
use std::cell::RefCell;
use std::cell::RefMut;
use std::ops::Deref;
use std::ops::DerefMut;

use dupe::Dupe;
use either::Either;

use super::value::FrozenSetData;
use super::value::SetData;
use crate::coerce::coerce;
use crate::values::layout::value::ValueLike;
use crate::values::set::value::SetGen;
use crate::values::Value;
use crate::values::ValueError;

pub struct SetRef<'v> {
    pub(crate) aref: Either<Ref<'v, SetData<'v>>, &'v SetData<'v>>,
}

impl<'v> SetRef<'v> {
    pub fn from_value(x: Value<'v>) -> Option<SetRef<'v>> {
        if x.unpack_frozen().is_some() {
            x.downcast_ref::<SetGen<FrozenSetData>>().map(|x| SetRef {
                aref: Either::Right(coerce(&x.0)),
            })
        } else {
            let ptr = x.downcast_ref::<SetGen<RefCell<SetData<'v>>>>()?;
            Some(SetRef {
                aref: Either::Left(ptr.0.borrow()),
            })
        }
    }
}

impl<'v> Deref for SetRef<'v> {
    type Target = SetData<'v>;

    fn deref(&self) -> &Self::Target {
        &self.aref
    }
}

impl<'v> Clone for SetRef<'v> {
    fn clone(&self) -> Self {
        match &self.aref {
            Either::Left(x) => SetRef {
                aref: Either::Left(Ref::clone(x)),
            },
            Either::Right(x) => SetRef {
                aref: Either::Right(*x),
            },
        }
    }
}

impl<'v> Dupe for SetRef<'v> {}

/// Mutably borrowed `Set`.
pub struct SetMut<'v> {
    pub(crate) aref: RefMut<'v, SetData<'v>>,
}

impl<'v> SetMut<'v> {
    /// Downcast the value to a mutable set reference.
    #[inline]
    pub fn from_value(x: Value<'v>) -> anyhow::Result<SetMut> {
        #[derive(thiserror::Error, Debug)]
        #[error("Value is not set, value type: `{0}`")]
        struct NotSetError(&'static str);

        #[cold]
        #[inline(never)]
        fn error<'v>(x: Value<'v>) -> anyhow::Error {
            if x.downcast_ref::<SetGen<FrozenSetData>>().is_some() {
                ValueError::CannotMutateImmutableValue.into()
            } else {
                NotSetError(x.get_type()).into()
            }
        }

        let ptr = x.downcast_ref::<SetGen<RefCell<SetData<'v>>>>();
        match ptr {
            None => Err(error(x)),
            Some(ptr) => match ptr.0.try_borrow_mut() {
                Ok(x) => Ok(SetMut { aref: x }),
                Err(_) => Err(ValueError::MutationDuringIteration.into()),
            },
        }
    }
}

impl<'v> Deref for SetMut<'v> {
    type Target = SetData<'v>;

    fn deref(&self) -> &Self::Target {
        &self.aref
    }
}

impl<'v> DerefMut for SetMut<'v> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.aref
    }
}
