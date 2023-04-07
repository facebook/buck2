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

//! Implementation of iterators for string type.

use allocative::Allocative;
use derive_more::Display;
use starlark_derive::Freeze;
use starlark_derive::NoSerialize;
use starlark_derive::Trace;

use crate as starlark;
use crate::any::ProvidesStaticType;
use crate::coerce::Coerce;
use crate::starlark_type;
use crate::values::Heap;
use crate::values::StarlarkValue;
use crate::values::StringValue;
use crate::values::StringValueLike;
use crate::values::Value;
use crate::values::ValueLike;

/// An opaque iterator over a string, produced by elems/codepoints
#[derive(
    Debug,
    Trace,
    Coerce,
    Display,
    Freeze,
    NoSerialize,
    ProvidesStaticType,
    Allocative
)]
#[display(fmt = "iterator")]
#[repr(C)]
struct StringIteratorGen<'v, V: ValueLike<'v>> {
    string: V::String,
    produce_char: bool, // if not char, then int
}

pub(crate) fn iterate_chars<'v>(string: StringValue<'v>, heap: &'v Heap) -> Value<'v> {
    heap.alloc_complex(StringIteratorGen::<'v, Value<'v>> {
        string,
        produce_char: true,
    })
}

pub(crate) fn iterate_codepoints<'v>(string: StringValue<'v>, heap: &'v Heap) -> Value<'v> {
    heap.alloc_complex(StringIteratorGen::<'v, Value<'v>> {
        string,
        produce_char: false,
    })
}

impl<'v, V: ValueLike<'v> + 'v> StarlarkValue<'v> for StringIteratorGen<'v, V>
where
    Self: ProvidesStaticType,
{
    starlark_type!("iterator");

    fn iterate<'a>(
        &'a self,
        heap: &'v Heap,
    ) -> anyhow::Result<Box<dyn Iterator<Item = Value<'v>> + 'a>>
    where
        'v: 'a,
    {
        let s = self.string.as_str().chars();
        if self.produce_char {
            Ok(Box::new(s.map(move |x| heap.alloc(x))))
        } else {
            Ok(Box::new(s.map(|x| Value::new_int(u32::from(x) as i32))))
        }
    }

    fn with_iterator(
        &self,
        heap: &'v Heap,
        f: &mut dyn FnMut(&mut dyn Iterator<Item = Value<'v>>) -> anyhow::Result<()>,
    ) -> anyhow::Result<()> {
        let s = self.string.as_str().chars();
        if self.produce_char {
            f(&mut s.map(|x| heap.alloc(x)))
        } else {
            f(&mut s.map(|x| Value::new_int(u32::from(x) as i32)))
        }
    }
}
