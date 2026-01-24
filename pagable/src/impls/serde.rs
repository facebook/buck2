/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::time::Duration;

use anyhow::Context;
use serde::Deserialize;
use serde::Serialize;

// To support using `use_serde` both in and out of this crate, we use this import so we can have a common name.
use crate as pagable;
use crate::PagableDeserialize;
use crate::PagableSerialize;
use crate::PagableSerializer;

#[macro_export]
macro_rules! use_serde {
    ($ty:ty) => {
        impl pagable::PagableSerialize for $ty {
            fn pagable_serialize(
                &self,
                serializer: &mut dyn pagable::PagableSerializer,
            ) -> pagable::Result<()> {
                self.serialize(serializer.serde())
                    .with_context(|| format!("serializing type {}", std::any::type_name::<$ty>()))
            }
        }
        impl<'de> pagable::PagableDeserialize<'de> for $ty {
            fn pagable_deserialize<D: pagable::PagableDeserializer<'de> + ?Sized>(
                deserializer: &mut D,
            ) -> pagable::Result<Self> {
                <Self as pagable::__internal::serde::Deserialize<'de>>::deserialize(
                    deserializer.serde(),
                )
                .with_context(|| format!("deserializing type {}", std::any::type_name::<$ty>()))
            }
        }
    };
}

use_serde!(u8);
use_serde!(u16);
use_serde!(u32);
use_serde!(u64);
use_serde!(usize);
use_serde!(u128);
use_serde!(i8);
use_serde!(i16);
use_serde!(i32);
use_serde!(i64);
use_serde!(i128);
use_serde!(f32);
use_serde!(f64);
use_serde!(bool);
use_serde!(String);
use_serde!(Duration);

use_serde!(Box<str>);

impl PagableSerialize for serde_json::Value {
    fn pagable_serialize(&self, serializer: &mut dyn PagableSerializer) -> crate::Result<()> {
        match self {
            serde_json::Value::Null => {
                usize::serialize(&0, serializer.serde())?;
            }
            serde_json::Value::Bool(val) => {
                usize::serialize(&1, serializer.serde())?;
                bool::serialize(val, serializer.serde())?;
            }
            serde_json::Value::Number(val) => {
                usize::serialize(&2, serializer.serde())?;
                if let Some(val) = val.as_f64() {
                    bool::serialize(&true, serializer.serde())?;
                    f64::serialize(&val, serializer.serde())?;
                } else if let Some(val) = val.as_i64() {
                    bool::serialize(&false, serializer.serde())?;
                    i64::serialize(&val, serializer.serde())?;
                } else {
                    panic!()
                }
            }
            serde_json::Value::String(val) => {
                usize::serialize(&3, serializer.serde())?;
                String::serialize(val, serializer.serde())?;
            }
            serde_json::Value::Array(val) => {
                usize::serialize(&4, serializer.serde())?;
                val.pagable_serialize(serializer)?;
            }
            serde_json::Value::Object(val) => {
                usize::serialize(&5, serializer.serde())?;
                usize::serialize(&val.len(), serializer.serde())?;
                for (k, v) in val.iter() {
                    String::serialize(k, serializer.serde())?;
                    v.pagable_serialize(serializer)?;
                }
            }
        }
        Ok(())
    }
}

impl<'de> PagableDeserialize<'de> for serde_json::Value {
    fn pagable_deserialize<D: crate::PagableDeserializer<'de> + ?Sized>(
        deserializer: &mut D,
    ) -> crate::Result<Self> {
        let case = usize::deserialize(deserializer.serde())?;
        match case {
            0 => Ok(serde_json::Value::Null),
            1 => Ok(serde_json::Value::Bool(bool::deserialize(
                deserializer.serde(),
            )?)),
            2 => {
                let is_float = bool::deserialize(deserializer.serde())?;
                if is_float {
                    Ok(serde_json::Value::Number(
                        serde_json::Number::from_f64(f64::deserialize(deserializer.serde())?)
                            .ok_or_else(|| anyhow::anyhow!("deserialization error"))?,
                    ))
                } else {
                    Ok(serde_json::Value::Number(
                        serde_json::Number::from_i128(i64::deserialize(deserializer.serde())? as _)
                            .ok_or_else(|| anyhow::anyhow!("deserialization error"))?,
                    ))
                }
            }
            3 => Ok(serde_json::Value::String(String::deserialize(
                deserializer.serde(),
            )?)),
            4 => {
                let vec = <Vec<serde_json::Value>>::pagable_deserialize(deserializer)?;
                Ok(serde_json::Value::Array(vec))
            }
            5 => {
                let len = usize::deserialize(deserializer.serde())?;
                let mut map = serde_json::Map::with_capacity(len);
                for _ in 0..len {
                    let key = String::deserialize(deserializer.serde())?;
                    let value = Self::pagable_deserialize(deserializer)?;
                    map.insert(key, value);
                }
                Ok(serde_json::Value::Object(map))
            }
            case => Err(anyhow::anyhow!("invalid serde_json::Value case {}", case)),
        }
    }
}
