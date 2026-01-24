/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use core::sync::atomic::AtomicI64;

use ::serde::Deserialize;
use ::serde::ser::Serialize;

use crate::traits::PagableDeserialize;
use crate::traits::PagableDeserializer;
use crate::traits::PagableSerialize;
use crate::traits::PagableSerializer;

impl PagableSerialize for () {
    fn pagable_serialize(&self, _serializer: &mut dyn PagableSerializer) -> crate::Result<()> {
        Ok(())
    }
}

impl<'de> PagableDeserialize<'de> for () {
    fn pagable_deserialize<D: PagableDeserializer<'de> + ?Sized>(
        _deserializer: &mut D,
    ) -> crate::Result<Self> {
        Ok(())
    }
}

impl<'a, T: PagableSerialize> PagableSerialize for &'a T {
    fn pagable_serialize(&self, serializer: &mut dyn PagableSerializer) -> crate::Result<()> {
        (**self).pagable_serialize(serializer)
    }
}

impl<T: PagableSerialize> PagableSerialize for Option<T> {
    fn pagable_serialize(&self, serializer: &mut dyn PagableSerializer) -> crate::Result<()> {
        bool::serialize(&self.is_some(), serializer.serde())?;
        if let Some(v) = self {
            v.pagable_serialize(serializer)?;
        }
        Ok(())
    }
}

impl<'de, T: PagableDeserialize<'de>> PagableDeserialize<'de> for Option<T> {
    fn pagable_deserialize<D: PagableDeserializer<'de> + ?Sized>(
        deserializer: &mut D,
    ) -> crate::Result<Self> {
        if bool::deserialize(deserializer.serde())? {
            Ok(Some(T::pagable_deserialize(deserializer)?))
        } else {
            Ok(None)
        }
    }
}

impl<T: ?Sized> PagableSerialize for std::marker::PhantomData<T> {
    fn pagable_serialize(&self, _serializer: &mut dyn PagableSerializer) -> crate::Result<()> {
        Ok(())
    }
}

impl<'de, T: ?Sized> PagableDeserialize<'de> for std::marker::PhantomData<T> {
    fn pagable_deserialize<D: PagableDeserializer<'de> + ?Sized>(
        _deserializer: &mut D,
    ) -> crate::Result<Self> {
        Ok(std::marker::PhantomData)
    }
}

impl<T: PagableSerialize> PagableSerialize for Box<T> {
    fn pagable_serialize(&self, serializer: &mut dyn PagableSerializer) -> crate::Result<()> {
        (**self).pagable_serialize(serializer)
    }
}

impl<'de, T: PagableDeserialize<'de>> PagableDeserialize<'de> for Box<T> {
    fn pagable_deserialize<D: PagableDeserializer<'de> + ?Sized>(
        deserializer: &mut D,
    ) -> crate::Result<Self> {
        Ok(Box::new(T::pagable_deserialize(deserializer)?))
    }
}

impl<T: PagableSerialize> PagableSerialize for [T] {
    fn pagable_serialize(&self, serializer: &mut dyn PagableSerializer) -> crate::Result<()> {
        usize::serialize(&self.len(), serializer.serde())?;
        for v in self.iter() {
            v.pagable_serialize(serializer)?;
        }
        Ok(())
    }
}

impl<T: PagableSerialize> PagableSerialize for Box<[T]> {
    fn pagable_serialize(&self, serializer: &mut dyn PagableSerializer) -> crate::Result<()> {
        <[T] as PagableSerialize>::pagable_serialize(&**self, serializer)
    }
}

impl<'de, T: PagableDeserialize<'de>> PagableDeserialize<'de> for Box<[T]> {
    fn pagable_deserialize<D: PagableDeserializer<'de> + ?Sized>(
        deserializer: &mut D,
    ) -> crate::Result<Self> {
        let items = usize::deserialize(deserializer.serde())?;
        let mut v = Vec::with_capacity(items);
        for _ in 0..items {
            v.push(T::pagable_deserialize(deserializer)?);
        }
        Ok(v.into_boxed_slice())
    }
}

impl PagableSerialize for AtomicI64 {
    fn pagable_serialize(&self, serializer: &mut dyn PagableSerializer) -> crate::Result<()> {
        Ok(i64::serialize(
            &self.load(core::sync::atomic::Ordering::Relaxed),
            serializer.serde(),
        )?)
    }
}

impl<'de> PagableDeserialize<'de> for AtomicI64 {
    fn pagable_deserialize<D: PagableDeserializer<'de> + ?Sized>(
        deserializer: &mut D,
    ) -> crate::Result<Self> {
        let val = i64::deserialize(deserializer.serde())?;
        Ok(AtomicI64::new(val))
    }
}
