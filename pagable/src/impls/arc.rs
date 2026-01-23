/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

#![allow(unused)]

use dupe::Dupe;
use strong_hash::StrongHash;

use crate::PagableDeserialize;
use crate::PagableDeserializer;
use crate::PagableSerialize;
use crate::PagableSerializer;
use crate::arc_erase::ArcErase;

impl<T: for<'a> PagableDeserialize<'a> + PagableSerialize + Send + Sync + std::any::Any + 'static>
    PagableSerialize for std::sync::Arc<T>
{
    fn pagable_serialize<S: PagableSerializer>(&self, serializer: &mut S) -> crate::Result<()> {
        serializer.serialize_arc(self.dupe())
    }
}

impl<'de, T: PagableSerialize + for<'a> PagableDeserialize<'a> + Send + Sync + std::any::Any>
    PagableDeserialize<'de> for std::sync::Arc<T>
{
    fn pagable_deserialize<D: PagableDeserializer<'de>>(
        deserializer: &mut D,
    ) -> crate::Result<Self> {
        deserializer.deserialize_arc::<Self>()
    }
}

impl<
    T: PagableSerialize
        + for<'a> PagableDeserialize<'a>
        + PagableSerialize
        + Send
        + Sync
        + std::any::Any,
> PagableSerialize for std::sync::Arc<[T]>
{
    fn pagable_serialize<S: PagableSerializer>(&self, serializer: &mut S) -> crate::Result<()> {
        serializer.serialize_arc(self.dupe())
    }
}

impl<'de, T: PagableSerialize + for<'a> PagableDeserialize<'a> + Send + Sync + std::any::Any>
    PagableDeserialize<'de> for std::sync::Arc<[T]>
{
    fn pagable_deserialize<D: PagableDeserializer<'de>>(
        deserializer: &mut D,
    ) -> crate::Result<Self> {
        deserializer.deserialize_arc::<Self>()
    }
}

impl<
    T: PagableSerialize
        + for<'a> PagableDeserialize<'a>
        + PagableSerialize
        + Send
        + Sync
        + std::any::Any,
> PagableSerialize for triomphe::Arc<T>
{
    fn pagable_serialize<S: PagableSerializer>(&self, serializer: &mut S) -> crate::Result<()> {
        serializer.serialize_arc(self.clone())
    }
}

impl<'de, T: PagableSerialize + for<'a> PagableDeserialize<'a> + Send + Sync + std::any::Any>
    PagableDeserialize<'de> for triomphe::Arc<T>
{
    fn pagable_deserialize<D: PagableDeserializer<'de>>(
        deserializer: &mut D,
    ) -> crate::Result<Self> {
        deserializer.deserialize_arc::<Self>()
    }
}

impl<
    T: PagableSerialize
        + for<'a> PagableDeserialize<'a>
        + PagableSerialize
        + Send
        + Sync
        + std::any::Any,
> PagableSerialize for triomphe::Arc<[T]>
{
    fn pagable_serialize<S: PagableSerializer>(&self, serializer: &mut S) -> crate::Result<()> {
        serializer.serialize_arc(self.clone())
    }
}

impl<'de, T: PagableSerialize + for<'a> PagableDeserialize<'a> + Send + Sync + std::any::Any>
    PagableDeserialize<'de> for triomphe::Arc<[T]>
{
    fn pagable_deserialize<D: PagableDeserializer<'de>>(
        deserializer: &mut D,
    ) -> crate::Result<Self> {
        deserializer.deserialize_arc::<Self>()
    }
}

impl<
    H: for<'a> PagableDeserialize<'a> + PagableSerialize + Send + Sync + std::any::Any,
    T: for<'a> PagableDeserialize<'a> + PagableSerialize + Send + Sync + std::any::Any,
> PagableSerialize for triomphe::ThinArc<H, T>
{
    fn pagable_serialize<S: PagableSerializer>(&self, serializer: &mut S) -> crate::Result<()> {
        serializer.serialize_arc(self.clone())
    }
}

impl<
    'de,
    H: PagableSerialize + for<'a> PagableDeserialize<'a> + Send + Sync + std::any::Any,
    T: PagableSerialize + for<'a> PagableDeserialize<'a> + Send + Sync + std::any::Any,
> PagableDeserialize<'de> for triomphe::ThinArc<H, T>
{
    fn pagable_deserialize<D: PagableDeserializer<'de>>(
        deserializer: &mut D,
    ) -> crate::Result<Self> {
        deserializer.deserialize_arc::<Self>()
    }
}
