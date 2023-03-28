/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;
use std::marker::PhantomData;
use std::ops::Index;
use std::ops::IndexMut;

use derive_more::Display;

#[derive(Copy, Clone, Default, PartialEq)]
pub struct GraphVertexKind;

pub trait VertexKind: Copy + Clone + Default {}

impl VertexKind for GraphVertexKind {}

/// The ID of a Vertex. This can be used to index into AbstractVertexData. Those IDs are given a
/// kind so we don't confuse indices in a critical path with vertex indices in a graph.
#[derive(Copy, Clone, Default, Ord, PartialOrd, PartialEq, Eq, Display)]
#[display(fmt = "{}", "0")]
pub struct AbstractVertexId<Kind: VertexKind>(u32, PhantomData<Kind>);

impl<Kind> AbstractVertexId<Kind>
where
    Kind: VertexKind,
{
    pub fn new(v: u32) -> Self {
        Self(v, PhantomData)
    }
}

impl<T, Kind> Index<AbstractVertexId<Kind>> for AbstractVertexData<T, Kind>
where
    Kind: VertexKind,
{
    type Output = T;

    fn index(&self, index: AbstractVertexId<Kind>) -> &Self::Output {
        &self.0[index.0 as usize]
    }
}

impl<T, Kind> IndexMut<AbstractVertexId<Kind>> for AbstractVertexData<T, Kind>
where
    Kind: VertexKind,
{
    fn index_mut(&mut self, index: AbstractVertexId<Kind>) -> &mut Self::Output {
        &mut self.0[index.0 as usize]
    }
}

/// An Optional AbstractVertexId.
#[derive(Copy, Clone)]
pub struct AbstractOptionalVertexId<Kind: VertexKind>(
    /// -1 means None.
    i32,
    PhantomData<Kind>,
);

impl<Kind> From<AbstractVertexId<Kind>> for AbstractOptionalVertexId<Kind>
where
    Kind: VertexKind,
{
    #[inline]
    fn from(node: AbstractVertexId<Kind>) -> Self {
        Self(node.0 as _, PhantomData)
    }
}

impl<Kind> AbstractOptionalVertexId<Kind>
where
    Kind: VertexKind,
{
    #[inline]
    pub fn none() -> Self {
        Self(-1, PhantomData)
    }

    #[inline]
    pub fn is_some(self) -> bool {
        self.0 >= 0
    }

    #[inline]
    pub fn into_option(self) -> Option<AbstractVertexId<Kind>> {
        if self.0 >= 0 {
            return Some(AbstractVertexId(self.0 as _, PhantomData));
        }

        None
    }
}

/// A Vec that stores data indexed by Vertex.
#[derive(Clone)]
pub struct AbstractVertexData<T, Kind: VertexKind>(Vec<T>, PhantomData<Kind>);

impl<T, Kind> AbstractVertexData<T, Kind>
where
    Kind: VertexKind,
{
    pub fn new(v: Vec<T>) -> Self {
        Self(v, PhantomData)
    }

    pub fn keys(&self) -> impl Iterator<Item = AbstractVertexId<Kind>> + DoubleEndedIterator {
        // By construction the length of this is always less than the maximum vertex id.
        let len: u32 = self.0.len().try_into().unwrap();
        (0..len).map(AbstractVertexId::new)
    }

    pub fn iter(&self) -> impl Iterator<Item = (AbstractVertexId<Kind>, &T)> + DoubleEndedIterator {
        self.keys().map(|k| (k, &self.0[k.0 as usize]))
    }

    pub fn values(&mut self) -> impl Iterator<Item = &T> {
        self.0.iter()
    }

    pub fn values_mut(&mut self) -> impl Iterator<Item = &mut T> {
        self.0.iter_mut()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }
}

impl<Kind> fmt::Debug for AbstractVertexId<Kind>
where
    Kind: VertexKind,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}

impl<Kind> fmt::Debug for AbstractOptionalVertexId<Kind>
where
    Kind: VertexKind,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.into_option(), f)
    }
}

pub type VertexId = AbstractVertexId<GraphVertexKind>;
pub type OptionalVertexId = AbstractOptionalVertexId<GraphVertexKind>;
pub type VertexData<T> = AbstractVertexData<T, GraphVertexKind>;
