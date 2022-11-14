/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use crate::Visitor;

/// This trait allows traversal of object graph.
///
/// # Proc macro
///
/// Typically implemented with proc macro. Like this:
///
/// ```
/// use allocative::Allocative;
///
/// #[derive(Allocative)]
/// struct Foo {
///     x: u32,
///     y: String,
/// }
/// ```
///
/// Proc macro supports two attributes: `#[allocative(skip)]` and `#[allocative(bound = "")]`.
///
/// ## `#[allocative(skip)]`
///
/// `#[allocative(skip)]` can be used to skip field from traversal
/// (for example, to skip fields which are not `Allocative`,
/// and can be skipped because they are cheap).
///
/// ```
/// use allocative::Allocative;
///
/// /// This does not implement `Allocative`.
/// struct Unsupported;
///
/// #[derive(Allocative)]
/// struct Bar {
///     #[allocative(skip)]
///     unsupported: Unsupported,
/// }
/// ```
///
/// ## `#[allocative(bound = "")]`
///
/// `#[allocative(bound = "")]` can be used to not add `T: Allocative` bound
/// to `Allocative` trait implementation, like this:
///
/// ```
/// use std::marker::PhantomData;
/// use allocative::Allocative;
///
/// struct Unsupported;
///
/// #[derive(Allocative)]
/// #[allocative(bound = "")]
/// struct Baz<T> {
///     _marker: PhantomData<T>,
/// }
///
/// // So `Baz<Unsupported>` is `Allocative` even though `Unsupported` is not.
/// let allocative: &dyn Allocative = &Baz::<Unsupported> { _marker: PhantomData };
/// ```
pub trait Allocative {
    fn visit<'a, 'b: 'a>(&self, visitor: &'a mut Visitor<'b>);
}
