/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! 'DeferredData's are deferred work registered by a rule implementation that can be stored in
//! fields of providers.
//!
//! These are usually slow operations required to generate the information on 'Provider's.
//! For example, information that depend on the execution of some actions, or information that
//! require reading the filesystem.
//!
//! We model these asynchronous data to be stored on fields of 'Provider's as 'DeferredData'. Each
//! 'DeferredData' will hold an 'DeferredKey'. That key that maps to an implementation of
//! 'Deferred', which is the actual work to be ran when execution of the deferred is needed.

pub(crate) mod calculation;
pub mod types;
