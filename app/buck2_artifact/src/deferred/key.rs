/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use allocative::Allocative;
use buck2_core::base_deferred_key::BaseDeferredKey;
use dupe::Dupe;
use itertools::Itertools;

use crate::deferred::id::DeferredId;

/// A key to lookup a 'Deferred' of any result type
#[derive(
    Clone,
    Dupe,
    derive_more::Display,
    Debug,
    Eq,
    Hash,
    PartialEq,
    Allocative
)]
pub enum DeferredKey {
    /// Base means it's the first deferred registered that can be looked up via the ID based on
    /// analysis of the 'ConfiguredTargetLabel'.
    #[display(fmt = "(target: `{}`, id: `{}`)", _0, _1)]
    Base(BaseDeferredKey, DeferredId),
    /// Points to a 'Deferred' that is generated from another 'Deferred'. The 'DeferredID' can only
    /// be looked up based on the results of executing the deferred at 'DeferredKey'
    #[display(fmt = "(target: `{}`, id: `{}`)", _0, _1)]
    Deferred(Arc<DeferredKey>, DeferredId),
}

impl DeferredKey {
    pub fn id(&self) -> DeferredId {
        *match self {
            DeferredKey::Base(_, id) | DeferredKey::Deferred(_, id) => id,
        }
    }

    /// Create action_key information from the ids, uniquely
    /// identifying this action within this target.
    pub fn action_key(&self, additional_id: Option<u32>) -> String {
        let mut ids = Vec::new();
        if let Some(v) = additional_id {
            ids.push(v as usize);
        }
        let mut x = self;
        loop {
            match x {
                DeferredKey::Base(_, id) => {
                    ids.push(id.as_usize());
                    break;
                }
                DeferredKey::Deferred(base, id) => {
                    ids.push(id.as_usize());
                    x = base
                }
            }
        }
        // FIXME(ndmitchell): We'd like to have some kind of user supplied name/category here,
        // rather than using the usize ids, so things are a bit more stable and as these strings
        // are likely to come up in error messages users might see (e.g. with paths).
        ids.iter().rev().map(|x| x.to_string()).join("_")
    }

    pub fn owner(&self) -> &BaseDeferredKey {
        let mut x = self;
        loop {
            match x {
                DeferredKey::Base(base, _) => return base,
                DeferredKey::Deferred(base, _) => x = base,
            }
        }
    }
}

/// The base key. We can actually get rid of this and just use 'DeferredKey' if rule analysis is an
/// 'Deferred' itself. This is used to construct the composed 'DeferredKey::Deferred' or
/// 'DeferredKey::Base' type.
#[derive(
    Hash,
    Eq,
    PartialEq,
    Clone,
    Dupe,
    derive_more::Display,
    Debug,
    Allocative
)]

pub enum DeferredHolderKey {
    Base(BaseDeferredKey),
    // While DeferredKey is Dupe, it has quite a lot of Arc's inside it, so maybe an Arc here makes sense?
    // Maybe not?
    Deferred(Arc<DeferredKey>),
}

impl DeferredHolderKey {
    pub fn make_key(&self, id: DeferredId) -> DeferredKey {
        match self {
            DeferredHolderKey::Base(base) => DeferredKey::Base(base.dupe(), id),
            DeferredHolderKey::Deferred(base) => DeferredKey::Deferred(base.dupe(), id),
        }
    }

    pub fn owner(&self) -> &BaseDeferredKey {
        match self {
            DeferredHolderKey::Base(base) => base,
            DeferredHolderKey::Deferred(base) => base.owner(),
        }
    }

    /// Create action_key information from the ids, uniquely
    /// identifying this action within this target.
    pub fn action_key(&self, additional_id: u32) -> String {
        // FIXME(ndmitchell): We'd like to have some kind of user supplied name/category here,
        // rather than using the usize ids, so things are a bit more stable and as these strings
        // are likely to come up in error messages users might see (e.g. with paths).
        match self {
            DeferredHolderKey::Base(x) => {
                format!("{}_{}", x, additional_id)
            }
            DeferredHolderKey::Deferred(x) => x.action_key(Some(additional_id)),
        }
    }
}
