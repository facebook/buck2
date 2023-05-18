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
use buck2_core::base_deferred_key_dyn::BaseDeferredKeyDyn;
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
#[cfg_attr(feature = "gazebo_lint", allow(gazebo_lint_arc_on_dupe))] // Recursive type
pub enum DeferredKey {
    /// Base means it's the first deferred registered that can be looked up via the ID based on
    /// analysis of the 'ConfiguredTargetLabel'.
    #[display(fmt = "(target: `{}`, id: `{}`)", _0, _1)]
    Base(BaseDeferredKeyDyn, DeferredId),
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
    pub fn action_key(&self) -> String {
        let mut ids = Vec::new();
        let mut x = self;
        loop {
            match x {
                DeferredKey::Base(_, id) => {
                    ids.push(id);
                    break;
                }
                DeferredKey::Deferred(base, id) => {
                    ids.push(id);
                    x = base
                }
            }
        }
        // FIXME(ndmitchell): We'd like to have some kind of user supplied name/category here,
        // rather than using the usize ids, so things are a bit more stable and as these strings
        // are likely to come up in error messages users might see (e.g. with paths).
        ids.iter().rev().map(|x| x.as_usize().to_string()).join("_")
    }

    pub fn owner(&self) -> &BaseDeferredKeyDyn {
        let mut x = self;
        loop {
            match x {
                DeferredKey::Base(base, _) => return base,
                DeferredKey::Deferred(base, _) => x = base,
            }
        }
    }
}
