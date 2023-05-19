/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::any::Any;
use std::borrow::Cow;
use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hash;
use std::hash::Hasher;
use std::sync::Arc;

use allocative::Allocative;
use buck2_data::action_key_owner::BaseDeferredKeyProto;
use buck2_data::ToProtoMessage;
use dupe::Dupe;
use gazebo::cmp::PartialEqAny;

use crate::fs::paths::forward_rel_path::ForwardRelativePath;
use crate::fs::paths::forward_rel_path::ForwardRelativePathBuf;
use crate::fs::project_rel_path::ProjectRelativePath;
use crate::fs::project_rel_path::ProjectRelativePathBuf;
use crate::target::label::ConfiguredTargetLabel;
use crate::target::name::EQ_SIGN_SUBST;

pub trait BaseDeferredKeyDynImpl:
    Debug + Display + Any + Allocative + Send + Sync + 'static
{
    fn eq_token(&self) -> PartialEqAny;
    fn hash(&self) -> u64;
    fn make_hashed_path(
        &self,
        base: &ProjectRelativePath,
        prefix: &ForwardRelativePath,
        action_key: Option<&str>,
        path: &ForwardRelativePath,
    ) -> ProjectRelativePathBuf;
    /// Fake label for anon targets, `None` for BXL.
    fn configured_label(&self) -> Option<ConfiguredTargetLabel>;
    fn to_proto(&self) -> BaseDeferredKeyProto;
    fn into_any(self: Arc<Self>) -> Arc<dyn Any + Send + Sync>;
}

#[derive(Debug, derive_more::Display, Dupe, Clone, Allocative)]
pub enum BaseDeferredKeyDyn {
    TargetLabel(ConfiguredTargetLabel),
    AnonTarget(Arc<dyn BaseDeferredKeyDynImpl>),
    BxlLabel(Arc<dyn BaseDeferredKeyDynImpl>),
}

impl PartialEq for BaseDeferredKeyDyn {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (BaseDeferredKeyDyn::TargetLabel(a), BaseDeferredKeyDyn::TargetLabel(b)) => a == b,
            (BaseDeferredKeyDyn::TargetLabel(_), _) => false,
            (BaseDeferredKeyDyn::AnonTarget(a), BaseDeferredKeyDyn::AnonTarget(b)) => {
                a.eq_token() == b.eq_token()
            }
            (BaseDeferredKeyDyn::AnonTarget(_), _) => false,
            (BaseDeferredKeyDyn::BxlLabel(a), BaseDeferredKeyDyn::BxlLabel(b)) => {
                a.eq_token() == b.eq_token()
            }
            (BaseDeferredKeyDyn::BxlLabel(_), _) => false,
        }
    }
}

impl Eq for BaseDeferredKeyDyn {}

impl Hash for BaseDeferredKeyDyn {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            BaseDeferredKeyDyn::TargetLabel(a) => a.hash(state),
            BaseDeferredKeyDyn::AnonTarget(d) | BaseDeferredKeyDyn::BxlLabel(d) => {
                d.hash().hash(state)
            }
        }
    }
}

impl BaseDeferredKeyDyn {
    pub fn unpack_target_label(&self) -> Option<&ConfiguredTargetLabel> {
        match self {
            BaseDeferredKeyDyn::TargetLabel(a) => Some(a),
            _ => None,
        }
    }

    pub fn make_hashed_path(
        &self,
        base: &ProjectRelativePath,
        prefix: &ForwardRelativePath,
        action_key: Option<&str>,
        path: &ForwardRelativePath,
    ) -> ProjectRelativePathBuf {
        match self {
            BaseDeferredKeyDyn::TargetLabel(target) => {
                let cell_relative_path = target.pkg().cell_relative_path().as_str();
                let escaped_target_name = Self::escape_target_name(target.name().as_str());

                // It is performance critical that we use slices and allocate via `join` instead of
                // repeated calls to `join` on the path object because `join` allocates on each call,
                // which has a significant impact.
                let parts = [
                    base.as_str(),
                    "/",
                    prefix.as_str(),
                    "/",
                    target.pkg().cell_name().as_str(),
                    "/",
                    target.cfg().output_hash().as_str(),
                    if target.exec_cfg().is_some() { "-" } else { "" },
                    target
                        .exec_cfg()
                        .as_ref()
                        .map_or("", |x| x.output_hash().as_str()),
                    "/",
                    cell_relative_path,
                    if cell_relative_path.is_empty() {
                        ""
                    } else {
                        "/"
                    },
                    "__",
                    escaped_target_name.as_ref(),
                    "__",
                    "/",
                    if action_key.is_none() {
                        ""
                    } else {
                        "__action__"
                    },
                    action_key.unwrap_or_default(),
                    if action_key.is_none() { "" } else { "__/" },
                    path.as_str(),
                ];

                ProjectRelativePathBuf::unchecked_new(parts.concat())
            }
            BaseDeferredKeyDyn::AnonTarget(d) | BaseDeferredKeyDyn::BxlLabel(d) => {
                d.make_hashed_path(base, prefix, action_key, path)
            }
        }
    }

    pub fn make_unhashed_path(&self) -> Option<ForwardRelativePathBuf> {
        match self {
            BaseDeferredKeyDyn::TargetLabel(target) => Some(
                ForwardRelativePath::new(target.pkg().cell_name().as_str())
                    .unwrap()
                    .join(target.pkg().cell_relative_path()),
            ),
            _ => None,
        }
    }

    fn escape_target_name(target_name: &str) -> Cow<str> {
        // Equals sign is difficult to escape especially for cmd.exe on Windows
        // which doesn't follow common escaping rules.
        if target_name.contains('=') {
            Cow::Owned(target_name.replace('=', EQ_SIGN_SUBST))
        } else {
            Cow::Borrowed(target_name)
        }
    }

    pub fn to_proto(&self) -> BaseDeferredKeyProto {
        match self {
            BaseDeferredKeyDyn::TargetLabel(t) => BaseDeferredKeyProto::TargetLabel(t.as_proto()),
            BaseDeferredKeyDyn::AnonTarget(d) | BaseDeferredKeyDyn::BxlLabel(d) => d.to_proto(),
        }
    }
}
