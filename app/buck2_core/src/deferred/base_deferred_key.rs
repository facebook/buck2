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
use std::collections::hash_map::DefaultHasher;
use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hash;
use std::hash::Hasher;
use std::sync::Arc;

use allocative::Allocative;
use buck2_data::ToProtoMessage;
use buck2_data::action_key_owner::BaseDeferredKeyProto;
use cmp_any::PartialEqAny;
use dupe::Dupe;
use static_assertions::assert_eq_size;
use strong_hash::StrongHash;

use crate::fs::paths::forward_rel_path::ForwardRelativePath;
use crate::fs::paths::forward_rel_path::ForwardRelativePathBuf;
use crate::fs::project_rel_path::ProjectRelativePath;
use crate::fs::project_rel_path::ProjectRelativePathBuf;
use crate::global_cfg_options::GlobalCfgOptions;
use crate::target::configured_target_label::ConfiguredTargetLabel;
use crate::target::name::EQ_SIGN_SUBST;

pub trait BaseDeferredKeyDyn: Debug + Display + Any + Allocative + Send + Sync + 'static {
    fn eq_token(&self) -> PartialEqAny;
    fn hash(&self) -> u64;
    fn strong_hash(&self) -> u64;
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
    /// bxl anon target or bxl dynamic action node or bxl itself will return cfg else None
    fn global_cfg_options(&self) -> Option<GlobalCfgOptions>;
}

#[derive(Debug, derive_more::Display, Dupe, Clone, Allocative)]
pub struct BaseDeferredKeyBxl(pub Arc<dyn BaseDeferredKeyDyn>);

impl PartialEq for BaseDeferredKeyBxl {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq_token() == other.0.eq_token()
    }
}

#[derive(Debug, derive_more::Display, Dupe, Clone, Allocative)]
pub enum BaseDeferredKey {
    TargetLabel(ConfiguredTargetLabel),
    AnonTarget(Arc<dyn BaseDeferredKeyDyn>),
    BxlLabel(BaseDeferredKeyBxl),
}

assert_eq_size!(BaseDeferredKey, [usize; 3]);

impl PartialEq for BaseDeferredKey {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (BaseDeferredKey::TargetLabel(a), BaseDeferredKey::TargetLabel(b)) => a == b,
            (BaseDeferredKey::TargetLabel(_), _) => false,
            (BaseDeferredKey::AnonTarget(a), BaseDeferredKey::AnonTarget(b)) => {
                a.eq_token() == b.eq_token()
            }
            (BaseDeferredKey::AnonTarget(_), _) => false,
            (BaseDeferredKey::BxlLabel(a), BaseDeferredKey::BxlLabel(b)) => a == b,
            (BaseDeferredKey::BxlLabel(_), _) => false,
        }
    }
}

impl Eq for BaseDeferredKey {}

impl Hash for BaseDeferredKey {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            BaseDeferredKey::TargetLabel(a) => a.hash(state),
            BaseDeferredKey::AnonTarget(d) | BaseDeferredKey::BxlLabel(BaseDeferredKeyBxl(d)) => {
                d.hash().hash(state)
            }
        }
    }
}

impl StrongHash for BaseDeferredKey {
    fn strong_hash<H: Hasher>(&self, state: &mut H) {
        match self {
            BaseDeferredKey::TargetLabel(a) => a.strong_hash(state),
            BaseDeferredKey::AnonTarget(d) | BaseDeferredKey::BxlLabel(BaseDeferredKeyBxl(d)) => {
                d.strong_hash().strong_hash(state)
            }
        }
    }
}

impl BaseDeferredKey {
    pub fn unpack_target_label(&self) -> Option<&ConfiguredTargetLabel> {
        match self {
            BaseDeferredKey::TargetLabel(a) => Some(a),
            _ => None,
        }
    }

    pub fn configured_label(&self) -> Option<ConfiguredTargetLabel> {
        match self {
            BaseDeferredKey::TargetLabel(label) => Some(label.dupe()),
            BaseDeferredKey::AnonTarget(t) | BaseDeferredKey::BxlLabel(BaseDeferredKeyBxl(t)) => {
                t.configured_label()
            }
        }
    }

    pub(crate) fn make_hashed_path(
        &self,
        base: &ProjectRelativePath,
        prefix: &ForwardRelativePath,
        action_key: Option<&str>,
        path: &ForwardRelativePath,
        fully_hash_path: bool,
    ) -> buck2_error::Result<ProjectRelativePathBuf> {
        match self {
            BaseDeferredKey::TargetLabel(target) => {
                let cell_relative_path = target.pkg().cell_relative_path().as_str();
                let escaped_target_name = Self::escape_target_name(target.name().as_str());

                // It is performance critical that we use slices and allocate via `join` instead of
                // repeated calls to `join` on the path object because `join` allocates on each call,
                // which has a significant impact.
                let path_identifier = [
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
                ];

                let path_or_hash = if fully_hash_path {
                    let mut hasher = DefaultHasher::new();
                    path_identifier.hash(&mut hasher);

                    format!("{:016x}/", hasher.finish())
                } else {
                    path_identifier.concat()
                };

                let hashed_path = [
                    base.as_str(),
                    "/",
                    prefix.as_str(),
                    "/",
                    target.pkg().cell_name().as_str(),
                    "/",
                    path_or_hash.as_str(),
                    path.as_str(),
                ];

                Ok(ProjectRelativePathBuf::unchecked_new(hashed_path.concat()))
            }
            BaseDeferredKey::AnonTarget(d) | BaseDeferredKey::BxlLabel(BaseDeferredKeyBxl(d)) => {
                Ok(d.make_hashed_path(base, prefix, action_key, path))
            }
        }
    }

    pub fn make_unhashed_path(&self) -> Option<ForwardRelativePathBuf> {
        match self {
            BaseDeferredKey::TargetLabel(target) => Some(
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
            BaseDeferredKey::TargetLabel(t) => BaseDeferredKeyProto::TargetLabel(t.as_proto()),
            BaseDeferredKey::AnonTarget(d) | BaseDeferredKey::BxlLabel(BaseDeferredKeyBxl(d)) => {
                d.to_proto()
            }
        }
    }
}
