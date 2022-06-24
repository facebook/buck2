/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::BTreeMap;
use std::fmt::Display;
use std::sync::Arc;

use crate::attrs::attr_type::arg::QueryExpansion;
use crate::attrs::attr_type::attr_config::AttrConfig;
use crate::attrs::attr_type::dep::DepAttrType;
use crate::attrs::attr_type::dep::ProviderIdSet;

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct QueryAttrType {
    pub inner: DepAttrType,
}

impl QueryAttrType {
    pub fn new(inner: DepAttrType) -> Self {
        Self { inner }
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct QueryAttr<C: AttrConfig> {
    pub providers: Option<Arc<ProviderIdSet>>,
    pub query: QueryAttrBase<C>,
}

impl<C: AttrConfig> QueryAttr<C> {
    pub fn query(&self) -> &str {
        self.query.query()
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct QueryMacroBase<C: AttrConfig> {
    pub expansion_type: QueryExpansion,
    pub query: QueryAttrBase<C>,
}

impl<C: AttrConfig> Display for QueryMacroBase<C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", &self.expansion_type, self.query.query())?;
        Ok(())
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct QueryAttrBase<C: AttrConfig> {
    pub query: String,
    pub resolved_literals: ResolvedQueryLiterals<C>,
}

impl<C: AttrConfig> QueryAttrBase<C> {
    pub fn query(&self) -> &str {
        &self.query
    }
}

pub type ResolvedQueryLiterals<C> = BTreeMap<String, <C as AttrConfig>::ProvidersType>;
