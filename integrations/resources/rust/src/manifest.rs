/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::fmt;
use std::path::Path;
use std::path::PathBuf;

use serde::de;

struct ResourcePath<'a> {
    base_dir: &'a Path,
}

impl<'de> de::Visitor<'de> for ResourcePath<'_> {
    type Value = PathBuf;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("string path")
    }

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        Ok(self.base_dir.join(v))
    }
}

impl<'de> de::DeserializeSeed<'de> for ResourcePath<'_> {
    type Value = PathBuf;

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: de::Deserializer<'de>,
    {
        deserializer.deserialize_str(self)
    }
}

pub struct ResourcesMap<'a> {
    base_dir: &'a Path,
}

impl<'a> ResourcesMap<'a> {
    pub fn new(base_dir: &'a Path) -> Self {
        ResourcesMap { base_dir }
    }
}

impl<'de> de::Visitor<'de> for ResourcesMap<'_> {
    type Value = HashMap<String, PathBuf>;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("map of string to path")
    }

    fn visit_map<M>(self, mut map: M) -> Result<Self::Value, M::Error>
    where
        M: de::MapAccess<'de>,
    {
        let mut manifest = HashMap::new();
        while let Some(key) = map.next_key::<String>()? {
            let seed = ResourcePath {
                base_dir: self.base_dir,
            };
            let value = map.next_value_seed(seed)?;
            manifest.insert(key, value);
        }
        Ok(manifest)
    }
}

impl<'de> de::DeserializeSeed<'de> for ResourcesMap<'_> {
    type Value = HashMap<String, PathBuf>;

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: de::Deserializer<'de>,
    {
        deserializer.deserialize_map(self)
    }
}

#[cfg(test)]
mod tests {
    use serde::de::DeserializeSeed as _;

    use super::*;

    #[test]
    fn test_resources_map_deserialize() {
        let deserializer =
            &mut serde_json::Deserializer::from_str(r#" {"a": "x.exe", "b": "y\\z.exe"} "#);
        let manifest = ResourcesMap::new(Path::new("/tmp"))
            .deserialize(deserializer)
            .unwrap();
        assert_eq!(
            manifest,
            HashMap::from([
                ("a".into(), "/tmp/x.exe".into()),
                ("b".into(), "/tmp/y\\z.exe".into()),
            ]),
        );
    }
}
