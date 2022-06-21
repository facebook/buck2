/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::convert::TryFrom;
use std::fmt::Display;
use std::fmt::{self};
use std::fs::File;
use std::io::BufReader;
use std::sync::Arc;

use anyhow::Context;
use buck2_core::fs::anyhow as fs;
use buck2_core::fs::project::ProjectFilesystem;
use buck2_core::fs::project::ProjectRelativePathBuf;
use gazebo::any::ProvidesStaticType;
use gazebo::prelude::*;
use starlark::collections::SmallMap;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::values::dict::Dict;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Value;
use thiserror::Error;

use crate::actions::artifact::Artifact;

/// The Starlark representation of an `Artifact` on disk which can be accessed.
#[derive(Debug, ProvidesStaticType, NoSerialize)]
pub struct StarlarkArtifactValue {
    // We only keep the artifact for Display, since we don't want to leak the underlying path by default
    artifact: Artifact,
    path: ProjectRelativePathBuf,
    fs: Arc<ProjectFilesystem>,
}

impl Display for StarlarkArtifactValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.artifact, f)
    }
}

starlark_simple_value!(StarlarkArtifactValue);

impl StarlarkArtifactValue {
    /// Create a new artifact value. Must be materialised to disk before calling this function.
    pub fn new(
        artifact: Artifact,
        path: ProjectRelativePathBuf,
        fs: Arc<ProjectFilesystem>,
    ) -> Self {
        Self { artifact, path, fs }
    }
}

impl<'v> StarlarkValue<'v> for StarlarkArtifactValue {
    starlark_type!("artifact_value");

    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(artifact_value_methods)
    }
}

#[derive(Debug, Error)]
enum JsonError {
    #[error("JSON number is outside the bounds that Starlark supports, `{0}`")]
    NumberOutOfBounds(String),
}

fn json_convert<'v>(v: serde_json::Value, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
    match v {
        serde_json::Value::Null => Ok(Value::new_none()),
        serde_json::Value::Bool(x) => Ok(Value::new_bool(x)),
        serde_json::Value::Number(x) => {
            if let Some(x) = x.as_i64().and_then(|x| i32::try_from(x).ok()) {
                Ok(Value::new_int(x))
            } else if let Some(x) = x.as_f64() {
                Ok(heap.alloc(x))
            } else {
                Err(JsonError::NumberOutOfBounds(x.to_string()).into())
            }
        }
        serde_json::Value::String(x) => Ok(heap.alloc(x)),
        serde_json::Value::Array(xs) => Ok(heap.alloc(xs.into_try_map(|v| json_convert(v, heap))?)),
        serde_json::Value::Object(mp) => {
            let mut res = SmallMap::with_capacity(mp.len());
            for (k, v) in mp.into_iter() {
                res.insert_hashed(heap.alloc(k).get_hashed()?, json_convert(v, heap)?);
            }
            Ok(heap.alloc(Dict::new(res)))
        }
    }
}

#[starlark_module]
fn artifact_value_methods(builder: &mut MethodsBuilder) {
    fn read_string(this: &StarlarkArtifactValue) -> anyhow::Result<String> {
        let path = this.fs.resolve(&this.path);
        fs::read_to_string(&path)
    }

    fn read_json<'v>(this: &StarlarkArtifactValue, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        let path = this.fs.resolve(&this.path);
        let file = File::open(&path).with_context(|| format!("When openning file `{}`", path))?;
        let reader = BufReader::new(file);
        let value: serde_json::Value = serde_json::from_reader(reader)
            .with_context(|| format!("When parsing JSON file `{}`", path))?;
        json_convert(value, heap)
    }
}

#[cfg(test)]
mod tests {
    use starlark::values::Heap;

    use super::*;

    #[test]
    fn test_json_convert() {
        let heap = Heap::new();
        let testcase = "{\"test\": [1, true, \"pi\", 7.5, {}]}";
        let value: serde_json::Value = serde_json::from_str(testcase).unwrap();
        let res = json_convert(value, &heap).unwrap().to_repr();
        assert_eq!(res, testcase.replace("true", "True"))
    }
}
