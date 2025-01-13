/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;
use std::fmt::Display;
use std::fs::File;
use std::io::BufReader;

use allocative::Allocative;
use buck2_artifact::artifact::artifact_type::Artifact;
use buck2_core::fs::fs_util;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_error::BuckErrorContext;
use gazebo::prelude::*;
use starlark::any::ProvidesStaticType;
use starlark::collections::SmallMap;
use starlark::environment::GlobalsBuilder;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::values::dict::Dict;
use starlark::values::starlark_value;
use starlark::values::starlark_value_as_type::StarlarkValueAsType;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Value;

/// The Starlark representation of an `Artifact` on disk which can be accessed.
#[derive(Debug, ProvidesStaticType, NoSerialize, Allocative)]
pub struct StarlarkArtifactValue {
    // We only keep the artifact for Display, since we don't want to leak the underlying path by default
    artifact: Artifact,
    path: ProjectRelativePathBuf,
    fs: ProjectRoot,
}

impl Display for StarlarkArtifactValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.artifact, f)
    }
}

starlark_simple_value!(StarlarkArtifactValue);

impl StarlarkArtifactValue {
    /// Create a new artifact value. Must be materialised to disk before calling this function.
    pub fn new(artifact: Artifact, path: ProjectRelativePathBuf, fs: ProjectRoot) -> Self {
        Self { artifact, path, fs }
    }
}

#[starlark_value(type = "artifact_value")]
impl<'v> StarlarkValue<'v> for StarlarkArtifactValue {
    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(artifact_value_methods)
    }
}

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
enum JsonError {
    #[error("JSON number is outside the bounds that Starlark supports, `{0}`")]
    NumberOutOfBounds(String),
}

fn json_convert<'v>(v: serde_json::Value, heap: &'v Heap) -> starlark::Result<Value<'v>> {
    match v {
        serde_json::Value::Null => Ok(Value::new_none()),
        serde_json::Value::Bool(x) => Ok(Value::new_bool(x)),
        serde_json::Value::Number(x) => {
            if let Some(x) = x.as_i64().and_then(|x| i32::try_from(x).ok()) {
                Ok(heap.alloc(x))
            } else if let Some(x) = x.as_f64() {
                Ok(heap.alloc(x))
            } else {
                Err(starlark::Error::new_other(buck2_error::Error::from(
                    JsonError::NumberOutOfBounds(x.to_string()),
                )))
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
    fn read_string(this: &StarlarkArtifactValue) -> starlark::Result<String> {
        let path = this.fs.resolve(&this.path);
        Ok(fs_util::read_to_string(path).map_err(buck2_error::Error::from)?)
    }

    fn read_json<'v>(this: &StarlarkArtifactValue, heap: &'v Heap) -> starlark::Result<Value<'v>> {
        let path = this.fs.resolve(&this.path);
        let file = File::open(&path)
            .with_buck_error_context(|| format!("Error opening file `{}`", path))?;
        let reader = BufReader::new(file);
        let value: serde_json::Value = serde_json::from_reader(reader)
            .with_buck_error_context(|| format!("Error parsing JSON file `{}`", path))?;
        json_convert(value, heap)
    }
}

#[starlark_module]
pub(crate) fn register_artifact_value(globals: &mut GlobalsBuilder) {
    const ArtifactValue: StarlarkValueAsType<StarlarkArtifactValue> = StarlarkValueAsType::new();
}

#[cfg(test)]
mod tests {

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
