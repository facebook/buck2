/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use starlark::environment::GlobalsBuilder;
use starlark::eval::Evaluator;
use starlark::values::StringValue;
use starlark::values::Value;

use crate::extra::BuildContext;

#[starlark_module]
pub fn register_read_config(globals: &mut GlobalsBuilder) {
    #[starlark(speculative_exec_safe)]
    fn read_config<'v>(
        section: StringValue,
        key: StringValue,
        default: Option<Value<'v>>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        // In Buck v1, we read additional configuration information from /etc/buckconfig.d.
        // On devservers and other locations, the file fb_chef.ini has host_features.gvfs = true.
        // Replicate that specific key, otherwise we can't build targets like protoc.
        if section.as_str() == "host_features" && key.as_str() == "gvfs" {
            return Ok(eval.heap().alloc("true"));
        }

        let buckconfig = &BuildContext::from_context(eval)?.buckconfig;
        match buckconfig.get(section, key)? {
            Some(v) => Ok(v.to_value()),
            None => Ok(default.unwrap_or_else(Value::new_none)),
        }
    }
}
