/*
 * Copyright 2018 The Starlark in Rust Authors.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

use crate as starlark;
use crate::{assert::Assert, environment::GlobalsBuilder};

// The example from the starlark_module documentation.
#[test]
fn test_starlark_module() {
    #[starlark_module]
    fn global(builder: &mut GlobalsBuilder) {
        fn cc_binary(name: &str, srcs: Vec<&str>) -> anyhow::Result<String> {
            // real implementation may write it to a global variable
            Ok(format!("{:?} {:?}", name, srcs))
        }
    }

    let mut a = Assert::new();
    a.globals_add(global);
    let v = a.pass("cc_binary(name='star', srcs=['a.cc', 'b.cc'])");
    assert_eq!(
        v.value().unpack_str().unwrap(),
        r#""star" ["a.cc", "b.cc"]"#
    );
}
