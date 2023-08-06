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

use crate::environment::GlobalsBuilder;
use crate::stdlib::funcs::dict::register_dict;
use crate::stdlib::funcs::list::register_list;
use crate::stdlib::funcs::min_max::register_min_max;
use crate::stdlib::funcs::other::register_other;
use crate::stdlib::funcs::zip::register_zip;

pub(crate) fn register_globals(globals: &mut GlobalsBuilder) {
    register_list(globals);
    register_dict(globals);
    register_min_max(globals);
    register_zip(globals);
    register_other(globals);
}
