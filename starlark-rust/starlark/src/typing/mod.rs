/*
 * Copyright 2019 The Starlark in Rust Authors.
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

// Notes:
//   We deal with list.append/list.extend/list.insert, which mutate their list argument
//   We ignore dict.setdefault/dict.update, as these are pretty complex functions
//   We consider "non-sensicle" operations like list.remove and == to have implied types that make them meaningful
//       even if they don't fail when doing something silly

//! Types required to support the [`typecheck`](crate::syntax::AstModule::typecheck) function.

pub(crate) mod bindings;
pub(crate) mod ctx;
pub(crate) mod oracle;
pub(crate) mod ty;
pub(crate) mod typecheck;

pub use bindings::Interface;
pub use oracle::OracleNoBuiltins;
pub use oracle::OracleNone;
pub use oracle::OracleSequence;
pub use oracle::TypingOracle;
pub use ty::Approximation;
pub use ty::Arg;
pub use ty::Param;
pub use ty::ParamMode;
pub use ty::Ty;
pub use ty::TyFunction;
pub use ty::TyName;
pub use ty::TyUnion;
pub use typecheck::TypeMap;
