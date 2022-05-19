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

//! Call-related bytecode interpreter code.

use std::{
    fmt,
    fmt::{Display, Formatter},
};

use gazebo::prelude::*;

use crate::{
    collections::symbol_map::Symbol,
    eval::{
        bc::{instr_arg::BcInstrArg, stack_ptr::BcStackPtr},
        compiler::def::FrozenDef,
        runtime::arguments::{
            ArgSymbol, ArgumentsFull, ArgumentsImpl, ArgumentsPos, ResolvedArgName,
        },
    },
    values::FrozenStringValue,
};

/// Call arguments.
pub(crate) trait BcCallArgs<S: ArgSymbol>: BcInstrArg {
    fn pop_from_stack<'a, 'v>(&'a self, stack: &'a BcStackPtr<'v, '_>) -> ArgumentsFull<'v, 'a, S>;
}

/// Call arguments for `def` call.
pub(crate) trait BcCallArgsForDef: BcInstrArg {
    type Args<'v, 'a>: ArgumentsImpl<'v, 'a, ArgSymbol = ResolvedArgName>
    where
        'v: 'a;

    fn pop_from_stack<'a, 'v>(&'a self, stack: &'a BcStackPtr<'v, '_>) -> Self::Args<'v, 'a>;
}

/// Full call arguments: positional, named, star and star-star. All taken from the stack.
#[derive(Debug)]
pub(crate) struct BcCallArgsFull<S: ArgSymbol> {
    pub(crate) pos_named: u32,
    pub(crate) names: Box<[(S, FrozenStringValue)]>,
    pub(crate) args: bool,
    pub(crate) kwargs: bool,
}

/// Positional-only call arguments, from stack.
#[derive(Debug)]
pub(crate) struct BcCallArgsPos {
    /// Number of positional arguments.
    pub(crate) pos: u32,
}

impl<S: ArgSymbol> BcCallArgsFull<S> {
    fn pos(&self) -> u32 {
        assert!(self.pos_named as usize >= self.names.len());
        self.pos_named - (self.names.len() as u32)
    }
}

impl BcCallArgsFull<Symbol> {
    pub(crate) fn resolve(self, def: &FrozenDef) -> BcCallArgsFull<ResolvedArgName> {
        let BcCallArgsFull {
            pos_named,
            names,
            args,
            kwargs,
        } = self;
        BcCallArgsFull {
            pos_named,
            names: names
                .into_vec()
                .into_map(|(name, value)| (def.resolve_arg_name(name.as_str_hashed()), value))
                .into_boxed_slice(),
            args,
            kwargs,
        }
    }
}

impl<S: ArgSymbol> Display for BcCallArgsFull<S> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut first = true;
        let mut write_sep = |f: &mut Formatter| {
            if !first {
                write!(f, " ")?;
            }
            first = false;
            Ok(())
        };
        // Number of positional arguments.
        if self.pos() != 0 {
            write_sep(f)?;
            write!(f, "{}", self.pos())?;
        }
        // Named arguments.
        for (_, name) in &*self.names {
            write_sep(f)?;
            write!(f, "{}", name.as_str())?;
        }
        // Star argument?
        if self.args {
            write_sep(f)?;
            write!(f, "*")?;
        }
        // Star-star argument?
        if self.kwargs {
            write_sep(f)?;
            write!(f, "**")?;
        }
        Ok(())
    }
}

impl<S: ArgSymbol> BcCallArgs<S> for BcCallArgsFull<S> {
    fn pop_from_stack<'a, 'v>(&'a self, stack: &'a BcStackPtr<'v, '_>) -> ArgumentsFull<'v, 'a, S> {
        stack.pop_args(self)
    }
}

impl<S: ArgSymbol> BcCallArgs<S> for BcCallArgsPos {
    fn pop_from_stack<'a, 'v>(&'a self, stack: &'a BcStackPtr<'v, '_>) -> ArgumentsFull<'v, 'a, S> {
        stack.pop_args_pos_as_full(self)
    }
}

impl BcCallArgsForDef for BcCallArgsFull<ResolvedArgName> {
    type Args<'v, 'a>
    where
        'v: 'a,
    = ArgumentsFull<'v, 'a, ResolvedArgName>;

    #[inline]
    fn pop_from_stack<'a, 'v>(
        &'a self,
        stack: &'a BcStackPtr<'v, '_>,
    ) -> ArgumentsFull<'v, 'a, ResolvedArgName> {
        stack.pop_args(self)
    }
}

impl BcCallArgsForDef for BcCallArgsPos {
    type Args<'v, 'a>
    where
        'v: 'a,
    = ArgumentsPos<'v, 'a, ResolvedArgName>;

    #[inline]
    fn pop_from_stack<'a, 'v>(
        &'a self,
        stack: &'a BcStackPtr<'v, '_>,
    ) -> ArgumentsPos<'v, 'a, ResolvedArgName> {
        stack.pop_args_pos(self)
    }
}
