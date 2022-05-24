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

use std::{
    cell::Cell, cmp, collections::HashMap, convert::TryInto, intrinsics::unlikely, iter,
    marker::PhantomData,
};

use either::Either;
use gazebo::{
    coerce::{coerce, Coerce},
    prelude::*,
};
use thiserror::Error;

use crate as starlark;
use crate::{
    collections::{
        symbol_map::{Symbol, SymbolMap},
        Hashed, SmallMap, StarlarkHashValue,
    },
    eval::Evaluator,
    values::{
        dict::{Dict, DictRef},
        docs,
        docs::DocString,
        Freeze, Heap, StringValue, Trace, UnpackValue, Value, ValueError, ValueLike,
    },
};

#[derive(Debug, Clone, Error)]
pub(crate) enum FunctionError {
    #[error("Missing parameter `{name}` for call to {function}")]
    MissingParameter { name: String, function: String },
    #[error("Found {count} extra positional parameter(s) for call to {function}")]
    ExtraPositionalParameters { count: usize, function: String },
    #[error("Found {} extra named parameter(s) for call to {function}", .names.join(" "))]
    ExtraNamedParameters {
        names: Vec<String>,
        function: String,
    },
    #[error("Parameter `{name}` occurs both explicitly and in **kwargs")]
    RepeatedParameter { name: String },
    #[error("The argument provided for *args is not an identifier")]
    ArgsValueIsNotString,
    #[error("The argument provided for *args is not iterable")]
    ArgsArrayIsNotIterable,
    #[error("The argument provided for **kwargs is not a dictionary")]
    KwArgsIsNotDict,
    #[error("Wrong number of positional parameters, expected {}, got {got}",
        if min == max {min.to_string()} else {format!("between {} and {}", min, max)})]
    WrongNumberOfParameters { min: usize, max: usize, got: usize },
}

#[derive(Debug, Clone, Coerce, PartialEq, Trace, Freeze)]
#[repr(C)]
pub(crate) enum ParameterKind<V> {
    Required,
    /// When optional parameter is not supplied, there's no error,
    /// but the slot remains `None`.
    ///
    /// This is used only in native code, parameters of type `Option<T>` become `Optional`.
    Optional,
    Defaulted(V),
    Args,
    KWargs,
}

#[derive(Debug, Copy, Clone, Dupe, PartialEq, Eq, Ord, PartialOrd)]
enum CurrentParameterStyle {
    /// Parameter can be only filled positionally.
    PosOnly,
    /// Parameter can be filled positionally or by name.
    PosOrNamed,
    /// Parameter can be filled by name only.
    NamedOnly,
    /// No more args accepted.
    NoMore,
}

/// Builder for [`ParametersSpec`]
pub struct ParametersSpecBuilder<V> {
    function_name: String,
    params: Vec<(String, ParameterKind<V>)>,
    names: SymbolMap<usize>,
    positional: usize,

    /// Has the no_args been passed
    current_style: CurrentParameterStyle,

    args: Option<usize>,
    kwargs: Option<usize>,
}

/// Define a list of parameters. This code assumes that all names are distinct and that
/// `*args`/`**kwargs` occur in well-formed locations.
// V = Value, or FrozenValue
#[derive(Debug, Clone, Trace, Freeze)]
#[repr(C)]
pub struct ParametersSpec<V> {
    /// Only used in error messages
    function_name: String,

    /// These two fields describe everything about the signature.
    /// The `kinds` lists all the arguments in the order they occur.
    /// The `names` gives a mapping from name to index where the argument lives.
    /// The only entries in `kinds` which are not in `names` are Args/KWargs,
    /// and the iteration order of `names` is the same order as `types`.
    params: Vec<(String, ParameterKind<V>)>,
    #[freeze(identity)]
    names: SymbolMap<usize>,

    /// Number of arguments that can be filled positionally.
    /// Excludes *args/**kwargs, keyword arguments after *args
    positional: usize,

    /// The index at which *args should go
    args: Option<usize>,
    /// The index at which **kwargs should go
    kwargs: Option<usize>,
}

// Can't derive this since we don't want ParameterKind to be public
unsafe impl<From: Coerce<To>, To> Coerce<ParametersSpec<To>> for ParametersSpec<From> {}

impl<V> ParametersSpecBuilder<V> {
    fn add(&mut self, name: &str, val: ParameterKind<V>) {
        assert!(!matches!(val, ParameterKind::Args | ParameterKind::KWargs));

        // Regular arguments cannot follow `**kwargs`, but can follow `*args`.
        assert!(self.current_style < CurrentParameterStyle::NoMore);
        assert!(self.kwargs.is_none());

        let i = self.params.len();
        self.params.push((name.to_owned(), val));
        if self.current_style != CurrentParameterStyle::PosOnly {
            let old = self.names.insert(name, i);
            assert!(old.is_none(), "Repeated parameter `{}`", name);
        }
        if self.args.is_none() && self.current_style != CurrentParameterStyle::NamedOnly {
            // If you've already seen `args` or `no_args`, you can't enter these
            // positionally
            self.positional = i + 1;
        }
    }

    /// Add a required parameter. Will be an error if the caller doesn't supply
    /// it. If you want to supply a position-only argument, prepend a `$` to
    /// the name.
    pub fn required(&mut self, name: &str) {
        self.add(name, ParameterKind::Required);
    }

    /// Add an optional parameter. Will be None if the caller doesn't supply it.
    /// If you want to supply a position-only argument, prepend a `$` to the
    /// name.
    pub fn optional(&mut self, name: &str) {
        self.add(name, ParameterKind::Optional);
    }

    /// Add an optional parameter. Will be the default value if the caller
    /// doesn't supply it. If you want to supply a position-only argument,
    /// prepend a `$` to the name.
    pub fn defaulted(&mut self, name: &str, val: V) {
        self.add(name, ParameterKind::Defaulted(val));
    }

    /// Add an `*args` parameter which will be an iterable sequence of parameters,
    /// recorded into a [`Vec`]. A function can only have one `args`
    /// parameter. After this call, any subsequent
    /// [`required`](ParametersSpecBuilder::required),
    /// [`optional`](ParametersSpecBuilder::optional) or
    /// [`defaulted`](ParametersSpecBuilder::defaulted)
    /// parameters can _only_ be supplied by name.
    pub fn args(&mut self) {
        assert!(self.args.is_none());
        assert!(self.current_style < CurrentParameterStyle::NamedOnly);
        assert!(self.kwargs.is_none());
        self.params.push(("*args".to_owned(), ParameterKind::Args));
        self.args = Some(self.params.len() - 1);
        self.current_style = CurrentParameterStyle::NamedOnly;
    }

    /// Following parameters can be filled positionally or by name.
    pub fn no_more_positional_only_args(&mut self) {
        assert_eq!(self.current_style, CurrentParameterStyle::PosOnly);
        self.current_style = CurrentParameterStyle::PosOrNamed;
    }

    /// This function has no `*args` parameter, corresponds to the Python parameter `*`.
    /// After this call, any subsequent
    /// [`required`](ParametersSpecBuilder::required),
    /// [`optional`](ParametersSpecBuilder::optional) or
    /// [`defaulted`](ParametersSpecBuilder::defaulted)
    /// parameters can _only_ be supplied by name.
    pub fn no_more_positional_args(&mut self) {
        assert!(self.args.is_none());
        assert!(self.current_style < CurrentParameterStyle::NamedOnly);
        assert!(self.kwargs.is_none());
        self.current_style = CurrentParameterStyle::NamedOnly;
    }

    /// Add a `**kwargs` parameter which will be a dictionary, recorded into a [`SmallMap`].
    /// A function can only have one `kwargs` parameter.
    /// parameter. After this call, any subsequent
    /// [`required`](ParametersSpecBuilder::required),
    /// [`optional`](ParametersSpecBuilder::optional) or
    /// [`defaulted`](ParametersSpecBuilder::defaulted)
    /// parameters can _only_ be supplied by position.
    pub fn kwargs(&mut self) {
        assert!(self.kwargs.is_none());
        self.params
            .push(("**kwargs".to_owned(), ParameterKind::KWargs));
        self.current_style = CurrentParameterStyle::NoMore;
        self.kwargs = Some(self.params.len() - 1);
    }

    /// Construct the parameters specification.
    pub fn finish(self) -> ParametersSpec<V> {
        let ParametersSpecBuilder {
            function_name,
            positional,
            args,
            current_style,
            kwargs,
            params,
            names,
        } = self;
        let _ = current_style;
        ParametersSpec {
            function_name,
            params,
            names,
            positional,
            args,
            kwargs,
        }
    }
}

impl<V> ParametersSpec<V> {
    /// Create a new [`ParametersSpec`] with the given function name.
    pub fn new(function_name: String) -> ParametersSpecBuilder<V> {
        Self::with_capacity(function_name, 0)
    }

    /// Create a new [`ParametersSpec`] with the given function name and an advance capacity hint.
    pub fn with_capacity(function_name: String, capacity: usize) -> ParametersSpecBuilder<V> {
        ParametersSpecBuilder {
            function_name,
            params: Vec::with_capacity(capacity),
            names: SymbolMap::with_capacity(capacity),
            positional: 0,
            current_style: CurrentParameterStyle::PosOnly,
            args: None,
            kwargs: None,
        }
    }

    /// Produce an approximate signature for the function, combining the name and arguments.
    pub fn signature(&self) -> String {
        let mut collector = String::new();
        self.collect_signature(&mut collector);
        collector
    }

    // Generate a good error message for it
    pub(crate) fn collect_signature(&self, collector: &mut String) {
        collector.push_str(&self.function_name);

        // We used to make the "name" of a function include all its parameters, but that is a lot of
        // details and visually crowds out everything else. Try disabling, although we might want it
        // in some contexts, so don't delete it.
    }

    /// Function parameter as they would appear in `def`
    /// (excluding types, default values and formatting).
    pub fn parameters_str(&self) -> String {
        let mut emitted_star = false;
        let mut collector = String::new();
        for (i, typ) in self.params.iter().enumerate() {
            if !collector.is_empty() {
                collector.push_str(", ");
            }

            // TODO: also print `/` for positional-only parameters.

            if i == self.positional
                && !emitted_star
                && !matches!(typ.1, ParameterKind::Args | ParameterKind::KWargs)
            {
                collector.push_str("*, ");
                emitted_star = true;
            }

            match typ.1 {
                ParameterKind::Args | ParameterKind::KWargs => {
                    // For `*args` or `**kwargs` param name includes the `*` or `**`.
                    collector.push_str(&typ.0);
                    emitted_star = true;
                }
                ParameterKind::Required => {
                    collector.push_str(&typ.0);
                }
                ParameterKind::Optional | ParameterKind::Defaulted(_) => {
                    collector.push_str(&typ.0);
                    collector.push_str(" = ...");
                }
            }
        }
        collector
    }

    /// Get the index where a user would have supplied "*" as a parameter.
    pub(crate) fn no_args_param_index(&self) -> Option<usize> {
        if self.positional < self.params.len() {
            match self.params.get(self.positional).map(|x| &x.1) {
                Some(ParameterKind::Args) | Some(ParameterKind::KWargs) => None,
                _ => Some(self.positional),
            }
        } else {
            None
        }
    }

    /// Iterate over the parameters
    ///
    /// Returns an iterator over (parameter index, name, kind)
    pub(crate) fn iter_params(&self) -> impl Iterator<Item = (&str, &ParameterKind<V>)> {
        self.params.iter().map(|(name, kind)| (name.as_str(), kind))
    }

    pub(crate) fn resolve_name(&self, name: Hashed<&str>) -> ResolvedArgName {
        let hash = name.hash();
        let param_index = self.names.get_hashed_str(name).map(|index| *index as u32);
        ResolvedArgName { hash, param_index }
    }
}

impl<'v, V: ValueLike<'v>> ParametersSpec<V> {
    /// Number of function parameters.
    pub fn len(&self) -> usize {
        self.params.len()
    }

    /// Move parameters from [`Arguments`] to a list of [`Value`],
    /// using the supplied [`ParametersSpec`].
    pub fn collect(
        &self,
        args: &Arguments<'v, '_>,
        slots: &[Cell<Option<Value<'v>>>],
        heap: &'v Heap,
    ) -> anyhow::Result<()> {
        self.collect_inline(&args.0, slots, heap)
    }

    /// Collect `N` arguments.
    ///
    /// This function is called by generated code.
    pub fn collect_into<const N: usize>(
        &self,
        args: &Arguments<'v, '_>,
        heap: &'v Heap,
    ) -> anyhow::Result<[Cell<Option<Value<'v>>>; N]> {
        let slots = [(); N].map(|_| Cell::new(None));
        self.collect(args, &slots, heap)?;
        Ok(slots)
    }

    /// A variant of collect that is always inlined
    /// for Def and NativeFunction that are hot-spots
    #[inline(always)]
    pub(crate) fn collect_inline<'a, A: ArgumentsImpl<'v, 'a>>(
        &self,
        args: &A,
        slots: &[Cell<Option<Value<'v>>>],
        heap: &'v Heap,
    ) -> anyhow::Result<()>
    where
        'v: 'a,
    {
        // If the arguments equal the length and the kinds, and we don't have any other args,
        // then no_args, *args and **kwargs must all be unset,
        // and we don't have to crate args/kwargs objects, we can skip everything else
        if args.pos().len() == self.positional
            && args.pos().len() == self.params.len()
            && args.named().is_empty()
            && args.args().is_none()
            && args.kwargs().is_none()
        {
            for (v, s) in args.pos().iter().zip(slots.iter()) {
                s.set(Some(*v));
            }

            return Ok(());
        }

        self.collect_slow(args, slots, heap)
    }

    fn collect_slow<'a, A: ArgumentsImpl<'v, 'a>>(
        &self,
        args: &A,
        slots: &[Cell<Option<Value<'v>>>],
        heap: &'v Heap,
    ) -> anyhow::Result<()>
    where
        'v: 'a,
    {
        /// Lazily initialized `kwargs` object.
        #[derive(Default)]
        struct LazyKwargs<'v> {
            kwargs: Option<Box<SmallMap<StringValue<'v>, Value<'v>>>>,
        }

        impl<'v> LazyKwargs<'v> {
            // Return true if the value is a duplicate
            #[inline(always)]
            fn insert(&mut self, key: Hashed<StringValue<'v>>, val: Value<'v>) -> bool {
                match &mut self.kwargs {
                    None => {
                        let mut mp = SmallMap::with_capacity_largest_vec();
                        mp.insert_hashed(key, val);
                        self.kwargs = Some(box mp);
                        false
                    }
                    Some(mp) => mp.insert_hashed(key, val).is_some(),
                }
            }

            fn alloc(self, heap: &'v Heap) -> Value<'v> {
                let kwargs = match self.kwargs {
                    Some(kwargs) => Dict::new(coerce(*kwargs)),
                    None => Dict::default(),
                };
                heap.alloc(kwargs)
            }
        }

        let len = self.params.len();
        // We might do unchecked stuff later on, so make sure we have as many slots as we expect
        assert!(slots.len() >= len);

        let mut star_args = Vec::new();
        let mut kwargs = LazyKwargs::default();
        let mut next_position = 0;

        // First deal with positional parameters
        if args.pos().len() <= self.positional {
            // fast path for when we don't need to bounce down to filling in args
            for (v, s) in args.pos().iter().zip(slots.iter()) {
                s.set(Some(*v));
            }
            next_position = args.pos().len();
        } else {
            for v in args.pos() {
                if next_position < self.positional {
                    slots[next_position].set(Some(*v));
                    next_position += 1;
                } else {
                    star_args.push(*v);
                }
            }
        }

        // Next deal with named parameters
        // The lowest position at which we've written a name.
        // If at the end lowest_name is less than next_position, we got the same variable twice.
        // So no duplicate checking until after all positional arguments
        let mut lowest_name = usize::MAX;
        // Avoid a lot of loop setup etc in the common case
        if !args.names().is_empty() {
            for ((name, name_value), v) in args.names().iter().zip(args.named()) {
                // Safe to use new_unchecked because hash for the Value and str are the same
                match name.get_index_from_param_spec(self) {
                    None => {
                        kwargs.insert(Hashed::new_unchecked(name.small_hash(), *name_value), *v);
                    }
                    Some(i) => {
                        slots[i].set(Some(*v));
                        lowest_name = cmp::min(lowest_name, i);
                    }
                }
            }
        }

        // Next up are the *args parameters
        if let Some(param_args) = args.args() {
            param_args
                .with_iterator(heap, |it| {
                    for v in it {
                        if next_position < self.positional {
                            slots[next_position].set(Some(v));
                            next_position += 1;
                        } else {
                            star_args.push(v);
                        }
                    }
                })
                .map_err(|_| FunctionError::ArgsArrayIsNotIterable)?;
        }

        // Check if the named arguments clashed with the positional arguments
        if unlikely(next_position > lowest_name) {
            return Err(FunctionError::RepeatedParameter {
                name: self.params[lowest_name].0.clone(),
            }
            .into());
        }

        // Now insert the kwargs, if there are any
        if let Some(param_kwargs) = args.kwargs() {
            match Dict::from_value(param_kwargs) {
                Some(y) => {
                    for (k, v) in y.iter_hashed() {
                        match StringValue::new(*k.key()) {
                            None => return Err(FunctionError::ArgsValueIsNotString.into()),
                            Some(s) => {
                                let repeat = match self
                                    .names
                                    .get_hashed_string_value(Hashed::new_unchecked(k.hash(), s))
                                {
                                    None => kwargs.insert(Hashed::new_unchecked(k.hash(), s), v),
                                    Some(i) => {
                                        let this_slot = &slots[*i];
                                        let repeat = this_slot.get().is_some();
                                        this_slot.set(Some(v));
                                        repeat
                                    }
                                };
                                if unlikely(repeat) {
                                    return Err(FunctionError::RepeatedParameter {
                                        name: s.as_str().to_owned(),
                                    }
                                    .into());
                                }
                            }
                        }
                    }
                }
                None => return Err(FunctionError::KwArgsIsNotDict.into()),
            }
        }

        // We have moved parameters into all the relevant slots, so need to finalise things.
        // We need to set default values and error if any required values are missing
        let kinds = &self.params;
        // This code is very hot, and setting up iterators was a noticeable bottleneck.
        for index in next_position..kinds.len() {
            // The number of locals must be at least the number of parameters, see `collect`
            // which reserves `max(_, kinds.len())`.
            let slot = unsafe { slots.get_unchecked(index) };
            let def = unsafe { kinds.get_unchecked(index) };

            // We know that up to next_position got filled positionally, so we don't need to check those
            if slot.get().is_some() {
                continue;
            }
            match def.1 {
                ParameterKind::Required => {
                    return Err(FunctionError::MissingParameter {
                        name: self.params[index].0.clone(),
                        function: self.signature(),
                    }
                    .into());
                }
                ParameterKind::Defaulted(x) => {
                    slot.set(Some(x.to_value()));
                }
                _ => {}
            }
        }

        // Now set the kwargs/args slots, if they are requested, and fail it they are absent but used
        // Note that we deliberately give warnings about missing parameters _before_ giving warnings
        // about unexpected extra parameters, so if a user mis-spells an argument they get a better error.
        if let Some(args_pos) = self.args {
            slots[args_pos].set(Some(heap.alloc_tuple(&star_args)));
        } else if unlikely(!star_args.is_empty()) {
            return Err(FunctionError::ExtraPositionalParameters {
                count: star_args.len(),
                function: self.signature(),
            }
            .into());
        }

        if let Some(kwargs_pos) = self.kwargs {
            slots[kwargs_pos].set(Some(kwargs.alloc(heap)));
        } else if let Some(kwargs) = kwargs.kwargs {
            return Err(FunctionError::ExtraNamedParameters {
                names: kwargs.keys().map(|x| x.as_str().to_owned()).collect(),
                function: self.signature(),
            }
            .into());
        }
        Ok(())
    }

    /// Check if current parameters can be filled with given arguments signature.
    #[allow(clippy::needless_range_loop)]
    pub fn can_fill_with_args(&self, pos: usize, names: &[&str]) -> bool {
        let mut filled = vec![false; self.params.len()];
        for p in 0..pos {
            if p < self.positional {
                filled[p] = true;
            } else if self.args.is_some() {
                // Filled into `*args`.
            } else {
                return false;
            }
        }
        if pos > self.positional && self.args.is_none() {
            return false;
        }
        for name in names {
            match self.names.get_str(name) {
                Some(i) => {
                    if filled[*i] {
                        // Duplicate argument.
                        return false;
                    }
                    filled[*i] = true;
                }
                None => {
                    if self.kwargs.is_none() {
                        return false;
                    }
                }
            }
        }
        for (filled, p) in filled.iter().zip(self.params.iter()) {
            if *filled {
                continue;
            }
            match &p.1 {
                ParameterKind::Args => {}
                ParameterKind::KWargs => {}
                ParameterKind::Defaulted(_) => {}
                ParameterKind::Optional => {}
                ParameterKind::Required => return false,
            }
        }
        true
    }

    /// Generate documentation for each of the parameters.
    ///
    /// # Arguments
    /// * `parameter_types` should be a mapping of parameter index to type
    /// * `parameter_docs` should be a mapping of parameter name to possible documentation for
    ///                    that parameter
    pub fn documentation(
        &self,
        mut parameter_types: HashMap<usize, docs::Type>,
        mut parameter_docs: HashMap<String, Option<DocString>>,
    ) -> Vec<docs::Param> {
        let mut params: Vec<docs::Param> = self
            .iter_params()
            .enumerate()
            .map(|(i, (name, kind))| {
                let typ = parameter_types.remove(&i);
                let docs = parameter_docs.remove(name).flatten();
                let name = name.to_owned();
                match kind {
                    ParameterKind::Required => docs::Param::Arg {
                        name,
                        docs,
                        typ,
                        default_value: None,
                    },
                    ParameterKind::Optional => docs::Param::Arg {
                        name,
                        docs,
                        typ,
                        default_value: Some("None".to_owned()),
                    },
                    ParameterKind::Defaulted(v) => docs::Param::Arg {
                        name,
                        docs,
                        typ,
                        default_value: Some(v.to_value().to_repr()),
                    },
                    ParameterKind::Args => docs::Param::Args { name, docs, typ },
                    ParameterKind::KWargs => docs::Param::Kwargs { name, docs, typ },
                }
            })
            .collect();

        // Go back and add the "*" arg if it's present
        if let Some(i) = self.no_args_param_index() {
            params.insert(i, docs::Param::NoArgs);
        }

        params
    }

    /// Create a [`ParametersParser`] for given arguments.
    pub fn parser<R, F>(
        &self,
        args: &Arguments<'v, '_>,
        eval: &mut Evaluator<'v, '_>,
        k: F,
    ) -> anyhow::Result<R>
    where
        F: FnOnce(ParametersParser<'v, '_>, &mut Evaluator<'v, '_>) -> anyhow::Result<R>,
    {
        eval.alloca_init(
            self.len(),
            || Cell::new(None),
            |slots, eval| {
                self.collect_inline(&args.0, slots, eval.heap())?;
                let parser = ParametersParser::new(slots);
                k(parser, eval)
            },
        )
    }
}

/// Parse a series of parameters which were specified by [`ParametersSpec`].
///
/// This is usually created with [`ParametersSpec::parser`].
pub struct ParametersParser<'v, 'a>(std::slice::Iter<'a, Cell<Option<Value<'v>>>>);

impl<'v, 'a> ParametersParser<'v, 'a> {
    /// Create a parameter parser, which stored parameters into provided slots reference.
    pub fn new(slots: &'a [Cell<Option<Value<'v>>>]) -> Self {
        Self(slots.iter())
    }

    fn get_next(&mut self) -> Option<Value<'v>> {
        let v = self
            .0
            .next()
            .expect("ParametersParser: wrong number of requested arguments");
        v.get()
    }

    /// Obtain the next parameter, corresponding to [`ParametersSpecBuilder::optional`].
    /// It is an error to request more parameters than were specified.
    /// The `name` is only used for error messages.
    pub fn next_opt<T: UnpackValue<'v>>(&mut self, name: &str) -> anyhow::Result<Option<T>> {
        match self.get_next() {
            None => Ok(None),
            Some(v) => Ok(Some(T::unpack_named_param(v, name)?)),
        }
    }

    /// Obtain the next parameter, which can't be defined by [`ParametersSpecBuilder::optional`].
    /// It is an error to request more parameters than were specified.
    /// The `name` is only used for error messages.
    pub fn next<T: UnpackValue<'v>>(&mut self, name: &str) -> anyhow::Result<T> {
        // After ParametersCollect.done() all variables will be Some,
        // apart from those where we called ParametersSpec.optional(),
        // and for those we chould call next_opt()

        // This is definitely not unassigned because ParametersCollect.done checked
        // that.
        let v = self.get_next().unwrap();
        T::unpack_named_param(v, name)
    }
}

/// An object accompanying argument name for faster argument resolution.
pub(crate) trait ArgSymbol: Coerce<Self> + 'static {
    fn get_index_from_param_spec<'v, V: ValueLike<'v>>(
        &self,
        ps: &ParametersSpec<V>,
    ) -> Option<usize>;

    fn small_hash(&self) -> StarlarkHashValue;
}

impl ArgSymbol for Symbol {
    fn get_index_from_param_spec<'v, V: ValueLike<'v>>(
        &self,
        ps: &ParametersSpec<V>,
    ) -> Option<usize> {
        ps.names.get(self).copied()
    }

    fn small_hash(&self) -> StarlarkHashValue {
        self.small_hash()
    }
}

/// `Symbol` resolved to function parameter index.
pub(crate) struct ResolvedArgName {
    /// Hash of the argument name.
    hash: StarlarkHashValue,
    /// Parameter index or `None` if the argument should go to kwargs.
    param_index: Option<u32>,
}

impl ArgSymbol for ResolvedArgName {
    fn get_index_from_param_spec<'v, V: ValueLike<'v>>(
        &self,
        _ps: &ParametersSpec<V>,
    ) -> Option<usize> {
        self.param_index.map(|i| i as usize)
    }

    fn small_hash(&self) -> StarlarkHashValue {
        self.hash
    }
}

unsafe impl Coerce<ResolvedArgName> for ResolvedArgName {}

#[derive(Debug, Clone_, Dupe_, Default_)]
pub(crate) struct ArgNames<'a, 'v, S: ArgSymbol> {
    /// Names are not guaranteed to be unique here.
    names: &'a [(S, StringValue<'v>)],
}

impl<'a, 'v, S: ArgSymbol> Copy for ArgNames<'a, 'v, S> {}

impl<'a, 'v, S: ArgSymbol> ArgNames<'a, 'v, S> {
    /// Names are allowed to be not-unique.
    /// String in `Symbol` must be equal to the `StringValue`,
    /// it is caller responsibility to ensure that.
    pub(crate) fn new(names: &'a [(S, StringValue<'v>)]) -> ArgNames<'a, 'v, S> {
        ArgNames { names }
    }

    pub(crate) fn names(&self) -> &'a [(S, StringValue<'v>)] {
        self.names
    }

    pub(crate) fn iter(&self) -> impl ExactSizeIterator<Item = &'a (S, StringValue<'v>)> {
        self.names.iter()
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.names.is_empty()
    }

    pub(crate) fn len(&self) -> usize {
        self.names.len()
    }
}

/// Either full arguments, or short arguments for positional-only calls.
pub(crate) trait ArgumentsImpl<'v, 'a> {
    type ArgSymbol: ArgSymbol;
    fn pos(&self) -> &[Value<'v>];
    fn named(&self) -> &[Value<'v>];
    fn names(&self) -> ArgNames<'a, 'v, Self::ArgSymbol>;
    fn args(&self) -> Option<Value<'v>>;
    fn kwargs(&self) -> Option<Value<'v>>;
}

/// Arguments object is passed from the starlark interpreter to function implementation
/// when evaluation function or method calls.
#[derive(Default_, Clone_, Dupe_)]
pub(crate) struct ArgumentsFull<'v, 'a, S: ArgSymbol> {
    /// Positional arguments.
    pub(crate) pos: &'a [Value<'v>],
    /// Named arguments.
    pub(crate) named: &'a [Value<'v>],
    /// Names of named arguments.
    ///
    /// `named` length must be equal to `names` length.
    pub(crate) names: ArgNames<'a, 'v, S>,
    /// `*args` argument.
    pub(crate) args: Option<Value<'v>>,
    /// `**kwargs` argument.
    pub(crate) kwargs: Option<Value<'v>>,
}

impl<'v, 'a, S: ArgSymbol> ArgumentsImpl<'v, 'a> for ArgumentsFull<'v, 'a, S> {
    type ArgSymbol = S;

    #[inline]
    fn pos(&self) -> &[Value<'v>] {
        self.pos
    }

    #[inline]
    fn named(&self) -> &[Value<'v>] {
        self.named
    }

    #[inline]
    fn names(&self) -> ArgNames<'a, 'v, S> {
        self.names
    }

    #[inline]
    fn args(&self) -> Option<Value<'v>> {
        self.args
    }

    #[inline]
    fn kwargs(&self) -> Option<Value<'v>> {
        self.kwargs
    }
}

/// Positional-only arguments, smaller and faster than `ArgumentsFull`.
pub(crate) struct ArgumentsPos<'v, 'a, S: ArgSymbol> {
    pub(crate) pos: &'a [Value<'v>],
    pub(crate) names: PhantomData<&'static S>,
}

impl<'a, 'v, S: ArgSymbol> ArgumentsImpl<'v, 'a> for ArgumentsPos<'v, 'a, S> {
    type ArgSymbol = S;

    #[inline]
    fn pos(&self) -> &[Value<'v>] {
        self.pos
    }

    #[inline]
    fn named(&self) -> &[Value<'v>] {
        &[]
    }

    #[inline]
    fn names(&self) -> ArgNames<'a, 'v, S> {
        ArgNames::default()
    }

    #[inline]
    fn args(&self) -> Option<Value<'v>> {
        None
    }

    #[inline]
    fn kwargs(&self) -> Option<Value<'v>> {
        None
    }
}

/// Arguments object is passed from the starlark interpreter to function implementation
/// when evaluation function or method calls.
#[derive(Default, Clone, Dupe_)]
pub struct Arguments<'v, 'a>(pub(crate) ArgumentsFull<'v, 'a, Symbol>);

impl<'v, 'a> Arguments<'v, 'a> {
    /// Unwrap all named arguments (both explicit and in `**kwargs`) into a map.
    ///
    /// This operation fails if named argument names are not unique.
    pub fn names_map(&self) -> anyhow::Result<SmallMap<StringValue<'v>, Value<'v>>> {
        match self.unpack_kwargs()? {
            None => {
                let mut result = SmallMap::with_capacity(self.0.names.len());
                for (k, v) in self.0.names.iter().zip(self.0.named) {
                    result.insert_hashed(Hashed::new_unchecked(k.0.small_hash(), k.1), *v);
                }
                Ok(result)
            }
            Some(kwargs) => {
                if self.0.names.is_empty() {
                    match kwargs.downcast_ref_key_string() {
                        Some(kwargs) => Ok(kwargs.clone()),
                        None => Err(FunctionError::ArgsValueIsNotString.into()),
                    }
                } else {
                    // We have to insert the names before the kwargs since the iteration order is observable
                    let mut result = SmallMap::with_capacity(self.0.names.len() + kwargs.len());
                    for (k, v) in self.0.names.iter().zip(self.0.named) {
                        result.insert_hashed(Hashed::new_unchecked(k.0.small_hash(), k.1), *v);
                    }
                    for (k, v) in kwargs.iter_hashed() {
                        let s = Arguments::unpack_kwargs_key_as_value(*k.key())?;
                        let k = Hashed::new_unchecked(k.hash(), s);
                        let old = result.insert_hashed(k, v);
                        if unlikely(old.is_some()) {
                            return Err(FunctionError::RepeatedParameter {
                                name: s.as_str().to_owned(),
                            }
                            .into());
                        }
                    }
                    Ok(result)
                }
            }
        }
    }

    /// Unwrap all named arguments (both explicit and in `**kwargs`) into a dictionary.
    ///
    /// This operation fails if named argument names are not unique.
    pub fn names(&self) -> anyhow::Result<Dict<'v>> {
        Ok(Dict::new(coerce(self.names_map()?)))
    }

    /// Unpack all positional parameters into an iterator.
    pub fn positions<'b>(
        &'b self,
        heap: &'v Heap,
    ) -> anyhow::Result<impl Iterator<Item = Value<'v>> + 'b> {
        let tail = match self.0.args {
            None => Either::Left(iter::empty()),
            Some(args) => Either::Right(args.iterate(heap)?),
        };
        Ok(self.0.pos.iter().copied().chain(tail))
    }

    /// Examine the `kwargs` field, converting it to a [`Dict`] or failing.
    /// Note that even if this operation succeeds, the keys in the kwargs
    /// will _not_ have been validated to be strings (as they must be).
    /// The arguments may also overlap with named, which would be an error.
    #[inline(always)]
    pub fn unpack_kwargs(&self) -> anyhow::Result<Option<DictRef<'v>>> {
        match self.0.kwargs {
            None => Ok(None),
            Some(kwargs) => match Dict::from_value(kwargs) {
                None => Err(FunctionError::KwArgsIsNotDict.into()),
                Some(x) => Ok(Some(x)),
            },
        }
    }

    /// Confirm that a key in the `kwargs` field is indeed a string, or [`Err`].
    #[inline(always)]
    pub(crate) fn unpack_kwargs_key_as_value(k: Value<'v>) -> anyhow::Result<StringValue<'v>> {
        match StringValue::new(k) {
            None => Err(FunctionError::ArgsValueIsNotString.into()),
            Some(k) => Ok(k),
        }
    }

    /// Confirm that a key in the `kwargs` field is indeed a string, or [`Err`].
    #[inline(always)]
    pub fn unpack_kwargs_key(k: Value<'v>) -> anyhow::Result<&'v str> {
        Arguments::unpack_kwargs_key_as_value(k).map(|k| k.as_str())
    }

    /// Produce [`Err`] if there are any positional arguments.
    #[inline(always)]
    pub fn no_positional_args(&self, heap: &'v Heap) -> anyhow::Result<()> {
        let [] = self.positional(heap)?;
        Ok(())
    }

    /// Produce [`Err`] if there are any named (i.e. non-positional) arguments.
    #[inline(always)]
    pub fn no_named_args(&self) -> anyhow::Result<()> {
        #[cold]
        #[inline(never)]
        fn bad(x: &Arguments) -> anyhow::Result<()> {
            // We might have a empty kwargs dictionary, but probably have an error
            let mut extra = Vec::new();
            extra.extend(x.0.names.iter().map(|x| x.0.as_str().to_owned()));
            if let Some(kwargs) = x.unpack_kwargs()? {
                for k in kwargs.keys() {
                    extra.push(Arguments::unpack_kwargs_key(k)?.to_owned());
                }
            }
            if extra.is_empty() {
                Ok(())
            } else {
                // Would be nice to give a better name here, but it's in the call stack, so no big deal
                Err(FunctionError::ExtraNamedParameters {
                    names: extra,
                    function: "function".to_owned(),
                }
                .into())
            }
        }

        if self.0.named.is_empty() && self.0.kwargs.is_none() {
            Ok(())
        } else {
            bad(self)
        }
    }

    /// Collect exactly `N` positional arguments from the [`Arguments`], failing if there are too many/few
    /// arguments. Ignores named arguments.
    #[inline(always)]
    pub fn positional<const N: usize>(&self, heap: &'v Heap) -> anyhow::Result<[Value<'v>; N]> {
        #[cold]
        #[inline(never)]
        fn rare<'v, const N: usize>(
            x: &Arguments<'v, '_>,
            heap: &'v Heap,
        ) -> anyhow::Result<[Value<'v>; N]> {
            // Very sad that we allocate into a vector, but I expect calling into a small positional argument
            // with a *args is very rare.
            let xs =
                x.0.pos
                    .iter()
                    .copied()
                    .chain(x.0.args.unwrap().iterate(heap)?)
                    .collect::<Vec<_>>();
            xs.as_slice().try_into().map_err(|_| {
                FunctionError::WrongNumberOfParameters {
                    min: N,
                    max: N,
                    got: x.0.pos.len(),
                }
                .into()
            })
        }

        if self.0.args.is_none() {
            self.0.pos.try_into().map_err(|_| {
                FunctionError::WrongNumberOfParameters {
                    min: N,
                    max: N,
                    got: self.0.pos.len(),
                }
                .into()
            })
        } else {
            rare(self, heap)
        }
    }

    /// Collect exactly `REQUIRED` positional arguments, plus at most `OPTIONAL` positional arguments
    /// from the [`Arguments`], failing if there are too many/few arguments. Ignores named arguments.
    /// The `OPTIONAL` array will never have a [`Some`] after a [`None`].
    #[inline(always)]
    pub fn optional<const REQUIRED: usize, const OPTIONAL: usize>(
        &self,
        heap: &'v Heap,
    ) -> anyhow::Result<([Value<'v>; REQUIRED], [Option<Value<'v>>; OPTIONAL])> {
        #[cold]
        #[inline(never)]
        fn rare<'v, const REQUIRED: usize, const OPTIONAL: usize>(
            x: &Arguments<'v, '_>,
            heap: &'v Heap,
        ) -> anyhow::Result<([Value<'v>; REQUIRED], [Option<Value<'v>>; OPTIONAL])> {
            // Very sad that we allocate into a vector, but I expect calling into a small positional argument
            // with a *args is very rare.
            let args = match x.0.args {
                None => box None.into_iter(),
                Some(args) => args.iterate(heap)?,
            };
            let xs = x.0.pos.iter().copied().chain(args).collect::<Vec<_>>();
            if xs.len() >= REQUIRED && xs.len() <= REQUIRED + OPTIONAL {
                let required = xs[0..REQUIRED].try_into().unwrap();
                let mut optional = [None; OPTIONAL];
                for (a, b) in optional.iter_mut().zip(&xs[REQUIRED..]) {
                    *a = Some(*b);
                }
                Ok((required, optional))
            } else {
                Err(FunctionError::WrongNumberOfParameters {
                    min: REQUIRED,
                    max: REQUIRED + OPTIONAL,
                    got: xs.len(),
                }
                .into())
            }
        }

        if self.0.args.is_none()
            && self.0.pos.len() >= REQUIRED
            && self.0.pos.len() <= REQUIRED + OPTIONAL
        {
            let required = self.0.pos[0..REQUIRED].try_into().unwrap();
            let mut optional = [None; OPTIONAL];
            for (a, b) in optional.iter_mut().zip(&self.0.pos[REQUIRED..]) {
                *a = Some(*b);
            }
            Ok((required, optional))
        } else {
            rare(self, heap)
        }
    }

    /// Collect 1 positional arguments from the [`Arguments`], failing if there are too many/few
    /// arguments. Ignores named arguments.
    #[inline(always)]
    pub fn positional1(&self, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        // Could be implemented more directly, let's see if profiling shows it up
        let [x] = self.positional(heap)?;
        Ok(x)
    }

    /// Collect up to 1 optional arguments from the [`Arguments`], failing if there are too many
    /// arguments. Ignores named arguments.
    #[inline(always)]
    pub fn optional1(&self, heap: &'v Heap) -> anyhow::Result<Option<Value<'v>>> {
        // Could be implemented more directly, let's see if profiling shows it up
        let ([], [x]) = self.optional(heap)?;
        Ok(x)
    }
}

impl Arguments<'_, '_> {
    /// Utility for checking a `this` parameter matches what you expect.
    pub fn check_this<'v, T: UnpackValue<'v>>(this: Value<'v>) -> anyhow::Result<T> {
        T::unpack_named_param(this, "this")
    }

    /// Utility for checking a required parameter matches what you expect.
    pub fn check_required<'v, T: UnpackValue<'v>>(
        name: &str,
        x: Option<Value<'v>>,
    ) -> anyhow::Result<T> {
        let x = x.ok_or_else(|| ValueError::MissingRequired(name.to_owned()))?;
        T::unpack_named_param(x, name)
    }

    /// Utility for checking an optional parameter matches what you expect.
    pub fn check_optional<'v, T: UnpackValue<'v>>(
        name: &str,
        x: Option<Value<'v>>,
    ) -> anyhow::Result<Option<T>> {
        match x {
            None => Ok(None),
            Some(x) => Ok(Some(T::unpack_value(x).ok_or_else::<anyhow::Error, _>(
                || {
                    ValueError::IncorrectParameterTypeNamedWithExpected(
                        name.to_owned(),
                        T::expected(),
                        x.get_type().to_owned(),
                    )
                    .into()
                },
            )?)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{assert::Assert, eval::compiler::def::FrozenDef, values::FrozenValue};

    #[test]
    fn test_parameter_unpack() {
        let heap = Heap::new();
        fn f<'v, F: Fn(&Arguments<'v, '_>), const N: usize>(heap: &'v Heap, op: F) {
            for i in 0..=N {
                let mut p = Arguments::default();
                let pos = (0..i).map(|x| Value::new_int(x as i32)).collect::<Vec<_>>();
                let args = (i..N).map(|x| Value::new_int(x as i32)).collect::<Vec<_>>();
                let empty_args = args.is_empty();
                p.0.pos = &pos;
                p.0.args = Some(heap.alloc(args));
                op(&p);
                if empty_args {
                    p.0.args = None;
                    op(&p);
                }
            }
        }

        f::<_, 0>(&heap, |p| {
            assert_eq!(&p.positional::<0>(&heap).unwrap(), &[]);
            assert!(&p.positional::<1>(&heap).is_err());
            assert!(&p.positional::<2>(&heap).is_err());
            assert_eq!(&p.optional::<0, 1>(&heap).unwrap(), &([], [None]));
            assert!(&p.optional::<1, 1>(&heap).is_err());
            assert_eq!(&p.optional::<0, 2>(&heap).unwrap(), &([], [None, None]));
        });
        f::<_, 1>(&heap, |p| {
            assert!(&p.positional::<0>(&heap).is_err());
            assert_eq!(&p.positional::<1>(&heap).unwrap(), &[Value::new_int(0)]);
            assert!(&p.positional::<2>(&heap).is_err());
            assert_eq!(
                &p.optional::<0, 1>(&heap).unwrap(),
                &([], [Some(Value::new_int(0))])
            );
            assert_eq!(
                &p.optional::<1, 1>(&heap).unwrap(),
                &([Value::new_int(0)], [None])
            );
            assert_eq!(
                &p.optional::<0, 2>(&heap).unwrap(),
                &([], [Some(Value::new_int(0)), None])
            );
        });
        f::<_, 2>(&heap, |p| {
            assert!(&p.positional::<0>(&heap).is_err());
            assert!(&p.positional::<1>(&heap).is_err());
            assert_eq!(
                &p.positional::<2>(&heap).unwrap(),
                &[Value::new_int(0), Value::new_int(1)]
            );
            assert!(p.optional::<0, 1>(&heap).is_err());
            assert_eq!(
                &p.optional::<1, 1>(&heap).unwrap(),
                &([Value::new_int(0)], [Some(Value::new_int(1))])
            );
            assert_eq!(
                &p.optional::<0, 2>(&heap).unwrap(),
                &([], [Some(Value::new_int(0)), Some(Value::new_int(1))])
            );
        });
        f::<_, 3>(&heap, |p| {
            assert!(&p.positional::<0>(&heap).is_err());
            assert!(&p.positional::<1>(&heap).is_err());
            assert!(&p.positional::<2>(&heap).is_err());
            assert!(p.optional::<0, 1>(&heap).is_err());
            assert!(p.optional::<1, 1>(&heap).is_err());
            assert!(p.optional::<0, 2>(&heap).is_err());
        });
    }

    #[test]
    fn test_parameter_no_named() {
        let heap = Heap::new();
        let mut p = Arguments::default();
        assert!(p.no_named_args().is_ok());

        // Test lots of forms of kwargs work properly
        p.0.kwargs = Some(Value::new_none());
        assert!(p.no_named_args().is_err());
        p.0.kwargs = Some(heap.alloc(Dict::default()));
        assert!(p.no_named_args().is_ok());
        let mut sm = SmallMap::new();
        sm.insert_hashed(heap.alloc_str("test").get_hashed(), Value::new_none());
        p.0.kwargs = Some(heap.alloc(Dict::new(coerce(sm))));
        assert!(p.no_named_args().is_err());

        // Test named arguments work properly
        p.0.kwargs = None;
        let named = [Value::new_none()];
        p.0.named = &named;
        let names = [(Symbol::new("test"), heap.alloc_str("test"))];
        p.0.names = ArgNames::new(&names);
        assert!(p.no_named_args().is_err());
    }

    #[test]
    fn test_parameter_iteration() {
        let mut p = ParametersSpec::<FrozenValue>::new("f".to_owned());
        p.required("a");
        p.optional("b");
        p.no_more_positional_args();
        p.optional("c");
        p.kwargs();
        let p = p.finish();

        let params: Vec<(&str, &ParameterKind<FrozenValue>)> = p.iter_params().collect();

        let expected: Vec<(&str, &ParameterKind<FrozenValue>)> = vec![
            ("a", &ParameterKind::Required),
            ("b", &ParameterKind::Optional),
            ("c", &ParameterKind::Optional),
            ("**kwargs", &ParameterKind::KWargs),
        ];

        assert_eq!(expected, params);
        assert_eq!(Some(2), p.no_args_param_index());

        let mut p = ParametersSpec::<FrozenValue>::new("f".to_owned());
        p.required("a");
        p.args();
        p.kwargs();
        let p = p.finish();

        let params: Vec<(&str, &ParameterKind<FrozenValue>)> = p.iter_params().collect();

        let expected: Vec<(&str, &ParameterKind<FrozenValue>)> = vec![
            ("a", &ParameterKind::Required),
            ("*args", &ParameterKind::Args),
            ("**kwargs", &ParameterKind::KWargs),
        ];

        assert_eq!(expected, params);
        assert_eq!(None, p.no_args_param_index());

        let mut p = ParametersSpec::<FrozenValue>::new("f".to_owned());
        p.args();
        p.optional("a");
        p.optional("b");
        let p = p.finish();

        let params: Vec<(&str, &ParameterKind<FrozenValue>)> = p.iter_params().collect();

        let expected: Vec<(&str, &ParameterKind<FrozenValue>)> = vec![
            ("*args", &ParameterKind::Args),
            ("a", &ParameterKind::Optional),
            ("b", &ParameterKind::Optional),
        ];

        assert_eq!(expected, params);
    }

    #[test]
    fn test_documentation() -> anyhow::Result<()> {
        // Make sure that documentation for some odder parameter specs works properly.
        let mut p = ParametersSpec::<FrozenValue>::new("f".to_owned());
        p.args();
        p.optional("a");
        p.optional("b");
        let p = p.finish();

        use crate::values::docs;
        let expected = vec![
            docs::Param::Args {
                name: "*args".to_owned(),
                docs: None,
                typ: None,
            },
            docs::Param::Arg {
                name: "a".to_owned(),
                docs: None,
                typ: Some(docs::Type {
                    raw_type: "int".to_owned(),
                }),
                default_value: Some("None".to_owned()),
            },
            docs::Param::Arg {
                name: "b".to_owned(),
                docs: DocString::from_docstring(docs::DocStringKind::Rust, "param b docs"),
                typ: None,
                default_value: Some("None".to_owned()),
            },
        ];
        let mut types = HashMap::new();
        types.insert(
            1,
            docs::Type {
                raw_type: "int".to_owned(),
            },
        );
        let mut docs = HashMap::new();
        docs.insert("a".to_owned(), None);
        docs.insert(
            "b".to_owned(),
            DocString::from_docstring(docs::DocStringKind::Rust, "param b docs"),
        );

        let params = p.documentation(types, docs);
        assert_eq!(expected, params);
        Ok(())
    }

    #[test]
    fn test_parameters_str() {
        fn test(sig: &str) {
            let a = Assert::new();
            let f = a
                .pass_module(&format!("def f({sig}): pass"))
                .get("f")
                .unwrap();
            assert_eq!(sig, &f.value().parameters_spec().unwrap().parameters_str());
        }

        test("");

        test("a, b, c, d, e, f, g, h, *args, **kwargs");

        test("*, a");
        test("x, *, a");

        test("*args, a");
        test("x, *args, a");

        test("**kwargs");
        test("a, **kwargs");
    }

    #[test]
    fn test_can_fill_with_args() {
        fn test(sig: &str, pos: usize, names: &[&str], expected: bool) {
            let a = Assert::new();
            let module = a.pass_module(&format!("def f({}): pass", sig));
            let f = module.get("f").unwrap().downcast::<FrozenDef>().unwrap();
            let parameters_spec = &f.parameters;
            assert_eq!(expected, parameters_spec.can_fill_with_args(pos, names));
        }

        test("", 0, &[], true);
        test("", 1, &[], false);
        test("", 0, &["a"], false);

        test("a", 1, &[], true);
        test("a", 0, &["a"], true);
        test("a", 1, &["a"], false);
        test("a", 0, &["x"], false);

        test("a, b = 1", 1, &[], true);
        test("a, b = 1", 2, &[], true);
        test("a, b = 1", 0, &["a"], true);
        test("a, b = 1", 0, &["b"], false);
        test("a, b = 1", 0, &["a", "b"], true);

        test("*, a", 0, &[], false);
        test("*, a", 1, &[], false);
        test("*, a", 0, &["a"], true);

        test("a, *args", 0, &[], false);
        test("a, *args", 1, &[], true);
        test("a, *args", 10, &[], true);

        test("*args, b", 0, &[], false);
        test("*args, b", 1, &[], false);
        test("*args, b", 0, &["b"], true);

        test("**kwargs", 0, &[], true);
        test("**kwargs", 0, &["a"], true);
        test("**kwargs", 1, &[], false);

        // No test for positional-only args because we can't create them in starlark.
    }
}
