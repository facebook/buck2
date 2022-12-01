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

//! Function parameters.

use std::cell::Cell;
use std::cmp;
use std::collections::HashMap;

use allocative::Allocative;
use gazebo::coerce::coerce;
use gazebo::coerce::Coerce;
use gazebo::dupe::Dupe;
use starlark_map::small_map::SmallMap;
use starlark_map::Hashed;

use crate as starlark;
use crate::collections::symbol_map::SymbolMap;
use crate::eval::runtime::arguments::ArgSymbol;
use crate::eval::runtime::arguments::ArgumentsImpl;
use crate::eval::runtime::arguments::FunctionError;
use crate::eval::runtime::arguments::ResolvedArgName;
use crate::eval::Arguments;
use crate::eval::Evaluator;
use crate::hint::unlikely;
use crate::values::dict::Dict;
use crate::values::docs;
use crate::values::docs::DocString;
use crate::values::Heap;
use crate::values::StringValue;
use crate::values::UnpackValue;
use crate::values::Value;
use crate::values::ValueLike;

#[derive(Debug, Copy, Clone, Dupe, Coerce, PartialEq, Trace, Freeze, Allocative)]
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

#[derive(Debug, Copy, Clone, Dupe, PartialEq, Eq, PartialOrd, Ord)]
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
    names: SymbolMap<u32>,
    positional: usize,

    /// Has the no_args been passed
    current_style: CurrentParameterStyle,

    args: Option<usize>,
    kwargs: Option<usize>,
}

/// Define a list of parameters. This code assumes that all names are distinct and that
/// `*args`/`**kwargs` occur in well-formed locations.
// V = Value, or FrozenValue
#[derive(Debug, Clone, Trace, Freeze, Allocative)]
#[repr(C)]
pub struct ParametersSpec<V> {
    /// Only used in error messages
    function_name: String,

    /// Parameters in the order they occur.
    param_kinds: Box<[ParameterKind<V>]>,
    /// Parameter names in the order they occur.
    param_names: Box<[String]>,
    /// Mapping from name to index where the argument lives.
    #[freeze(identity)]
    pub(crate) names: SymbolMap<u32>,

    /// Number of arguments that can be filled positionally.
    /// Excludes *args/**kwargs, keyword arguments after *args
    positional: u32,

    /// The index at which *args should go
    args: Option<u32>,
    /// The index at which **kwargs should go
    kwargs: Option<u32>,
}

// Can't derive this since we don't want ParameterKind to be public
unsafe impl<From: Coerce<To>, To> Coerce<ParametersSpec<To>> for ParametersSpec<From> {}

impl<V: Copy> ParametersSpecBuilder<V> {
    fn add(&mut self, name: &str, val: ParameterKind<V>) {
        assert!(!matches!(val, ParameterKind::Args | ParameterKind::KWargs));

        // Regular arguments cannot follow `**kwargs`, but can follow `*args`.
        assert!(self.current_style < CurrentParameterStyle::NoMore);
        assert!(self.kwargs.is_none());

        let i = self.params.len();
        self.params.push((name.to_owned(), val));
        if self.current_style != CurrentParameterStyle::PosOnly {
            let old = self.names.insert(name, i.try_into().unwrap());
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
            param_kinds: params.iter().map(|p| p.1).collect(),
            param_names: params.into_iter().map(|p| p.0).collect(),
            names,
            positional: positional.try_into().unwrap(),
            args: args.map(|args| args.try_into().unwrap()),
            kwargs: kwargs.map(|kwargs| kwargs.try_into().unwrap()),
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
        for (i, typ) in self.iter_params().enumerate() {
            if !collector.is_empty() {
                collector.push_str(", ");
            }

            // TODO: also print `/` for positional-only parameters.

            if i == (self.positional as usize)
                && !emitted_star
                && !matches!(typ.1, ParameterKind::Args | ParameterKind::KWargs)
            {
                collector.push_str("*, ");
                emitted_star = true;
            }

            match typ.1 {
                ParameterKind::Args | ParameterKind::KWargs => {
                    // For `*args` or `**kwargs` param name includes the `*` or `**`.
                    collector.push_str(typ.0);
                    emitted_star = true;
                }
                ParameterKind::Required => {
                    collector.push_str(typ.0);
                }
                ParameterKind::Optional | ParameterKind::Defaulted(_) => {
                    collector.push_str(typ.0);
                    collector.push_str(" = ...");
                }
            }
        }
        collector
    }

    /// Get the index where a user would have supplied "*" as a parameter.
    pub(crate) fn no_args_param_index(&self) -> Option<usize> {
        if (self.positional as usize) < self.param_kinds.len() {
            match self.param_kinds.get(self.positional as usize) {
                Some(ParameterKind::Args) | Some(ParameterKind::KWargs) => None,
                _ => Some(self.positional as usize),
            }
        } else {
            None
        }
    }

    /// Iterate over the parameters
    ///
    /// Returns an iterator over (parameter index, name, kind)
    pub(crate) fn iter_params(&self) -> impl Iterator<Item = (&str, &ParameterKind<V>)> {
        assert_eq!(self.param_names.len(), self.param_kinds.len());
        self.param_names
            .iter()
            .map(|name| name.as_str())
            .zip(&*self.param_kinds)
    }

    pub(crate) fn resolve_name(&self, name: Hashed<&str>) -> ResolvedArgName {
        let hash = name.hash();
        let param_index = self.names.get_hashed_str(name).map(|index| *index as u32);
        ResolvedArgName { hash, param_index }
    }

    pub(crate) fn has_args_or_kwargs(&self) -> bool {
        self.args.is_some() || self.kwargs.is_some()
    }
}

impl<'v, V: ValueLike<'v>> ParametersSpec<V> {
    /// Number of function parameters.
    pub fn len(&self) -> usize {
        self.param_kinds.len()
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
        if args.pos().len() == (self.positional as usize)
            && args.pos().len() == self.param_kinds.len()
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
            kwargs: Option<SmallMap<StringValue<'v>, Value<'v>>>,
        }

        impl<'v> LazyKwargs<'v> {
            // Return true if the value is a duplicate
            #[inline(always)]
            fn insert(&mut self, key: Hashed<StringValue<'v>>, val: Value<'v>) -> bool {
                match &mut self.kwargs {
                    None => {
                        let mut mp = SmallMap::with_capacity(12);
                        mp.insert_hashed_unique_unchecked(key, val);
                        self.kwargs = Some(mp);
                        false
                    }
                    Some(mp) => mp.insert_hashed(key, val).is_some(),
                }
            }

            fn alloc(self, heap: &'v Heap) -> Value<'v> {
                let kwargs = match self.kwargs {
                    Some(kwargs) => Dict::new(coerce(kwargs)),
                    None => Dict::default(),
                };
                heap.alloc(kwargs)
            }
        }

        let len = self.param_kinds.len();
        // We might do unchecked stuff later on, so make sure we have as many slots as we expect
        assert!(slots.len() >= len);

        let mut star_args = Vec::new();
        let mut kwargs = LazyKwargs::default();
        let mut next_position = 0;

        // First deal with positional parameters
        if args.pos().len() <= (self.positional as usize) {
            // fast path for when we don't need to bounce down to filling in args
            for (v, s) in args.pos().iter().zip(slots.iter()) {
                s.set(Some(*v));
            }
            next_position = args.pos().len();
        } else {
            for v in args.pos() {
                if next_position < (self.positional as usize) {
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
                        if next_position < (self.positional as usize) {
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
            return Err(FunctionError::RepeatedArg {
                name: self.param_names[lowest_name].clone(),
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
                                        let this_slot = &slots[*i as usize];
                                        let repeat = this_slot.get().is_some();
                                        this_slot.set(Some(v));
                                        repeat
                                    }
                                };
                                if unlikely(repeat) {
                                    return Err(FunctionError::RepeatedArg {
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
        let kinds = &*self.param_kinds;
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
            match def {
                ParameterKind::Required => {
                    return Err(FunctionError::MissingParameter {
                        name: self.param_names[index].clone(),
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
            slots[args_pos as usize].set(Some(heap.alloc_tuple(&star_args)));
        } else if unlikely(!star_args.is_empty()) {
            return Err(FunctionError::ExtraPositionalArg {
                count: star_args.len(),
                function: self.signature(),
            }
            .into());
        }

        if let Some(kwargs_pos) = self.kwargs {
            slots[kwargs_pos as usize].set(Some(kwargs.alloc(heap)));
        } else if let Some(kwargs) = kwargs.kwargs {
            return Err(FunctionError::ExtraNamedArg {
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
        let mut filled = vec![false; self.param_kinds.len()];
        for p in 0..pos {
            if p < (self.positional as usize) {
                filled[p] = true;
            } else if self.args.is_some() {
                // Filled into `*args`.
            } else {
                return false;
            }
        }
        if pos > (self.positional as usize) && self.args.is_none() {
            return false;
        }
        for name in names {
            match self.names.get_str(name) {
                Some(i) => {
                    if filled[*i as usize] {
                        // Duplicate argument.
                        return false;
                    }
                    filled[*i as usize] = true;
                }
                None => {
                    if self.kwargs.is_none() {
                        return false;
                    }
                }
            }
        }
        for (filled, p) in filled.iter().zip(self.param_kinds.iter()) {
            if *filled {
                continue;
            }
            match p {
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

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::assert::Assert;
    use crate::eval::compiler::def::FrozenDef;
    use crate::eval::runtime::params::ParameterKind;
    use crate::eval::ParametersSpec;
    use crate::values::docs::DocString;
    use crate::values::FrozenValue;

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
