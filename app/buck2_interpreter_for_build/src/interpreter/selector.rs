/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fmt;
use std::fmt::Display;

use allocative::Allocative;
use buck2_error::BuckErrorContext;
use starlark::any::ProvidesStaticType;
use starlark::coerce::Coerce;
use starlark::collections::SmallMap;
use starlark::environment::GlobalsBuilder;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::eval::Evaluator;
use starlark::starlark_complex_value;
use starlark::starlark_module;
use starlark::values::Freeze;
use starlark::values::FreezeResult;
use starlark::values::Freezer;
use starlark::values::FrozenStringValue;
use starlark::values::FrozenValue;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::StringValue;
use starlark::values::Trace;
use starlark::values::Tracer;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::ValueLifetimeless;
use starlark::values::ValueLike;
use starlark::values::ValueOf;
use starlark::values::ValueOfUncheckedGeneric;
use starlark::values::dict::Dict;
use starlark::values::dict::DictRef;
use starlark::values::dict::DictType;
use starlark::values::none::NoneOr;
use starlark::values::starlark_value;
use starlark::values::starlark_value_as_type::StarlarkValueAsType;

/// Representation of `select()` in Starlark.
#[derive(Debug, ProvidesStaticType, NoSerialize, Allocative)] // TODO selector should probably support serializing
#[repr(C)]
pub enum StarlarkSelectorGen<V: ValueLifetimeless> {
    /// Simplest form, backed by dictionary representation
    /// wrapped into `select` function call.
    Primary(ValueOfUncheckedGeneric<V, DictType<FrozenStringValue, FrozenValue>>),
    Sum(V, V),
}

impl<V: ValueLifetimeless> Display for StarlarkSelectorGen<V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            StarlarkSelectorGen::Primary(v) => {
                f.write_str("select(")?;
                Display::fmt(v, f)?;
                f.write_str(")")
            }
            StarlarkSelectorGen::Sum(l, r) => {
                Display::fmt(l, f)?;
                f.write_str(" + ")?;
                Display::fmt(r, f)
            }
        }
    }
}

unsafe impl<From: Coerce<To> + ValueLifetimeless, To: ValueLifetimeless>
    Coerce<StarlarkSelectorGen<To>> for StarlarkSelectorGen<From>
{
}

starlark_complex_value!(pub StarlarkSelector);

impl<'v> StarlarkSelector<'v> {
    pub fn new(d: ValueOf<'v, DictType<StringValue<'v>, Value<'v>>>) -> Self {
        StarlarkSelector::Primary(d.as_unchecked().cast())
    }

    fn sum(left: Value<'v>, right: Value<'v>, heap: Heap<'v>) -> Value<'v> {
        heap.alloc(StarlarkSelector::Sum(left, right))
    }

    pub fn from_concat<I>(iter: I, heap: Heap<'v>) -> buck2_error::Result<Value<'v>>
    where
        I: IntoIterator<Item = Value<'v>>,
    {
        fn values_to_selector<'v, I>(
            selector: Option<StarlarkSelector<'v>>,
            values: &mut I,
            heap: Heap<'v>,
        ) -> buck2_error::Result<NoneOr<StarlarkSelector<'v>>>
        where
            I: Iterator<Item = Value<'v>>,
        {
            match (selector, values.next()) {
                (None, None) => Ok(NoneOr::None),
                (None, Some(v)) => {
                    if let Some(next_v) = values.next() {
                        let head = StarlarkSelector::Sum(v, next_v);
                        values_to_selector(Some(head), values, heap)
                    } else {
                        let v = ValueOf::unpack_value_err(v.to_value())
                            .internal_error("validated at construction")?;
                        Ok(NoneOr::Other(StarlarkSelector::new(v)))
                    }
                }
                (Some(s), None) => Ok(NoneOr::Other(s)),
                (Some(s), Some(v)) => {
                    let head = Some(StarlarkSelector::Sum(heap.alloc(s), v));
                    values_to_selector(head, values, heap)
                }
            }
        }
        let selector = values_to_selector(None, &mut iter.into_iter(), heap)?;
        Ok(heap.alloc(selector))
    }

    fn select_map<'a, const RECURSE: bool>(
        eval: &mut Evaluator<'a, '_, '_>,
        val: Value<'a>,
        func: Value<'a>,
    ) -> starlark::Result<Value<'a>> {
        // The recursion depth of this function is determined by the depth of
        // the input `select()` objects, which could be unbounded. Detect such
        // cases and return an error nicely rather than panic'ing.
        buck2_util::threads::check_stack_overflow()?;

        fn invoke<'v>(
            eval: &mut Evaluator<'v, '_, '_>,
            val: Value<'v>,
            func: Value<'v>,
        ) -> starlark::Result<Value<'v>> {
            eval.eval_function(func, &[val], &[])
        }

        if let Some(selector) = StarlarkSelector::from_value(val) {
            match *selector {
                StarlarkSelectorGen::Primary(selector) => {
                    let selector = DictRef::from_value(selector.get()).unwrap();
                    let mut mapped = SmallMap::with_capacity(selector.len());
                    for (k, v) in selector.iter_hashed() {
                        mapped.insert_hashed(
                            k,
                            if RECURSE {
                                Self::select_map::<RECURSE>(eval, v, func)
                            } else {
                                invoke(eval, v, func)
                            }?,
                        );
                    }
                    Ok(eval.heap().alloc(StarlarkSelector::new(
                        ValueOf::unpack_value_err(eval.heap().alloc(Dict::new(mapped)))
                            .internal_error("validated at construction")?,
                    )))
                }
                StarlarkSelectorGen::Sum(left, right) => {
                    Ok(eval.heap().alloc(StarlarkSelectorGen::Sum(
                        Self::select_map::<RECURSE>(eval, left, func)?,
                        Self::select_map::<RECURSE>(eval, right, func)?,
                    )))
                }
            }
        } else {
            invoke(eval, val, func)
        }
    }

    fn select_test<'a>(
        val: Value<'a>,
        eval: &mut Evaluator<'a, '_, '_>,
        func: Value<'a>,
    ) -> starlark::Result<bool> {
        fn invoke<'v>(
            eval: &mut Evaluator<'v, '_, '_>,
            func: Value<'v>,
            val: Value<'v>,
        ) -> starlark::Result<bool> {
            eval.eval_function(func, &[val], &[])?
                .unpack_bool()
                .ok_or_else(|| {
                    buck2_error::buck2_error!(
                        buck2_error::ErrorTag::Input,
                        "Expected testing function to have a boolean return type"
                    )
                    .into()
                })
        }

        if let Some(selector) = StarlarkSelector::from_value(val) {
            match *selector {
                StarlarkSelectorGen::Primary(selector) => {
                    let selector = DictRef::from_value(selector.get()).unwrap();
                    for v in selector.values() {
                        let result = invoke(eval, func, v)?;
                        if result {
                            return Ok(true);
                        }
                    }
                    Ok(false)
                }
                StarlarkSelectorGen::Sum(left, right) => {
                    Ok(Self::select_test(left, eval, func)?
                        || Self::select_test(right, eval, func)?)
                }
            }
        } else {
            invoke(eval, func, val)
        }
    }
}

trait StarlarkSelectorBase<'v> {
    type Item: ValueLike<'v>;
}

impl<'v> StarlarkSelectorBase<'v> for StarlarkSelector<'v> {
    type Item = Value<'v>;
}

unsafe impl<'v> Trace<'v> for StarlarkSelector<'v> {
    fn trace(&mut self, tracer: &Tracer<'v>) {
        match self {
            Self::Primary(a) => a.trace(tracer),
            Self::Sum(a, b) => {
                tracer.trace(a);
                tracer.trace(b);
            }
        }
    }
}

impl<'v> Freeze for StarlarkSelector<'v> {
    type Frozen = FrozenStarlarkSelector;
    fn freeze(self, freezer: &Freezer) -> FreezeResult<Self::Frozen> {
        Ok(match self {
            StarlarkSelector::Primary(v) => FrozenStarlarkSelector::Primary(v.freeze(freezer)?),
            StarlarkSelector::Sum(l, r) => {
                FrozenStarlarkSelector::Sum(l.freeze(freezer)?, r.freeze(freezer)?)
            }
        })
    }
}

impl StarlarkSelectorBase<'_> for FrozenStarlarkSelector {
    type Item = FrozenValue;
}

#[starlark_value(type = "Select")]
impl<'v, V: ValueLike<'v>> StarlarkValue<'v> for StarlarkSelectorGen<V>
where
    Self: ProvidesStaticType<'v> + StarlarkSelectorBase<'v, Item = V>,
{
    fn to_bool(&self) -> bool {
        true
    }

    fn radd(&self, left: Value<'v>, heap: Heap<'v>) -> Option<starlark::Result<Value<'v>>> {
        let right = heap.alloc(match self {
            StarlarkSelectorGen::Primary(x) => StarlarkSelectorGen::Primary(x.to_value()),
            StarlarkSelectorGen::Sum(x, y) => StarlarkSelectorGen::Sum(x.to_value(), y.to_value()),
        });
        Some(Ok(StarlarkSelector::sum(left, right, heap)))
    }

    fn add(&self, other: Value<'v>, heap: Heap<'v>) -> Option<starlark::Result<Value<'v>>> {
        match self {
            Self::Primary(v) => match ValueOf::unpack_value_err(v.get().to_value()) {
                Ok(v) => {
                    let this = heap.alloc(StarlarkSelector::new(v));
                    Some(Ok(StarlarkSelector::sum(this, other, heap)))
                }
                Err(e) => Some(Err(e.into())),
            },
            Self::Sum(l, r) => {
                let this = StarlarkSelector::sum(l.to_value(), r.to_value(), heap);
                Some(Ok(StarlarkSelector::sum(this, other, heap)))
            }
        }
    }

    // used to provide the type documentation here
    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(selector_methods)
    }
}

#[starlark_module]
pub fn register_select(globals: &mut GlobalsBuilder) {
    const Select: StarlarkValueAsType<StarlarkSelector> = StarlarkValueAsType::new();

    fn select<'v>(
        #[starlark(require = pos)] d: ValueOf<'v, DictType<StringValue<'v>, Value<'v>>>,
    ) -> starlark::Result<StarlarkSelector<'v>> {
        Ok(StarlarkSelector::new(d))
    }

    /// Maps a selector.
    ///
    /// For selector additions values:
    ///
    ///   Each value on each side of an addition is always passed to select_map
    ///   recursively.
    ///
    /// For selector maps values, for each value within a selector:
    ///
    ///   If recurse is False, the value will be passed to the mapping function.
    ///   This may be a selector, or not.
    ///
    ///   If recurse is True, the value will be passed to select_map()
    ///   recursively
    ///
    /// For non-selector value, the value will be passed to the mapping
    /// function.
    ///
    /// Generally the returned selector will have the same key/depth structure
    /// as the input selector. In the recurse=False case a mapping function that
    /// turns a selector into a non-selector may prune the structure. In the
    /// recurse=True case, the mapping functino cannot change the existing
    /// structure. In either case the mapping function can return a selector for
    /// a non-selector input, causing the selector structure to get deeper.
    ///
    /// Ex:
    /// ```python
    /// def increment_items(a):
    ///     return [v + 1 for v in a]
    ///
    /// select_map([1, 2] + select({"c": [2]}), increment_items, False) == [2, 3] + select({"c": [3]})
    ///
    /// select_apply([1, 2] + select({"c": select({"d": [2]}}), increment_items, True) == [2, 3] + select({"c": select("d": [3]})})
    /// ```
    fn select_map<'v>(
        #[starlark(require = pos)] d: Value<'v>,
        #[starlark(require = pos)] func: Value<'v>,
        // TODO(T245559941): change the default to recurse=true
        #[starlark(require = named, default = false)] recurse: bool,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> starlark::Result<Value<'v>> {
        if recurse {
            StarlarkSelector::select_map::<true>(eval, d, func)
        } else {
            StarlarkSelector::select_map::<false>(eval, d, func)
        }
    }

    /// Test values in the select expression using the given function.
    ///
    /// Returns True, if any value in the select passes, else False.
    ///
    /// Ex:
    /// ```python
    /// select_test([1] + select({"c": [1]}), lambda a: len(a) > 1) == False
    /// select_test([1, 2] + select({"c": [1]}), lambda a: len(a) > 1) == True
    /// select_test([1] + select({"c": [1, 2]}), lambda a: len(a) > 1) == True
    /// ```
    fn select_test<'v>(
        #[starlark(require = pos)] d: Value<'v>,
        #[starlark(require = pos)] func: Value<'v>,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> starlark::Result<bool> {
        StarlarkSelector::select_test(d, eval, func)
    }
}
#[starlark_module]
pub fn register_select_internal(globals: &mut GlobalsBuilder) {
    /// Tests that two selects are equal to each other. For testing use only.
    /// We simply compare their string representations.
    fn select_equal<'v>(
        #[starlark(require = pos)] left: Value<'v>,
        #[starlark(require = pos)] right: Value<'v>,
    ) -> starlark::Result<bool> {
        Ok(left.to_repr() == right.to_repr())
    }
}

/// The `Select` type represents a conditional attribute value in Buck2.
///
/// `Select` objects are created using the `select()` function and enable build rules to have
/// different attribute values based on the target's configuration.
/// This is essential for cross-platform builds and conditional compilation.
///
/// # Resolution Timing
///
/// `Select` objects are resolved during Buck2's **configuration phase**, which happens after
/// BUCK file evaluation but before rule implementation. This means:
/// - Starlark code in BUCK files and macro rules cannot see resolved values
/// - Use `select_map()` or `select_test()` for macro-level operations
///
/// # Operations
///
/// `Select` supports the addition operator (`+`) for combining conditional values:
///
/// ```python
/// # Combine multiple selects
/// deps = select({
///     "//config:linux": ["//lib:linux"],
///     "DEFAULT": [],
/// }) + select({
///     "//config:debug": ["//debug:tools"],
///     "DEFAULT": [],
/// })
///
/// # Add to regular values
/// flags = ["-Wall"] + select({
///     "//config:optimize": ["-O3"],
///     "DEFAULT": ["-O0"],
/// })
/// ```
///
/// # Resolution Algorithm
///
/// When Buck2 resolves a `Select`:
///
/// 1. Evaluates each key of select options
/// 2. Collects all keys that match the current configuration
/// 3. Applies **refinement** to choose the most specific match:
///    - If zero matches: Use DEFAULT or error if no DEFAULT
///    - If one match: Use that value
///    - If multiple matches: Select the one that is most specific
/// 4. If ties exist (equally specific):
///    - Same values: OK, use that value
///    - Different values: Error
///
/// # Select Refinement Example
///
/// ```python
/// config_setting(
///     name = "linux",
///     constraint_values = ["//constraints:linux"],  # 1 constraint
/// )
///
/// config_setting(
///     name = "linux-arm64",
///     constraint_values = [
///         "//constraints:linux",
///         "//constraints:arm64",  # 2 constraints - more specific
///     ],
/// )
///
/// my_rule(
///     srcs = select({
///         ":linux": ["generic_linux.cpp"],
///         ":linux-arm64": ["optimized_arm64.cpp"],  # This wins when both match
///     }),
/// )
/// ```
///
/// **Key point**: Order in the dictionary doesn't matter; specificity always wins.
///
/// # Configuration System
///
/// ## Constraints
///
/// - **constraint_setting**: Defines a configuration dimension
///   ```python
///   constraint_setting(name = "os")
///   constraint_setting(name = "cpu")
///   ```
///
/// - **constraint_value**: Specific value for a dimension
///   ```python
///   constraint_value(name = "linux", constraint_setting = ":os")
///   constraint_value(name = "arm64", constraint_setting = ":cpu")
///   ```
///
/// - **platform**: Collection of constraint values
///   ```python
///   platform(
///       name = "linux-arm64",
///       constraint_values = [":linux", ":arm64"],
///   )
///   ```
///
/// ## config_setting
///
/// Matches specific configuration conditions:
///
/// ```python
/// # Match constraints
/// config_setting(
///     name = "linux-arm",
///     constraint_values = [
///         "//constraints:linux",
///         "//constraints:arm",
///     ],
/// )
///
/// # Match buckconfig values
/// config_setting(
///     name = "fastmode",
///     values = {
///         "build.fastmode": "true",
///     },
/// )
///
/// # Combine both
/// config_setting(
///     name = "linux-fastmode",
///     constraint_values = ["//constraints:linux"],
///     values = {"build.fastmode": "true"},
/// )
/// ```
///
/// # Platform Selection
///
/// Buck2 determines the target platform through:
///
/// 1. `--target-platforms` command-line flag (highest priority)
/// 2. `default_target_platform` attribute on the target
/// 3. Cell's default platform from buckconfig
///
/// Example:
/// ```bash
/// buck2 build //app:main --target-platforms //platforms:linux-x86_64
/// ```
///
/// # Target Compatibility
///
/// Buck2 provides two attributes for platform compatibility:
///
/// ## target_compatible_with
///
/// **ALL semantics**: Target is compatible only if **all** listed constraints match:
///
/// ```python
/// cxx_library(
///     name = "windows_dev_only",
///     target_compatible_with = [
///         "//constraints:windows",  # Must be Windows AND
///         "//constraints:dev",      # Must be dev mode
///     ],
/// )
/// ```
///
/// ## compatible_with
///
/// **ANY semantics**: Target is compatible if **any** listed constraint matches:
///
/// ```python
/// cxx_library(
///     name = "unix_compatible",
///     compatible_with = [
///         "//constraints:linux",    # Linux OR
///         "//constraints:macos",    # macOS (either works)
///     ],
/// )
/// ```
///
/// # Common Patterns
///
/// ## Platform-Specific Dependencies
///
/// ```python
/// cxx_library(
///     name = "mylib",
///     srcs = ["common.cpp"],
///     deps = [
///         "//common:base",
///     ] + select({
///         "//config:linux": ["//platform:linux_support"],
///         "//config:macos": ["//platform:macos_support"],
///         "//config:windows": ["//platform:windows_support"],
///     }),
/// )
/// ```
///
/// ## Build Mode Flags
///
/// ```python
/// cxx_binary(
///     name = "app",
///     compiler_flags = select({
///         "//config:debug": ["-g", "-O0"],
///         "//config:release": ["-O3", "-DNDEBUG"],
///         "DEFAULT": ["-O2"],
///     }),
/// )
/// ```
///
/// ## Architecture-Specific Optimizations
///
/// ```python
/// cxx_library(
///     name = "simd",
///     compiler_flags = select({
///         "//config:x86_64": ["-msse4.2"],
///         "//config:arm64": ["-march=armv8-a"],
///         "DEFAULT": [],
///     }),
/// )
/// ```
///
/// # Working with Selects in Macros
///
/// Since `select()` values aren't resolved during BUCK evaluation, use these functions:
///
/// - **select_map(value, func)**: Transform all possible values
///   ```python
///   # Add suffix to all possible values
///   new_deps = select_map(
///       deps_select,
///       lambda items: [item + "_wrapper" for item in items]
///   )
///   ```
///
/// - **select_test(value, func)**: Test if any value matches predicate
///   ```python
///   # Check if any branch has more than 2 items
///   has_many = select_test(deps_select, lambda items: len(items) > 2)
///   ```
///
/// # Error Messages
///
/// Common errors:
///
/// - **No match without DEFAULT**: "None of N conditions matched configuration ... and no default was set"
/// - **Ambiguous match**: "Both select keys `X` and `Y` match the configuration, but neither is more specific and they have different values"
/// - **Invalid usage**: Cannot use `select()` inside config_setting or other configuration rules
///
/// # See Also
///
/// - `select()` - Creates a Select object
/// - `select_map()` - Applies a function to all possible values
/// - `select_test()` - Tests if any value passes a predicate
/// - `target_compatible_with` - Platform compatibility filtering
/// - Buck2 docs: "Configurations By Example"
#[starlark_module]
fn selector_methods(builder: &mut MethodsBuilder) {}
