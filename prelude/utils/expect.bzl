# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

"""Provides macros for stating program invariant expectations.

It is a good practice to enforce program invariants in code, so that all
assumptions are explicit and execution fails fast and with clear message in
case any one of them is violated.
"""

load(
    "@prelude//utils:type_defs.bzl",
    "is_bool",
    "is_collection",
    "is_dict",
    "is_list",
    "is_number",
    "is_string",
)

def expect(condition: typing.Any, message: str = "condition not expected", *format_args):
    """Fails if provided condition is not truthy.

    Args:
      condition: condition that is expected to be truthy
      message: an optional error message to display in case provided condition
        is not truthy
      format_args: optional arguments to format the error message with
    """
    if not condition:
        formatted_message = message.format(*format_args)
        fail(formatted_message)

def expect_non_none(val, msg: str = "unexpected none", *fmt_args, **fmt_kwargs):
    """
    Require the given value not be `None`.
    """
    if val == None:
        fail(msg.format(*fmt_args, **fmt_kwargs))
    return val

def expect_type(name: str, check: typing.Callable, desc: str, val: typing.Any):
    """Fails if check(val) if not truthy. name, desc are used for the error message.

    Usually you shouldn't need to directly use this, and prefer the expect_* family of functions
    defined in the same file.

    Args:
      name: the name of the attribute we're checking
      check: a function implementing an invariant check
      desc: a description of what we expected to see
      val: the value we're checking

    See the below functions, like expect_string, for usage examples.
    """
    expect(check(val), 'Buck target requires "{}" to be a {}', name, desc)

def expect_string(name, val):
    expect_type(name, is_string, "string", val)

def expect_string_starts_with(name, val, prefix):
    expect_type(name, is_string, "string", val)
    expect(val.startswith(prefix), 'Buck target requires "{}" to start with "{}', name, prefix)

def expect_number(name, val):
    expect_type(name, is_number, "number", val)

def expect_bool(name, val):
    expect_type(name, is_bool, "bool", val)

def expect_list(name, val):
    expect_type(name, is_list, "list", val)

def expect_list_of(sub_expect, name, val):
    """Check that all items in val satisfy sub_expect

    See expect_list_of_strings for an example.
    """
    expect_list(name, val)
    for i, val in enumerate(val):
        sub_expect("{}[{}] (value: {})".format(name, i, val), val)

def expect_list_of_strings(name, val):
    expect_list_of(expect_string, name, val)

def expect_dict(name, val):
    expect_type(name, is_dict, "dict", val)

def expect_dict_of(key_expect, value_expect, name, val):
    """Verify that all key-value pairs of val satisfy key_expect and value_expect respectively"""
    expect_dict(name, val)
    for key, val in val.items():
        key_expect("Key '{}' in {}".format(key, name), key)
        value_expect("{}[{}] (value={})".format(name, repr(key), val), val)

def expect_collection(name, val):
    expect_type(name, is_collection, "collection", val)

def expect_contains(name, val, options):
    """Verify that val is in options."""
    expect(val in options, "{name} (value: {val}) not in {options}".format(name = name, val = val, options = options))

def expect_contains_all(name, val, options):
    """Verify all items in val are in the options."""

    expect_collection(name, val)
    for index, val in enumerate(val):
        expect_contains("{name}[{index}]".format(name = name, index = index), val, options)

# You'll likely want to import this struct for convenience, instead of each method separately
expects = struct(
    type = expect_type,
    string = expect_string,
    string_starts_with = expect_string_starts_with,
    number = expect_number,
    bool = expect_bool,
    list = expect_list,
    list_of = expect_list_of,
    list_of_strings = expect_list_of_strings,
    dict = expect_dict,
    dict_of = expect_dict_of,
    collection = expect_collection,
    contains = expect_contains,
    contains_all = expect_contains_all,
)
