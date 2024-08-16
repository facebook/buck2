# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//apple:apple_error_handler_types.bzl", "AppleErrorCategories")
load("@prelude//utils:set.bzl", "set", "set_type")

_APPLE_STDERR_ERROR_CATEGORIES = [
    #codesigning issues
    AppleErrorCategories(string_match = "codesignprovisioningerror", categories = ["apple_code_sign_error", "code_sign_provisioning_error"]),

    #toolchain / genrule issues
    AppleErrorCategories(string_match = "stack dump:", categories = ["apple_binary_execution_failure"]),
    AppleErrorCategories(string_match = "thread 'main' panicked", categories = ["apple_binary_execution_failure"]),
    AppleErrorCategories(string_match = "error while loading shared libraries", categories = ["apple_binary_execution_failure"]),
    AppleErrorCategories(string_match = "traceback (most recent call last)", categories = ["apple_python_execution_failure"]),
    AppleErrorCategories(string_match = "command not found", categories = ["apple_command_not_found_failure"]),
    AppleErrorCategories(string_match = "command timed out", categories = ["apple_timeout_failure"]),
    AppleErrorCategories(string_match = "no such file or directory", categories = ["apple_no_such_file_failure"]),
]

def _add_category_strings(lowercase_stderr: str, category_strings: set_type):
    for error_category in _APPLE_STDERR_ERROR_CATEGORIES:
        if error_category.string_match in lowercase_stderr:
            for category_string in error_category.categories:
                category_strings.add(category_string)

def apple_build_error_handler(ctx: ActionErrorCtx) -> list[ActionSubError]:
    lowercase_stderr = ctx.stderr.lower()
    categories = set()
    _add_category_strings(lowercase_stderr, categories)

    return [ctx.new_sub_error(category = category_string) for category_string in sorted(categories.list())]
