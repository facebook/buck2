# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//apple:apple_error_handler_types.bzl", "AppleErrorCategories")
# @oss-disable: load("@prelude//apple/meta_only:apple_extra_error_categories.bzl", "APPLE_META_STDERR_ERROR_CATEGORIES") 
load("@prelude//utils:set.bzl", "set", "set_type")

_APPLE_STDERR_ERROR_CATEGORIES = [
    #codesigning issues
    AppleErrorCategories(string_match = "codesignprovisioningerror", categories = ["apple_code_sign_error", "code_sign_provisioning_error"]),
    AppleErrorCategories(string_match = "the timestamp service is not available", categories = ["apple_code_sign_error"]),
    #compilation issues
    AppleErrorCategories(string_match = "failed to emit precompiled module", categories = ["apple_compilation_failure", "apple_pcm_compilation_failure"]),
    AppleErrorCategories(string_match = "please rebuild precompiled header", categories = ["apple_compilation_failure", "apple_pcm_compilation_failure"]),
    AppleErrorCategories(string_match = "llvm-lipo", categories = ["apple_lipo_failure"]),
    AppleErrorCategories(string_match = ".swift:", categories = ["apple_compilation_failure", "apple_swift_compilation_failure"]),
    AppleErrorCategories(string_match = ".cpp:", categories = ["apple_compilation_failure", "apple_cpp_compilation_failure"]),
    AppleErrorCategories(string_match = ".cxx:", categories = ["apple_compilation_failure", "apple_cpp_compilation_failure"]),
    AppleErrorCategories(string_match = ".m:", categories = ["apple_compilation_failure", "apple_objc_compilation_failure"]),
    AppleErrorCategories(string_match = ".mm:", categories = ["apple_compilation_failure", "apple_objc_compilation_failure", "apple_cpp_compilation_failure", "apple_objcpp_compilation_failure"]),
    AppleErrorCategories(string_match = ".c:", categories = ["apple_compilation_failure", "apple_c_compilation_failure"]),
    AppleErrorCategories(string_match = ".modulemap:", categories = ["apple_compilation_failure", "apple_modulemap_compilation_failure"]),
    AppleErrorCategories(string_match = "missing required modules", categories = ["apple_compilation_failure", "apple_missing_required_modules_error"]),
    AppleErrorCategories(string_match = "has a minimum deployment target", categories = ["apple_compilation_failure", "apple_deployment_target_error"]),

    #toolchain / genrule issues
    AppleErrorCategories(string_match = "stack dump:", categories = ["apple_binary_execution_failure"]),
    AppleErrorCategories(string_match = "thread 'main' panicked", categories = ["apple_binary_execution_failure"]),
    AppleErrorCategories(string_match = "error while loading shared libraries", categories = ["apple_binary_execution_failure"]),
    AppleErrorCategories(string_match = "traceback (most recent call last)", categories = ["apple_python_execution_failure"]),
    AppleErrorCategories(string_match = "command not found", categories = ["apple_command_not_found_failure"]),
    AppleErrorCategories(string_match = "command timed out", categories = ["apple_timeout_failure"]),
    AppleErrorCategories(string_match = "no such file or directory", categories = ["apple_no_such_file_failure"]),

    #user errors
    AppleErrorCategories(string_match = "unknown target", categories = ["apple_unknown_buck_target_failure"]),

    #linker issues
    AppleErrorCategories(string_match = "linker command failed", categories = ["apple_linker_failure"]),
    AppleErrorCategories(string_match = "duplicate symbol", categories = ["apple_duplicate_symbol_failure"]),
    AppleErrorCategories(string_match = "undefined symbol", categories = ["apple_undefined_symbol_failure"]),
    AppleErrorCategories(string_match = "framework not found", categories = ["apple_framework_not_found_failure"]),

    #buck configuration issues
    AppleErrorCategories(string_match = "unknown cell alias", categories = ["apple_buck_configuration_failure", "apple_unknown_cell_alias_failure"]),
]

def _add_category_strings(lowercase_stderr: str, category_string_target: set_type, source: list[AppleErrorCategories]):
    for error_category in source:
        if error_category.string_match in lowercase_stderr:
            for category_string in error_category.categories:
                category_string_target.add(category_string)

def apple_build_error_handler(ctx: ActionErrorCtx) -> list[ActionSubError]:
    lowercase_stderr = ctx.stderr.lower()
    categories = set()
    _add_category_strings(lowercase_stderr, categories, _APPLE_STDERR_ERROR_CATEGORIES)
    # @oss-disable: _add_category_strings(lowercase_stderr, categories, APPLE_META_STDERR_ERROR_CATEGORIES) 

    return [ctx.new_sub_error(category = category_string) for category_string in sorted(categories.list())]
