# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//apple:apple_error_handler_types.bzl", "AppleErrorCategory")
# @oss-disable[end= ]: load("@prelude//apple/meta_only:apple_extra_error_categories.bzl", "APPLE_META_STDERR_ERROR_CATEGORIES")

_APPLE_STDERR_ERROR_CATEGORIES = [

    # STOP! @oss-disable
    # If you work at Meta, you probably want to include things in APPLE_META_STDERR_ERROR_CATEGORIES   @oss-disable
    # so you can include a link to an internal resource (wiki, task, etc)                              @oss-disable
    # I would only add additional categories here if you think someone in open-source would benefit    @oss-disable

    #codesigning issues
    AppleErrorCategory(string_match = "codesignprovisioningerror", category = "apple_code_sign_error"),
    AppleErrorCategory(string_match = "codesignprovisioningerror", category = "code_sign_provisioning_error"),
    AppleErrorCategory(string_match = "the timestamp service is not available", category = "apple_code_sign_error"),

    #compilation issues
    AppleErrorCategory(string_match = "failed to emit precompiled module", category = "apple_pcm_compilation_failure"),
    AppleErrorCategory(string_match = "please rebuild precompiled header", category = "apple_pcm_compilation_failure"),
    AppleErrorCategory(string_match = "llvm-lipo", category = "apple_lipo_failure"),
    AppleErrorCategory(string_match = ".modulemap:", category = "apple_modulemap_compilation_failure"),
    AppleErrorCategory(string_match = "missing required modules", category = "apple_missing_required_modules_error"),
    AppleErrorCategory(string_match = "has a minimum deployment target", category = "apple_deployment_target_error"),

    #toolchain / genrule issues
    AppleErrorCategory(string_match = "stack dump:", category = "apple_binary_execution_failure"),
    AppleErrorCategory(string_match = "thread 'main' panicked", category = "apple_binary_execution_failure"),
    AppleErrorCategory(string_match = "error while loading shared libraries", category = "apple_binary_execution_failure"),
    AppleErrorCategory(string_match = "traceback (most recent call last)", category = "apple_python_execution_failure"),
    AppleErrorCategory(string_match = "command not found", category = "apple_command_not_found_failure"),
    AppleErrorCategory(string_match = "command timed out", category = "apple_timeout_failure"),
    AppleErrorCategory(string_match = "no such file or directory", category = "apple_no_such_file_failure"),

    #user errors
    AppleErrorCategory(string_match = "unknown target", category = "apple_unknown_buck_target_failure"),

    #linker issues
    AppleErrorCategory(string_match = "linker command failed", category = "apple_linker_failure"),
    AppleErrorCategory(string_match = "duplicate symbol", category = "apple_duplicate_symbol_failure"),
    AppleErrorCategory(string_match = "undefined symbol", category = "apple_undefined_symbol_failure"),
    AppleErrorCategory(string_match = "framework not found", category = "apple_framework_not_found_failure"),

    #buck configuration issues
    AppleErrorCategory(string_match = "unknown cell alias", category = "apple_unknown_cell_alias_failure"),
]

def _add_category_strings(ctx: ActionErrorCtx, lowercase_stderr: str, errors: list[ActionSubError], source: list[AppleErrorCategory]):
    for error_category in source:
        if error_category.string_match in lowercase_stderr:
            errors.append(ctx.new_sub_error(category = error_category.category, message = error_category.message))

def apple_build_error_handler(ctx: ActionErrorCtx) -> list[ActionSubError]:
    lowercase_stderr = ctx.stderr.lower()

    errors = []
    _add_category_strings(ctx, lowercase_stderr, errors, _APPLE_STDERR_ERROR_CATEGORIES)
    # @oss-disable[end= ]: _add_category_strings(ctx, lowercase_stderr, errors, APPLE_META_STDERR_ERROR_CATEGORIES)

    return errors
