# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

CxxGenericErrorType = record(
    string_match = [str, None],
    category_suffix = str,
)

def make_error_type(string_match = None, category_suffix = str) -> CxxGenericErrorType:
    return CxxGenericErrorType(
        string_match = string_match,
        category_suffix = category_suffix,
    )

def cxx_generic_error_handler(ctx: ActionErrorCtx) -> list[ActionSubError]:
    categories = []

    for error_type in CXX_GENERIC_ERROR_TYPES:
        if error_type.string_match in ctx.stderr:
            categories.append(ctx.new_sub_error(
                category = "cxx_{}".format(error_type.category_suffix),
            ))

    return categories

# PLEASE READ BEFORE ADDING NEW ERROR TYPES!
#
# Error types added here should be generic across all cxx toolchains, toolchain/platform specific errors
# should be defined in a separate toolchain and provided accordingly, these generic errors will automatically
# be added to all types. If you wish to add messages, it must be applicable to ALL scenarios, including OSS.
CXX_GENERIC_ERROR_TYPES = [
    # linker errors
    make_error_type(
        string_match = "error: duplicate symbol",
        category_suffix = "linker_duplicate_symbol",
    ),
    make_error_type(
        string_match = "error: undefined symbol",
        category_suffix = "linker_undefined_symbol",
    ),
    make_error_type(
        string_match = "error: unable to find library",
        category_suffix = "linker_missing_library",
    ),
    make_error_type(
        string_match = "cannot be used against local symbol",
        category_suffix = "linker_relocation",
    ),

    # compilation errors
    make_error_type(
        string_match = "file not found",
        category_suffix = "file_not_found",
    ),
    make_error_type(
        string_match = "extra-semi",
        category_suffix = "extra_semicolon",
    ),
    make_error_type(
        string_match = "cannot open file",
        category_suffix = "cannot_open_file",
    ),
    make_error_type(
        string_match = "too few arguments to function call",
        category_suffix = "too_few_arguments",
    ),
    make_error_type(
        string_match = "use of undeclared identifier",
        category_suffix = "undeclared_identifier",
    ),
    make_error_type(
        string_match = "macro redefined",
        category_suffix = "macro_redefined",
    ),
    make_error_type(
        string_match = "no type named",
        category_suffix = "missing_type",
    ),
    make_error_type(
        string_match = "no matching function for call",
        category_suffix = "missing_function",
    ),
    make_error_type(
        string_match = "a type specifier is required for all declarations",
        category_suffix = "type_specifier_required",
    ),
    make_error_type(
        string_match = "definition of implicit copy constructor for",
        category_suffix = "definition_deprecated",
    ),
    make_error_type(
        string_match = "Permission denied",
        category_suffix = "permission_denied",
    ),
    make_error_type(
        string_match = "incomplete type",
        category_suffix = "incomplete_type",
    ),
    make_error_type(
        string_match = "C2440",
        category_suffix = "type_conversion_failed",
    ),
    make_error_type(
        string_match = "cannot initialize a member subobject of type",
        category_suffix = "type_mismatch",
    ),
    make_error_type(
        string_match = "no matching member function",
        category_suffix = "missing_member_function",
    ),
    make_error_type(
        string_match = "unused variable",
        category_suffix = "unused_variable",
    ),
    make_error_type(
        string_match = "Wheader-hygiene",
        category_suffix = "header_hygiene_violated",
    ),
]
