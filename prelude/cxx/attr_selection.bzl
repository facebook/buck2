# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def cxx_by_language_ext(x: {"": ""}, ext: str.type, label: "label") -> [""]:
    # lang_preprocessor_flags is indexed by c/cxx
    # lang_compiler_flags is indexed by c_cpp_output/cxx_cpp_output
    # so write a function that can do either
    #
    # === Buck v1 Compatibility ===
    #
    # `lang_compiler_flags` keys are coerced to CxxSource.Type,
    # so the allowable values are the lowercase versions of the enum values.
    #
    # The keys themselves should be the _output_ type of the language. For example,
    # for Obj-C, that would be OBJC_CPP_OUTPUT.
    #
    # The actual lookup for `lang_compiler_flags` happens in
    # CxxSourceRuleFactory::getRuleCompileFlags().
    #
    # `lang_preprocessor_flags` keys are also coerced to CxxSource.Type.
    # The keys are the _input_ type of the language. For example, for Obj-C,
    # that would be OBJC.
    if ext == ".c":
        key_pp = "c"

        # TODO(gabrielrc): v1 docs have other keys
        # https://buck.build/rule/cxx_library.html#lang_compiler_flags
        # And you can see them in java code, but somehow it works with
        # this one, which is seem across the repo. Find out what's happening.
        key_compiler = "c_cpp_output"
    elif ext in (".cpp", ".cc", ".cxx", ".c++"):
        key_pp = "cxx"
        key_compiler = "cxx_cpp_output"
    elif ext == ".m":
        key_pp = "objc"
        key_compiler = "objc_cpp_output"
    elif ext == ".mm":
        key_pp = "objcxx"
        key_compiler = "objcxx_cpp_output"
    elif ext in (".s", ".S"):
        key_pp = "assembler_with_cpp"
        key_compiler = "assembler"
    elif ext == ".cu":
        key_pp = "cuda"
        key_compiler = "cuda_cpp_output"
    elif ext == ".hip":
        key_pp = "hip"
        key_compiler = "hip_cpp_output"
    elif ext in (".asm", ".asmpp"):
        key_pp = "asm_with_cpp"
        key_compiler = "asm"
    elif ext in (".h", ".hpp"):
        # Note that the label is only required for this error message, once we make this a hard error, delete the label argument
        soft_error("starlark_header_in_srcs", "Not allowed to have header files in the `srcs` attribute - put them in `headers`: " + repr(label))

        # Same as .cpp for now
        key_pp = "cxx"
        key_compiler = "cxx_cpp_output"
    else:
        fail("Unexpected file extension: " + ext)
    res = []
    if key_pp in x:
        res += x[key_pp]
    if key_compiler in x:
        res += x[key_compiler]
    return res
