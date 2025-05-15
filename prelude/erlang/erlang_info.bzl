# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# This file contains the specification for all the providers the Erlang
# integration uses.

# Information about an Erlang application and its dependencies.

ErlangAppCommonFields = [
    # application name
    "name",
    # mapping from ("application", "basename") -> to header artifact
    "includes",
    # path to include directory
    "include_dir",
    # deps files short_path -> artifact
    "deps_files",
]

# target type to break circular dependencies
ErlangAppIncludeInfo = provider(
    fields = ErlangAppCommonFields + [
        # original inclues, used for validating _include_only app against the original
        "_original_includes",
    ],
)

ErlangAppInfo = provider(
    fields =
        ErlangAppCommonFields + [
            # version
            "version",

            # mapping from module name to beam artifact
            "beams",

            # for tests we need to preserve the private includes
            "private_includes",
            "private_include_dir",
            # mapping from name to dependency for all Erlang dependencies
            "dependencies",
            # Transitive Set for calculating the start order
            "start_dependencies",
            # additional targets that the application depends on, the
            # default output will end up in priv/
            "resources",
            # applications that are in path but not build by buck2 are virtual
            # the use-case for virtual apps are OTP applications that are shipeped
            # with the Erlang distribution
            "virtual",
            # app folders for all toolchain
            "app_folders",
            # app_folder for primary toolchain
            "app_folder",
        ],
)

ErlangReleaseInfo = provider(
    fields = {
        "name": provider_field(typing.Any, default = None),
    },
)

# OTP Binaries
ErlangOTPBinariesInfo = provider(
    fields = {
        "erl": provider_field(Artifact),
        "erlc": provider_field(Artifact),
        "escript": provider_field(Artifact),
    },
)

Tool = cmd_args

Tools = record(
    name = field(str),
    erl = field(Tool),
    erlc = field(Tool),
    escript = field(Tool),
    _tools_binaries = field(ErlangOTPBinariesInfo),
)

# toolchain provider
ErlangToolchainInfo = provider(
    # @unsorted-dict-items
    fields = {
        "name": provider_field(str),
        # command line erlc options used when compiling
        "erl_opts": provider_field(list[str]),
        # emulator flags used when calling erl
        "emu_flags": provider_field(list[str]),
        # struct containing the binaries erlc, escript, and erl
        # this is further split into local and RE
        "otp_binaries": provider_field(Tools),
        # utility scripts
        # building .app file
        "app_src_script": provider_field(Tool),
        # building escripts
        "escript_builder": provider_field(Tool),
        # analyzing .(h|e)rl dependencies
        "dependency_analyzer": provider_field(Tool),
        "dependency_finalizer": provider_field(Tool),
        # trampoline rerouting stdout to stderr
        "erlc_trampoline": provider_field(Artifact),
        "escript_trampoline": provider_field(Artifact),
        # name to parse_transform artifacts mapping for core parse_transforms (that are always used) and
        # user defines ones
        "core_parse_transforms": provider_field(dict[str, (Artifact, Artifact)]),
        "parse_transforms": provider_field(dict[str, (Artifact, Artifact)]),
        # filter spec for parse transforms
        "parse_transforms_filters": provider_field(dict[str, list[str]]),
        # release boot script builder
        "boot_script_builder": provider_field(Tool),
        # build release_variables
        "release_variables_builder": provider_field(Tool),
        # copying erts
        "include_erts": provider_field(Tool),
        # beams we need for various reasons
        "utility_modules": provider_field(Artifact),
        # env to be set for toolchain invocations
        "env": provider_field(dict[str, str]),
    },
)

# multi-version toolchain
ErlangMultiVersionToolchainInfo = provider(
    # @unsorted-dict-items
    fields = {
        # toolchains
        "toolchains": provider_field(dict[str, ErlangToolchainInfo]),
        # primary toolchain name
        "primary": provider_field(str),
    },
)

# parse_transform
ErlangParseTransformInfo = provider(
    # @unsorted-dict-items
    fields = {
        # module implementing the parse_transform
        "source": provider_field(Artifact),
        # potential extra files placed in a resource folder
        "extra_files": provider_field(list[Artifact]),
    },
)

ErlangTestInfo = provider(
    # @unsorted-dict-items
    fields =
        {
            # The name of the suite
            "name": provider_field(str),
            # mapping from name to dependency for all Erlang dependencies
            "dependencies": provider_field(dict[str, Dependency]),
            # anchor to the output_dir
            "output_dir": provider_field(Artifact),
        },
)
