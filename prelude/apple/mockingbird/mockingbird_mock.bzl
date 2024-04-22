# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//user:rule_spec.bzl", "RuleRegistrationSpec")
load(":mockingbird_types.bzl", "MockingbirdLibraryInfo", "MockingbirdLibraryRecord", "MockingbirdSourcesInfo")

def _impl(ctx: AnalysisContext) -> list[Provider]:
    mockingbird_info = ctx.attrs.module[MockingbirdLibraryInfo]

    json_project_description = _get_mockingbird_json_project_description(info = mockingbird_info, included_srcs = ctx.attrs.srcs, excluded_srcs = ctx.attrs.excluded_srcs)
    json_project_description_output = ctx.actions.declare_output("mockingbird_project.json")
    ctx.actions.write_json(json_project_description_output.as_output(), json_project_description)

    mockingbird_source = ctx.actions.declare_output(mockingbird_info.name + "Mocks.generated.swift", dir = False)
    cmd = cmd_args()

    for record in mockingbird_info.tset.traverse():
        cmd.hidden(record.src_dir)

    params = [
        ctx.attrs._mockingbird_bin[RunInfo],
        "generate",
        "--target",
        mockingbird_info.name,
        "--project",
        json_project_description_output,
        "--output",
        mockingbird_source.as_output(),
        "--header",
        "// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.",
        "--support",
        ctx.attrs._mockingbird_support[DefaultInfo].default_outputs,
        "--verbose",
        "--disable-cache",
    ]

    if ctx.attrs.only_protocols:
        params.append("--only-protocols")

    cmd.add(params)

    ctx.actions.run(
        cmd,
        category = "mockingbird",
        local_only = True,
    )
    # TODO: T182716646 Remove local_only

    return [
        DefaultInfo(mockingbird_source),
        MockingbirdSourcesInfo(srcs = [mockingbird_source]),
    ]

def _attrs():
    attribs = {
        ## The list of source files to exclude. Only the name of the file, excluding the path, should be set. If set, the srcs attribute will be ignored.
        "excluded_srcs": attrs.list(attrs.string(), default = []),
        ## The module to generate mocks for.
        "module": attrs.dep(),
        ## Whether to only generate mocks for Swift protocols.
        "only_protocols": attrs.bool(default = False),
        ## A list of source files to include. Only the name of the file, excluding the path, should be set. By default all source files are included and this doesn't need to be specified.
        "srcs": attrs.list(attrs.string(), default = []),
        "_mockingbird_bin": attrs.exec_dep(providers = [RunInfo], default = "fbsource//fbobjc/VendorLib/Mockingbird:mockingbird-binary"),
        "_mockingbird_support": attrs.dep(providers = [DefaultInfo], default = "fbsource//fbobjc/VendorLib/Mockingbird:MockingbirdSupport"),
    }
    return attribs

registration_spec = RuleRegistrationSpec(
    name = "mockingbird_mock",
    impl = _impl,
    attrs = _attrs(),
)

# Produce JSON project description for Mockingbird codegen
# https://mockingbirdswift.com/json-project-description
# {
#   "targets": [
#     {
#       "name": "MyLibrary",
#       "type": "library",
#       "path": "/path/to/MyLibrary",
#       "dependencies": [],
#       "sources": [
#         "SourceFileA.swift",
#         "SourceFileB.swift"
#       ]
#     },
#     {
#       "name": "MyOtherLibrary",
#       "type": "library",
#       "path": "/path/to/MyOtherLibrary",
#       "dependencies": [
#         "MyLibrary"
#       ],
#       "sources": [
#         "SourceFileA.swift",
#         "SourceFileB.swift"
#       ]
#     },
#     {
#       "name": "MyLibraryTests",
#       "type": "test",
#       "path": "/path/to/MyLibraryTests",
#       "dependencies": [
#         "MyLibrary"
#       ],
#       "sources": [
#         "SourceFileA.swift",
#         "SourceFileB.swift"
#       ]
#     }
#   ]
# }
def _get_mockingbird_json_project_description(info: MockingbirdLibraryInfo, included_srcs: list[str], excluded_srcs: list[str]) -> dict:
    targets = []
    for record in info.tset.traverse():
        if record.name == info.name:
            targets.append(_target_dict_for_mockingbird_record(record = record, included_srcs = included_srcs, excluded_srcs = excluded_srcs, include_non_exported_deps = True))
        else:
            targets.append(_target_dict_for_mockingbird_record(record = record, included_srcs = [], excluded_srcs = [], include_non_exported_deps = False))
    json = {
        "targets": targets,
    }

    return json

def _target_dict_for_mockingbird_record(record: MockingbirdLibraryRecord, included_srcs: list[str], excluded_srcs: list[str], include_non_exported_deps: bool) -> dict:
    srcs = []
    if len(included_srcs) > 0 and len(excluded_srcs) > 0:
        fail("Included srcs and excluded srcs cannot both be set at the same time")

    if len(included_srcs) > 0:
        for src in record.srcs:
            if src.basename in included_srcs:
                srcs.append(src.basename)
    elif len(excluded_srcs) > 0:
        for src in record.srcs:
            if src.basename not in excluded_srcs:
                srcs.append(src.basename)
    else:
        srcs = [src.basename for src in record.srcs]

    deps = record.exported_dep_names

    if include_non_exported_deps:
        deps = deps + record.dep_names

    return {
        "dependencies": deps,
        "name": record.name,
        "path": record.src_dir,
        "sources": srcs,
        "type": record.type,
    }
