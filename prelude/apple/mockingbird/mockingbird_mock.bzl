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

    json_project_description = _get_mockingbird_json_project_description(mockingbird_info)
    json_project_description_output = ctx.actions.declare_output("mockingbird_project.json")
    ctx.actions.write_json(json_project_description_output.as_output(), json_project_description)

    mockingbird_source = ctx.actions.declare_output(mockingbird_info.name + "Mocks.generated.swift", dir = False)
    cmd = cmd_args()

    for record in mockingbird_info.tset.traverse():
        cmd.hidden(record.src_dir)

    cmd.add(
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
    )
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
        "module": attrs.dep(),
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
def _get_mockingbird_json_project_description(info: MockingbirdLibraryInfo) -> dict:
    json = {
        "targets": [_target_dict_for_mockingbird_record(record) for record in info.tset.traverse()],
    }

    return json

def _target_dict_for_mockingbird_record(record: MockingbirdLibraryRecord) -> dict:
    return {
        "dependencies": record.dep_names,
        "name": record.name,
        "path": record.src_dir,
        "sources": [src.basename for src in record.srcs],
        "type": record.type,
    }
