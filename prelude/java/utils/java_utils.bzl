# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//utils:utils.bzl", "expect")

def get_path_separator() -> "string":
    # TODO: msemko : replace with system-dependent path-separator character
    # On UNIX systems, this character is ':'; on Microsoft Windows systems it is ';'.
    return ":"

def derive_javac(javac_attribute: [str.type, "dependency", "artifact"]) -> [str.type, "RunInfo", "artifact"]:
    javac_attr_type = type(javac_attribute)
    if javac_attr_type == "dependency":
        javac_run_info = javac_attribute.get(RunInfo)
        if javac_run_info:
            return javac_run_info
        outputs = javac_attribute[DefaultInfo].default_outputs
        expect(len(outputs) == 1, "Expect one default output from build dep of attr javac!")
        return outputs[0]

    if javac_attr_type == "artifact":
        return javac_attribute

    if javac_attr_type == str.type:
        return javac_attribute

    fail("Type of attribute javac {} that equals to {} is not supported.\n Supported types are \"dependency\", \"artifact\" and \"string\".".format(javac_attr_type, javac_attribute))
