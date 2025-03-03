#!/usr/bin/env fbpython
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

import os
import shutil
import subprocess
import tempfile


def ls_files_cmd(pattern: str) -> list[str]:
    return (
        subprocess.check_output(["find", ".", "-path", f"./{pattern}"])
        .decode("utf-8")
        .splitlines()
    )


def set_generated_tag(pattern: str) -> None:
    tag_name = "generated"
    for f in ls_files_cmd(pattern):
        with tempfile.NamedTemporaryFile("w", delete=False) as temp_file:
            temp_file.write(f"// @{tag_name}\n")
            with open(f, "r") as original_file:
                temp_file.write(original_file.read())
        shutil.move(temp_file.name, f)


def remove_files(pattern: str) -> None:
    for f in ls_files_cmd(pattern):
        os.remove(f)


def main() -> None:
    repo_root = subprocess.check_output(["hg", "root"]).decode("utf-8").strip()
    os.chdir(repo_root + "/fbcode/buck2/prelude/toolchains/android")
    remove_files("src-gen/**/proto/*.java")
    remove_files("src-gen/**/model/*.java")
    for file in ls_files_cmd("src/**/*.proto"):
        print("processing " + file)
        result = subprocess.run(
            [
                "buck",
                "run",
                "prelude//toolchains/android/tools/protobuf:protoc",
                "--",
                "-I=third-party/java/protobuf",
                "-I=src/com/facebook/buck/cd/resources/proto",
                "-I=src/com/facebook/buck/installer/proto",
                "-I=src/com/facebook/buck/workertool/resources/proto",
                "--java_out=src-gen/",
                "--java_opt=annotate_code",
                "--grpc-java_out=src-gen/",
                file,
            ],
            capture_output=True,
        )
        if result.returncode != 0:
            print("failed to generate " + file + "\n" + result.stderr.decode("utf-8"))
            exit(1)
    remove_files("src-gen/**/*.pb.meta")
    set_generated_tag("src-gen/**/proto/*.java")
    set_generated_tag("src-gen/**/model/*.java")
    subprocess.run(
        ["buck", "build", "prelude//toolchains/android/src-gen/..."], check=True
    )


if __name__ == "__main__":
    main()
