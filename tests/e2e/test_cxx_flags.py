# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

from __future__ import annotations

import json
import shlex

import pytest
from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test, get_mode_from_platform

FIXTURE = "fbcode//buck2/tests/targets/rules/cxx/cxx_flags"


async def _compile_args(buck: Buck, target: str, source_name: str) -> list[str]:
    result = await buck.build(
        f"{target}[compilation-database]",
        "--show-full-output",
        get_mode_from_platform(),
    )
    output_dict = result.get_target_to_build_output()
    assert len(output_dict) == 1
    with open(next(iter(output_dict.values()))) as f:
        comp_db = json.load(f)
    entries = [e for e in comp_db if e["file"].endswith(source_name)]
    assert len(entries) == 1, f"expected one comp-db entry for {source_name}"
    return entries[0]["arguments"]


async def _shared_cxx_flags_argsfile(buck: Buck, target: str) -> str:
    result = await buck.build(
        f"{target}[argsfiles]",
        "--show-full-output",
        get_mode_from_platform(),
    )
    output_dict = result.get_target_to_build_output()
    assert len(output_dict) == 1
    with open(next(iter(output_dict.values()))) as f:
        target_argsfile = f.read()
    with open(buck.cwd.parent / target_argsfile) as f:
        paths = [
            shlex.split(line)[0].removeprefix("@")
            for line in f
            if "_cxx_flags_argsfile_anon_rule" in line
        ]
    assert len(paths) == 1, f"expected one cxx_flags argsfile in {target_argsfile}"
    return paths[0]


def _assert_before(args: list[str], first: str, second: str) -> None:
    assert first in args, f"{first} not in compile command"
    assert second in args, f"{second} not in compile command"
    assert args.index(first) < args.index(second), (
        f"expected {first} before {second} in {args}"
    )


@buck_test(inplace=True)
async def test_shared_flags_precede_target_flags_on_cxx_library(buck: Buck) -> None:
    args = await _compile_args(buck, f"{FIXTURE}:lib", "lib.cpp")
    _assert_before(args, "-DCXX_FLAGS_BASE_COMPILER=1", "-DCXX_FLAGS_A_COMPILER=1")
    _assert_before(args, "-DCXX_FLAGS_A_COMPILER=1", "-DCXX_FLAGS_B_COMPILER=1")
    _assert_before(args, "-DCXX_FLAGS_B_COMPILER=1", "-DTARGET_COMPILER_FLAG=1")
    assert args.count("-DCXX_FLAGS_BASE_COMPILER=1") == 1
    _assert_before(args, "-DCXX_FLAGS_A_LANG_CXX=1", "-DCXX_FLAGS_B_LANG_CXX=1")
    _assert_before(args, "-DCXX_FLAGS_B_LANG_CXX=1", "-DTARGET_LANG_CXX=1")
    assert "-DCXX_FLAGS_A_PP=1" in args
    assert "-DCXX_FLAGS_A_LANG_PP=1" in args


@buck_test(inplace=True)
async def test_no_shared_flags_means_no_shared_tokens(buck: Buck) -> None:
    args = await _compile_args(buck, f"{FIXTURE}:lib_no_flags", "lib.cpp")
    assert "-DTARGET_COMPILER_FLAG=1" in args
    for token in (
        "-DCXX_FLAGS_A_COMPILER=1",
        "-DCXX_FLAGS_B_COMPILER=1",
        "-DCXX_FLAGS_BASE_COMPILER=1",
        "-DCXX_FLAGS_A_LANG_CXX=1",
        "-DCXX_FLAGS_A_PP=1",
    ):
        assert token not in args, f"unexpected shared flag token {token}"


@buck_test(inplace=True)
async def test_shared_flags_on_cxx_binary_and_cxx_test(buck: Buck) -> None:
    for target in (f"{FIXTURE}:bin", f"{FIXTURE}:test"):
        args = await _compile_args(buck, target, "main.cpp")
        _assert_before(args, "-DCXX_FLAGS_A_COMPILER=1", "-DTARGET_COMPILER_FLAG=1")


@buck_test(inplace=True)
async def test_repeated_flags_label_contributes_once(buck: Buck) -> None:
    args = await _compile_args(buck, f"{FIXTURE}:lib_dup", "lib.cpp")
    assert args.count("-DCXX_FLAGS_A_COMPILER=1") == 1


@buck_test(inplace=True)
async def test_consumers_share_combined_cxx_flags_argsfile(buck: Buck) -> None:
    lib_argsfile = await _shared_cxx_flags_argsfile(buck, f"{FIXTURE}:lib")
    shared_argsfile = await _shared_cxx_flags_argsfile(
        buck,
        f"{FIXTURE}:lib_shared",
    )
    assert lib_argsfile == shared_argsfile
    with open(buck.cwd.parent / lib_argsfile) as f:
        flags = f.read()
    assert "-DCXX_FLAGS_BASE_COMPILER=1" in flags
    assert "-DCXX_FLAGS_A_COMPILER=1" in flags
    assert "-DCXX_FLAGS_B_COMPILER=1" in flags
    assert "-DTARGET_COMPILER_FLAG=1" not in flags
    assert "-DSHARED_TARGET_COMPILER_FLAG=1" not in flags


@buck_test(inplace=True)
@pytest.mark.parametrize("target", ["bin", "lib_shared[shared]"])
async def test_shared_linker_flags_precede_target_linker_flags(
    buck: Buck, target: str
) -> None:
    result = await buck.build(
        f"{FIXTURE}:{target}[linker.argsfile]",
        "--show-full-output",
        get_mode_from_platform(),
    )
    output_dict = result.get_target_to_build_output()
    assert len(output_dict) == 1
    with open(next(iter(output_dict.values()))) as f:
        argsfile = f.read()
    shared_sentinel = "cxx_flags_a_linker_sentinel"
    target_sentinel = "target_linker_sentinel"
    assert shared_sentinel in argsfile, argsfile
    assert target_sentinel in argsfile, argsfile
    assert argsfile.index(shared_sentinel) < argsfile.index(target_sentinel)


@buck_test(inplace=True)
async def test_cython_flags_are_not_cxx_flags(buck: Buck) -> None:
    result = await buck.build(
        "fbcode//buck2/tests/targets/rules/python/cython_test:hello_cython",
        "--show-full-output",
        get_mode_from_platform(),
    )
    output_dict = result.get_target_to_build_output()
    assert len(output_dict) == 1
    output = next(iter(output_dict.values()))
    assert output.endswith("hello.cpp"), output
    with open(output) as f:
        assert "hello from cython" in f.read()


@buck_test(inplace=True)
async def test_non_cxx_flags_dep_is_rejected(buck: Buck) -> None:
    await expect_failure(
        buck.build(
            f"{FIXTURE}/bad:consumer[compilation-database]",
            get_mode_from_platform(),
        ),
        stderr_regex="CxxFlagsInfo",
    )


@buck_test(inplace=True)
@pytest.mark.parametrize(
    "kind, extension",
    [("s", "s"), ("sx", "sx"), ("upper_s", "S"), ("asm", "asm"), ("asmpp", "asmpp")],
    ids=["gnu_s", "gnu_sx", "gnu_upper_s", "nasm", "nasm_cpp"],
)
async def test_assembly_language_flags(buck: Buck, kind: str, extension: str) -> None:
    args = await _compile_args(
        buck, f"{FIXTURE}:assembly_{kind}", f"assembly.{extension}"
    )
    expected, unexpected = (
        ("NASM", "GNU") if extension in ("asm", "asmpp") else ("GNU", "NASM")
    )
    for category in ("COMPILER", "PREPROCESSOR"):
        assert f"-D{expected}_{category}=1" in args
        assert f"-D{unexpected}_{category}=1" not in args
