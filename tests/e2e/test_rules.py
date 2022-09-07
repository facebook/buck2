import json
import os
import re
import sys
import typing
from pathlib import Path

import pytest
from xplat.build_infra.buck_e2e.api.buck import Buck
from xplat.build_infra.buck_e2e.api.buck_result import BuckResult
from xplat.build_infra.buck_e2e.asserts import expect_failure
from xplat.build_infra.buck_e2e.buck_workspace import buck_test, env

# builds targets in an fbcode target configuration, unsupported on mac RE workers
def fbcode_linux_only() -> bool:
    return sys.platform == "linux"


def mac_only() -> bool:
    return sys.platform == "darwin"


@buck_test(inplace=True)
async def test_genrule(buck: Buck) -> None:
    await buck.build("fbcode//buck2/tests/targets/rules/genrule:")
    await buck.build(
        "fbcode//buck2/tests/targets/rules/genrule/write_to_file_query_macros:"
    )
    await buck.build("fbcode//buck2/tests/targets/rules/genrule/named_outputs:")
    await expect_failure(
        buck.build("fbcode//buck2/tests/targets/rules/genrule/bad:my_genrule_bad"),
        stderr_regex="Action failed to produce output",
    )
    await expect_failure(
        buck.build("fbcode//buck2/tests/targets/rules/genrule/bad:my_genrule_bad_2"),
        stderr_regex="(failed with exit code 1|returned non-zero exit code 1)",
    )
    await expect_failure(
        buck.build("fbcode//buck2/tests/targets/rules/genrule/bad:my_genrule_bad_2"),
        stderr_regex="frecli cas download-action",
    )
    await expect_failure(
        buck.build("fbcode//buck2/tests/targets/rules/genrule/bad:my_genrule_bad_4"),
        stderr_regex="script_that_doesnt_exist",
    )


if fbcode_linux_only():

    @buck_test(inplace=True)
    async def test_haskell(buck: Buck) -> None:
        await buck.build("fbcode//buck2/tests/targets/rules/haskell/...")


if fbcode_linux_only():

    @buck_test(inplace=True)
    async def test_ocaml(buck: Buck) -> None:
        await buck.build("fbcode//buck2/tests/targets/rules/ocaml/...")
        await expect_failure(
            buck.build(
                "fbcode//buck2/tests/targets/rules/ocaml/cyclic-modules:cyclic_modules",
                "-c",
                "ocaml.enable-compile-fail-tests=yes",
            ),
            stderr_regex="error: fail: cycle in graph detected in `topo_sort`",
        )


_RUST_TESTS = [
    "fbcode//buck2/tests/targets/rules/rust:",
    "fbcode//buck2/tests/targets/rules/rust/cdylib:",
    # Some bad sanitizer interaction
    # "fbcode//buck2/tests/targets/rules/rust/dlopen:",
    "fbcode//buck2/tests/targets/rules/rust/flagged_deps:",
    "fbcode//buck2/tests/targets/rules/rust/hello_world:",
    "fbcode//buck2/tests/targets/rules/rust/rustdoc:",
]

_RUST_EXPECT_FAIL = [
    (
        "fbcode//buck2/tests/targets/rules/rust/bad:lib_with_error",
        "expected `&str`, found integer",
    ),
    (
        "fbcode//buck2/tests/targets/rules/rust/bad/flagged_deps:with_flagged_dep_fail",
        "use of undeclared crate or module `lib`",
    ),
]

if fbcode_linux_only():

    @buck_test(inplace=True)
    async def test_rust(buck: Buck) -> None:
        await buck.build(*_RUST_TESTS)
        for bad_rule, expect in _RUST_EXPECT_FAIL:
            await expect_failure(
                buck.build(bad_rule),
                stderr_regex=expect,
            )


if fbcode_linux_only():

    @buck_test(inplace=True)
    async def test_rust_pipelined(buck: Buck) -> None:
        await buck.build(*_RUST_TESTS, "-c", "rust.pipelined=true")
        for bad_rule, expect in _RUST_EXPECT_FAIL:
            await expect_failure(
                buck.build(bad_rule, "-c", "rust.pipelined=true"),
                stderr_regex=expect,
            )


if fbcode_linux_only():

    @buck_test(inplace=True)
    async def test_rust_failure_filter(buck: Buck) -> None:
        await buck.build(*_RUST_TESTS, "-c", "rust.failure_filter=true")
        for bad_rule, expect in _RUST_EXPECT_FAIL:
            await expect_failure(
                buck.build(bad_rule, "-c", "rust.failure_filter=true"),
                stderr_regex=expect,
            )


@buck_test(inplace=True)
async def test_apple(buck: Buck) -> None:
    await buck.build("fbcode//buck2/tests/targets/rules/apple/...")
    await buck.build(
        "fbsource//fbobjc/buck2/tests/...",
        "-c",
        "apple.codesign_type_override=skip",
    )
    await buck.build(
        "fbsource//fbobjc/buck2/samples/...",
        "-c",
        "xplat.available_platforms=APPLE,CXX",
        "-c",
        "apple.codesign_type_override=skip",
    )


@buck_test(inplace=True)
async def test_apple_tests(buck: Buck) -> None:
    await buck.test(
        "-c",
        "xplat.available_platforms=APPLE,CXX",
        "fbsource//fbobjc/Samples/TestInfra/TpxUnitTests:TpxUnitTests",
        "fbsource//fbobjc/Samples/TestInfra/TpxUnitTests:TpxUnitAppTests",
    )


@buck_test(inplace=True)
async def test_ide(buck: Buck) -> None:
    await buck.build("fbsource//xplat/buck2/tests/ide_integrations/...")


@buck_test(inplace=True)
async def test_work(buck: Buck) -> None:
    await buck.build("fbcode//buck2/tests/targets/rules/worker:")


@buck_test(inplace=True)
async def test_command_alias(buck: Buck) -> None:
    await buck.build("fbcode//buck2/tests/targets/rules/command_alias:")
    await buck.build("fbcode//buck2/tests/targets/rules/command_alias/single_arg_exe:")


if fbcode_linux_only():

    @buck_test(inplace=True)
    async def test_cxx(buck: Buck) -> None:
        await buck.build("fbcode//buck2/tests/targets/rules/cxx/...")
        # Raw header conversion tests require a config setting.
        await buck.build(
            "fbcode//buck2/tests/targets/rules/cxx/headers_as_raw_headers:",
            "-c" "cxx.headers_as_raw_headers_mode=disabled",
        )

    @buck_test(inplace=True)
    @env("BUCK2_KEEP_DEP_FILE_DIRECTORIES", "true")
    async def test_cxx_dep_files(buck: Buck) -> None:
        cpp9 = "fbcode//buck2/tests/targets/rules/cxx:my_cpp9"
        await buck.build(cpp9)
        res = await buck.audit_dep_files(cpp9, "cxx_compile", "cpp9.cpp")
        out = res.stdout

        # Check that we are tracking our dependency on stdlib headers, even
        # though they are neither explicitly included nor tagged.
        assert re.search(
            "untagged.*fbcode/third-party-buck/.*/build/glibc/include", out
        )

        # Check that we are tracking directly-included headrs
        assert re.search("headers.*glog/include", out)

        # Check that we are tracking transitively-included headrs
        assert re.search("headers.*gflags/include", out)


@buck_test(inplace=True)
async def test_cxx_comp_db(buck: Buck) -> None:
    async def assert_comp_db_is_reasonable(buck: Buck, target: str) -> None:
        result = await buck.build(target + "[compilation-database]")
        output = result.get_build_report().output_for_target(
            target, "compilation-database"
        )
        with output.open() as f:
            # See https://clang.llvm.org/docs/JSONCompilationDatabase.html for the contract for compilation databases
            contents = json.load(f)
            assert isinstance(contents, list), "compilation database isn't an array"
            assert contents, "compilation database is empty"
            for entry in contents:
                for key in ["directory", "file", "arguments"]:
                    assert key in entry, f"key {key} not found in command object"

                arguments = entry["arguments"]
                assert isinstance(arguments, list), "arguments should be a list"
                for arg in arguments:
                    assert (
                        '\\"' not in arg
                    ), f"argument {arg} should not contain escaped quotes"
                    assert (
                        "\\n" not in arg
                    ), f"argument {arg} should not contain escaped newlines"

    await assert_comp_db_is_reasonable(
        buck, "fbcode//buck2/tests/targets/rules/cxx/comp_db:lib"
    )
    await assert_comp_db_is_reasonable(
        buck, "fbcode//buck2/tests/targets/rules/cxx/comp_db:bin"
    )
    await assert_comp_db_is_reasonable(
        buck, "fbcode//buck2/tests/targets/rules/cxx/comp_db:test"
    )


@buck_test(inplace=True)
async def test_alias(buck: Buck) -> None:
    await buck.build("fbcode//buck2/tests/targets/rules/alias:")


@buck_test(inplace=True)
async def test_configured_alias(buck: Buck) -> None:
    await buck.build("fbcode//buck2/tests/targets/rules/configured_alias:")


@buck_test(inplace=True)
@pytest.mark.parametrize("package_style", ["standalone", "inplace"])
async def test_python(buck: Buck, package_style: str) -> None:
    args = [
        "fbcode//buck2/tests/targets/rules/python/...",
        "-c",
        f"python.package_style={package_style}",
    ]
    if sys.platform == "darwin":
        args.append("@fbcode//mode/mac")
    await buck.build(*args)


if fbcode_linux_only():

    @buck_test(inplace=True)
    @pytest.mark.parametrize("package_style", ["standalone", "inplace"])
    async def test_python_cxx(buck: Buck, package_style: str) -> None:
        await buck.build(
            "fbcode//buck2/tests/targets/rules/python_cxx/...",
            "-c",
            f"python.package_style={package_style}",
            "-c",
            "buck2.enable_omnibus_explicit_roots=True",
        )

    @buck_test(inplace=True)
    @pytest.mark.parametrize(
        "sub_target",
        [
            "omnibus-exclusion-roots",
            "omnibus-roots",
            "omnibus-excluded",
            "linkable-graph",
        ],
    )
    async def test_omnibus_metadata(buck: Buck, sub_target: str) -> None:
        await buck.build(
            f"fbcode//buck2/tests/targets/rules/python_cxx/omnibus/cxx_lib_root:bin[{sub_target}]",
            "-c",
            "python.emit_omnibus_metadata=true",
        )

    @buck_test(inplace=True)
    @pytest.mark.parametrize(
        "tool", ["analyze_sharing", "find_implicit_roots", "find_exclusions"]
    )
    async def test_omnibus_tool(buck: Buck, tool: str) -> None:
        def _check_analyze_sharing(res):
            assert re.search(r"Reused:.*1", res.stdout)

        def _check_find_implicit_roots(res):
            out = json.loads(res.stdout)
            assert len(out) == 1
            (k, v) = list(out.items())[0]
            assert "explicit_roots:cpp-lib1" in k
            assert "explicit_roots:lib" in v

        def _check_find_exclusions(res):
            pass

        (args, check) = {
            "analyze_sharing": (
                [
                    "--target",
                    "fbcode//buck2/tests/targets/rules/python_cxx/omnibus/root_sharing:bin",
                ],
                _check_analyze_sharing,
            ),
            "find_implicit_roots": (
                [
                    "--targets",
                    "fbcode//buck2/tests/targets/rules/python_cxx/omnibus/explicit_roots:bin",
                ],
                _check_find_implicit_roots,
            ),
            "find_exclusions": ([], _check_find_exclusions),
        }[tool]

        res = await buck.bxl(
            "-c",
            "python.emit_omnibus_metadata=true",
            "@fbcode//mode/opt",
            f"fbcode//buck2/omnibus_tools/{tool}.bxl:{tool}",
            "--",
            *args,
        )

        check(res)

    @buck_test(inplace=True)
    async def test_native_python_binary(buck: Buck) -> None:
        await buck.run(
            "@//mode/opt",
            "fbcode//python_efficiency/experimental/linking/tests:cpython_greetings",
        )


if mac_only():

    @buck_test(inplace=True)
    async def test_python_mac(buck: Buck) -> None:
        await buck.build(
            "@fbcode//mode/mac",
            "fbcode//buck2/tests/targets/rules/python_cxx/omnibus/mult_cpp_ext_roots:check_bin",
        )


@buck_test(inplace=True)
async def test_remote_file(buck: Buck) -> None:
    await buck.build("fbcode//buck2/tests/targets/rules/remote_file:")
    await expect_failure(
        buck.build("fbcode//buck2/tests/targets/rules/remote_file/bad:invalid_sha1"),
        stderr_regex="Invalid sha1 digest",
    )
    await expect_failure(
        buck.build("fbcode//buck2/tests/targets/rules/remote_file/bad:no_sha1"),
        stderr_regex="Must pass in at least one checksum",
    )
    await expect_failure(
        buck.build("fbcode//buck2/tests/targets/rules/remote_file/bad:invalid_sha256"),
        stderr_regex="Invalid sha256 digest",
    )
    await expect_failure(
        buck.build(
            "fbcode//buck2/tests/targets/rules/remote_file/bad:valid_sha1_invalid_sha256"
        ),
        stderr_regex="Invalid sha256 digest",
    )

    await expect_failure(
        buck.build("fbcode//buck2/tests/targets/rules/remote_file/bad:http_error"),
        stderr_regex="404 Not Found",
    )


@buck_test(inplace=True)
async def test_http_archive(buck: Buck) -> None:
    await buck.build("fbcode//buck2/tests/targets/rules/http_archive:")


@buck_test(inplace=True)
async def test_filegroup(buck: Buck) -> None:
    target_pattern = "fbcode//buck2/tests/targets/rules/filegroup:"
    expected_contents = {
        Path("foo.txt"): "foo.txt",
        Path("foo/bar.txt"): "foo/bar.txt",
        Path("dep/subdir/foo.txt"): "subdir/foo.txt",
        Path("dep/subdir/foo/bar.txt"): "subdir/foo/bar.txt",
    }

    result = await buck.build(target_pattern)
    outdir = result.get_build_report().output_for_target(target_pattern + "all_files")

    assert outdir.name == "all_files"
    assert outdir.parent.name == "__all_files__"

    assert count_number_of_files_recursively(outdir) == 4
    for path, contents in expected_contents.items():
        full_path = outdir / path
        assert full_path.read_text().strip() == contents
        # TODO(nmj): Verify that these are symlinks


@buck_test(inplace=True)
async def test_filegroup_subdir_glob(buck: Buck) -> None:
    target_pattern = "fbcode//buck2/tests/targets/rules/filegroup:"
    expected_contents = {
        Path("foo.txt"): "subdir/foo.txt",
        Path("foo/bar.txt"): "subdir/foo/bar.txt",
    }

    result = await buck.build(target_pattern)
    outdir = result.get_build_report().output_for_target(
        target_pattern + "subdir_glob_dep"
    )

    assert outdir.name == "subdir_glob_dep"
    assert outdir.parent.name == "__subdir_glob_dep__"

    assert count_number_of_files_recursively(outdir) == 2
    for path, contents in expected_contents.items():
        full_path = outdir / path
        assert full_path.read_text().strip() == contents


def count_number_of_files_recursively(outdir):
    num_filenames = 0
    for (_, _, filenames) in os.walk(outdir):
        num_filenames += len(filenames)
    return num_filenames


@buck_test(inplace=True)
async def test_cleanup(buck: Buck) -> None:
    # Test for T85589819 - broken cleanup
    target_pattern = "fbcode//buck2/tests/targets/rules/genrule:cleanup"
    result = await buck.build(target_pattern)
    output = result.get_build_report().output_for_target(target_pattern)

    # The output should be something like path/__cleanup__/out/dir1/dir2/output.txt
    # We want to ensure that if we make a file dir1 or dir1/dir2, cleanup still works
    output.unlink()
    output.parent.rmdir()
    output.parent.write_text("File that must be deleted")
    await buck.kill()
    await buck.build(target_pattern)

    output.unlink()
    output.parent.rmdir()
    output.parent.parent.rmdir()
    output.parent.parent.write_text("File that must be deleted")
    await buck.build(target_pattern)


@buck_test(inplace=True)
async def test_utils(buck: Buck) -> None:
    await buck.build("fbcode//buck2/tests/targets/rules/utils:")

    base_path = "fbcode//buck2/tests/targets/rules/utils/bad:"
    result = await buck.targets(base_path)
    targets = [line for line in result.stdout.splitlines() if base_path in line]
    for target in targets:
        if "build_pattern_test_" in target:
            await expect_failure(
                buck.build(target), stderr_regex="Invalid build target pattern"
            )
        elif target.endswith("test_expect_fail_no_format"):
            await expect_failure(
                buck.build(target), stderr_regex="This message has no format arguments."
            )
        elif target.endswith("test_expect_fail_format_one_arg"):
            await expect_failure(
                buck.build(target),
                stderr_regex="This message has the following format arguments: foo.",
            )
        elif target.endswith("test_expect_fail_format_two_args"):
            await expect_failure(
                buck.build(target),
                stderr_regex="This message has the following format arguments: foo followed by bar.",
            )
        else:
            pytest.fail(f"Unexpected test target {target} in {base_path}.")


@buck_test(inplace=True)
async def test_starlib(buck: Buck) -> None:
    await buck.build("fbcode//buck2/tests/targets/starlib:")


async def run_java_tests(
    buck: Buck, enable_javacd: bool, enable_source_only_abi: bool
) -> None:
    config_flags = []
    if enable_javacd:
        config_flags = [
            "-c",
            "buck2.enable_javacd=true",
            "-c",
            "java.javac=",
            "-c",
            "tools.javac=",
        ]

        if enable_source_only_abi:
            config_flags += [
                "-c",
                "buck2.enable_source_only_abi=true",
                "-c",
                "java.abi_generation_mode=source_only",
            ]

    else:
        assert (
            not enable_source_only_abi
        ), "source_only_abi only makes sense with internal javac enabled"

    async def build(*args, **kwargs):
        await buck.build(*(config_flags + list(args)), **kwargs)

    async def test(*args, **kwargs):
        await buck.test(*(config_flags + list(args)), **kwargs)

    await build("fbcode//buck2/tests/targets/rules/java/prebuilt_jar:")
    await expect_failure(
        build("fbcode//buck2/tests/targets/rules/java/prebuilt_jar/bad:"),
        stderr_regex="Extension of the binary_jar attribute has to be equal to",
    )
    await build("fbcode//buck2/tests/targets/rules/java/keystore:")
    await build("fbcode//buck2/tests/targets/rules/java/library:")
    await build(
        "fbcode//buck2/tests/targets/rules/java/library/simple_library_with_multiple_deps:lib[ast]"
    )
    await build("fbcode//buck2/tests/targets/rules/java/library/zipped_sources:")
    await expect_failure(
        build("fbcode//buck2/tests/targets/rules/java/library/java_version_bad:"),
        stderr_regex="No need to set 'source' and/or 'target' attributes when 'java_version' is present",
    )
    await expect_failure(
        build("fbcode//buck2/tests/targets/rules/java/library/java_library_bad:"),
        stderr_regex="error: <identifier> expected",
    )

    await build("fbcode//buck2/tests/targets/rules/java/jar_genrule:")
    await test("fbcode//buck2/tests/targets/rules/java/java_test:")


if fbcode_linux_only():

    @buck_test(inplace=True)
    async def test_java(buck: Buck) -> None:
        await run_java_tests(buck, False, False)

    @buck_test(inplace=True)
    async def test_java_with_javacd(buck: Buck) -> None:
        await run_java_tests(buck, True, False)

    @buck_test(inplace=True)
    async def test_java_with_source_only_abi(buck: Buck) -> None:
        await run_java_tests(buck, True, True)


if fbcode_linux_only():

    @buck_test(inplace=True)
    async def test_kotlin(buck: Buck) -> None:
        await buck.build("fbcode//buck2/tests/targets/rules/kotlin/kotlin_library:")


@buck_test(inplace=True)
async def test_zip_file(buck: Buck) -> None:
    await buck.build("fbcode//buck2/tests/targets/rules/zip_file:")
    await expect_failure(
        buck.build("fbcode//buck2/tests/targets/rules/zip_file/bad:"),
        stderr_regex='Duplicate entry "lemon.txt"',
    )


@buck_test(inplace=True)
async def test_android(buck: Buck) -> None:
    await buck.build("fbsource//fbandroid/buck2/tests/good/...")
    await expect_failure(
        buck.build("fbsource//fbandroid/buck2/tests/bad/resource:drawable"),
        stderr_regex="The following resources were not found:",
    )
    await expect_failure(
        buck.build(
            "fbsource//fbandroid/buck2/tests/bad/resource:android_library_cannot_export_android_resource"
        ),
        stderr_regex="Exported deps are meant to be forwarded onto the classpath for dependents, so only make sense for a target that emits Java bytecode",
    )
    await expect_failure(
        buck.build("fbsource//fbandroid/buck2/tests/bad/classpath_function:"),
        stderr_regex="Invalid traversal depth",
    )
    await expect_failure(
        buck.build(
            "fbsource//fbandroid/buck2/tests/bad/prebuilt_native_library:prebuilt_native_library_is_asset_and_has_wrap_script"
        ),
        stderr_regex="Cannot use `is_asset` and `has_wrap_script` in the same rule",
    )
    await expect_failure(
        buck.build(
            "fbsource//fbandroid/buck2/tests/bad/cxx_library:can_be_asset_and_used_by_wrap_script"
        ),
        stderr_regex="Cannot use `can_be_asset` and `used_by_wrap_script` in the same rule",
    )


@buck_test(inplace=True)
async def test_sh_test(buck: Buck) -> None:
    await buck.build("fbcode//buck2/tests/targets/rules/sh_test:check_test_deps")


if fbcode_linux_only():

    @buck_test(inplace=True)
    async def test_android_tests(buck: Buck) -> None:
        await buck.build("fbsource//fbandroid/buck2/tests/good/instrumentation_test:")
        await buck.test("fbsource//fbandroid/buck2/tests/good/instrumentation_test:")


@buck_test(inplace=True)
async def test_js(buck: Buck) -> None:
    await buck.build("fbcode//buck2/tests/targets/rules/js/js_bundle:")
    await buck.build("fbcode//buck2/tests/targets/rules/js/js_bundle_genrule:")
    await buck.build("fbcode//buck2/tests/targets/rules/js/js_library:")
    await buck.build("fbcode//buck2/tests/targets/rules/js/js_utils:")


@buck_test(inplace=True)
async def test_export_file(buck: Buck) -> None:
    await buck.build("fbcode//buck2/tests/targets/rules/export_file:")


@buck_test(inplace=True)
async def test_configurations(buck: Buck) -> None:
    await buck.build("//buck2/tests/targets/configurations_uncategorized:")


@buck_test(inplace=True)
async def test_config_setting(buck: Buck) -> None:
    config_setting_genrule = "fbcode//buck2/tests/targets/rules/config_setting:genrule"
    result1 = await buck.build(config_setting_genrule)
    assert (
        result1.get_build_report().output_for_target(config_setting_genrule).name
        == "false.txt"
    )
    result2 = await buck.build(config_setting_genrule, "-c", "test.config_setting=true")
    assert (
        result2.get_build_report().output_for_target(config_setting_genrule).name
        == "true.txt"
    )


@buck_test(inplace=True)
async def test_python_bootstrap(buck: Buck) -> None:
    await buck.run("//buck2/tests/targets/rules/python_bootstrap:hello")
    await buck.run("//buck2/tests/targets/rules/python_bootstrap:hello_imported")


@buck_test(inplace=True)
async def test_python_bootstrap_bad(buck: Buck) -> None:
    await expect_failure(
        buck.build("//buck2/tests/targets/rules/python_bootstrap/bad:colliding_deps"),
        stderr_regex="both declare a source file named `lib.py`",
    )


@buck_test(inplace=True)
async def test_argsfiles_subtarget(buck: Buck) -> None:
    base_target = "fbcode//buck2/tests/targets/rules/cxx/exported_deps_propagated:exported_deps_propagated"
    target_pattern = f"{base_target}[argsfiles]"

    result = await buck.build(
        target_pattern, "-c", "build_report.unstable_include_other_outputs=true"
    )
    report = result.get_build_report()
    default_outputs = report.outputs_for_target(base_target, "argsfiles")
    for file_name in ["argsfiles", ".cpp.argsfile"]:
        assert any([output.name == file_name] for output in default_outputs)

    other_outputs = report.results[base_target]["other_outputs"]["argsfiles"]
    assert any(
        [output.endswith("__bottom_dep__/headers.hmap") for output in other_outputs]
    )


if fbcode_linux_only():

    @buck_test(inplace=True)
    async def test_go(buck: Buck) -> None:
        await buck.build("fbcode//buck2/tests/targets/rules/go/...")

    @buck_test(inplace=True)
    async def test_link_groups(buck: Buck) -> None:
        await buck.build(
            "fbcode//tools/build/test/cpp/link_groups/...",
            "@//mode/opt",
            "-c",
            "fbcode.use_link_groups=True",
        )
        await buck.test(
            "fbcode//tools/build/test/cpp/link_groups/...",
            "@//mode/opt",
            "-c",
            "fbcode.use_link_groups=True",
        )
        await buck.build(
            "unicorn/build/test:py_binary",
            "@mode/opt",
            "-c",
            "fbcode.use_link_groups=True",
        )

    async def dist_lto_build_and_verify(buck: Buck, target: str) -> None:
        result = await buck.build(
            target,
            "@//mode/opt-clang-thinlto",
            "-c",
            "fbcode.experimental_distributed_thinlto=True",
            "-c",
            "fbcode.platform=platform010",
        )
        final_binary = result.get_build_report().output_for_target(target)

        # The link plan file is a hallmark of our distributed ThinLTO
        # implementation; its presence implies we actually used a distributed
        # ThinLTO and not a regular ThinLTO or LTO.
        link_plan_file = final_binary.parent / "main.link-plan.json"
        assert link_plan_file.exists()

    @buck_test(inplace=True)
    async def test_distributed_thinlto(buck: Buck) -> None:
        await dist_lto_build_and_verify(
            buck, "fbcode//buck2/tests/targets/rules/cxx/dist_lto:main"
        )
        await buck.test(
            "fbcode//buck2/tests/targets/rules/cxx/dist_lto:enforce-thinlto",
            "@//mode/opt-clang-thinlto",
            "-c",
            "fbcode.experimental_distributed_thinlto=True",
            "-c",
            "fbcode.platform=platform010",
        )

    @buck_test(inplace=True)
    async def test_distributed_thinlto_with_rust(buck: Buck) -> None:
        await dist_lto_build_and_verify(
            buck, "fbcode//buck2/tests/targets/rules/cxx/dist_lto:main"
        )
        await buck.test(
            "fbcode//buck2/tests/targets/rules/cxx/dist_lto:enforce-thinlto-rust",
            "@//mode/opt-clang-thinlto",
            "-c",
            "fbcode.experimental_distributed_thinlto=True",
            "-c",
            "fbcode.platform=platform010",
        )

    @buck_test(inplace=True)
    async def test_distributed_thinlto_cpp_calls_rust(buck: Buck) -> None:
        await dist_lto_build_and_verify(
            buck, "fbcode//buck2/tests/targets/rules/cxx/dist_lto/cpp_calls_rust:main"
        )
        await buck.run(
            "fbcode//buck2/tests/targets/rules/cxx/dist_lto/cpp_calls_rust:main"
        )


def _get_last_json_log(result: BuckResult) -> typing.Dict[str, typing.Any]:
    last_log_line = result.stdout.strip().split("\n")[-1]
    assert len(last_log_line) != 0, "expected 'buck2 log what-ran' to generate stdout"
    return json.loads(last_log_line)


def _assert_incremental_build(
    result: BuckResult,
    target: str,
    build_mode: typing.Optional[str] = None,
) -> None:
    data = _get_last_json_log(result)
    assert data["reason"] == "build"
    assert target in data["identity"]
    assert data["reproducer"]["executor"] == "Local"

    rustc_incremental_flag = list(
        filter(
            lambda x: x.startswith("-Cincremental"),
            data["reproducer"]["details"].get("command", []),
        )
    )
    assert len(rustc_incremental_flag) == 1

    if build_mode is not None:
        assert f"/{build_mode}" in rustc_incremental_flag[0]


def _assert_not_incremental_build(
    result: BuckResult,
    target: str,
) -> None:
    data = _get_last_json_log(result)
    assert data["reason"] == "build"
    assert target in data["identity"]

    rustc_incremental_flag = list(
        filter(
            lambda x: x.startswith("-Cincremental"),
            data["reproducer"]["details"].get("command", []),
        )
    )
    assert len(rustc_incremental_flag) == 0


if fbcode_linux_only():

    @buck_test(inplace=True)
    async def test_rust_bin_incremental_compilation(buck: Buck) -> None:
        target = "fbcode//buck2/tests/targets/rules/rust/rustdoc:self_contained_binary"
        await buck.build(
            target,
            "-c",
            "rust.incremental=buck2/tests/targets/rules/rust/rustdoc",
        )

        result = await buck.log("what-ran", "--format", "json")
        _assert_incremental_build(result, target)

    @buck_test(inplace=True)
    async def test_rust_lib_incremental_compilation(buck: Buck) -> None:
        target = "fbcode//buck2/tests/targets/rules/rust/rustdoc:self_contained"
        await buck.build(
            target,
            "-c",
            "rust.incremental=buck2/tests/targets/rules/rust/rustdoc",
        )

        result = await buck.log("what-ran", "--format", "json")
        _assert_incremental_build(result, target)

    @buck_test(inplace=True)
    async def test_rust_incremental_compilation_invalid_config(buck: Buck) -> None:
        target = "fbcode//buck2/tests/targets/rules/rust/rustdoc:self_contained"
        await buck.build(
            target,
            "-c",
            "rust.incremental=invalid/path",
        )

        result = await buck.log("what-ran", "--format", "json")
        _assert_not_incremental_build(result, target)

    @buck_test(inplace=True)
    async def test_rust_incremental_compilation_with_build_mode(buck: Buck) -> None:
        target = "fbcode//buck2/tests/targets/rules/rust/rustdoc:self_contained"
        await buck.build(
            target,
            "@//mode/opt",
            "-c",
            "rust.incremental=buck2/tests/targets/rules/rust/rustdoc",
        )

        result = await buck.log("what-ran", "--format", "json")
        _assert_incremental_build(result, target, build_mode="opt")

    @buck_test(inplace=True)
    async def test_rust_incremental_compilation_no_config(buck: Buck) -> None:
        target = "fbcode//buck2/tests/targets/rules/rust/rustdoc:self_contained"
        await buck.build(target)

        result = await buck.log("what-ran", "--format", "json")
        _assert_not_incremental_build(result, target)
