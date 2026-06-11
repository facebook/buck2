# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//:asserts.bzl", "asserts")
load("@prelude//:paths.bzl", "paths")
load("@prelude//cxx:headers.bzl", "headers_for_tests")

_infer = headers_for_tests.infer_include_prefix

def _coverage_replacement(srcs_paths, header_namespace, package, cell_prefix):
    """Simulate the full coverage prefix-map flow:
    _infer_include_prefix → _get_prefix_map_replacement → replacement string.

    This mirrors the logic in _get_prefix_map_replacement for the gnu-linker
    path, using string inputs instead of CxxToolchainInfo/Artifact types.
    """
    include_prefix = _infer(srcs_paths, header_namespace, package)
    prefix_target = cell_prefix
    if header_namespace == "":
        if include_prefix != None:
            if include_prefix:
                prefix_target = paths.join(prefix_target, include_prefix) if prefix_target else include_prefix
        else:
            if package:
                prefix_target = paths.join(prefix_target, package) if prefix_target else package
    return prefix_target if prefix_target else "."

def test_infer_include_prefix():
    # ── Paths unchanged by D106137297 ────────────────────────────────

    # Path 1: normal target with header_namespace (e.g. folly)
    # _infer returns None → replacement uses cell_prefix only.
    asserts.equals(
        "fbcode",
        _coverage_replacement(
            srcs_paths = {"aes.h": "include/mbedtls/aes.h"},
            header_namespace = "folly",
            package = "folly",
            cell_prefix = "fbcode",
        ),
        "normal target: replacement should be just cell_prefix",
    )

    # Path 4: subdir_glob where key partially overlaps package path
    # _infer returns cell-relative prefix → replacement is cell_prefix/prefix.
    asserts.equals(
        "fbcode/xplat",
        _coverage_replacement(
            srcs_paths = {"security/lionhead/utils/lib.h": "utils/lib.h"},
            header_namespace = "",
            package = "xplat/security/lionhead",
            cell_prefix = "fbcode",
        ),
        "subdir_glob partial overlap: replacement should be cell_prefix/prefix",
    )

    # Path 5: subdir_glob where key fully encodes the package path
    # _infer returns "" → replacement is just cell_prefix.
    asserts.equals(
        "xplat",
        _coverage_replacement(
            srcs_paths = {"xplat/security/lionhead/utils/lib.h": "utils/lib.h"},
            header_namespace = "",
            package = "xplat/security/lionhead",
            cell_prefix = "xplat",
        ),
        "subdir_glob full overlap: replacement should be just cell_prefix",
    )

    # Path 6: header layout where no match can be determined
    # _infer returns None → fallback to cell_prefix/package.
    asserts.equals(
        "fbcode/pkg",
        _coverage_replacement(
            srcs_paths = {"totally/different.h": "some/other/path.h"},
            header_namespace = "",
            package = "pkg",
            cell_prefix = "fbcode",
        ),
        "no match: replacement should fall back to cell_prefix/package",
    )

    # ── Paths fixed by D106137297 ────────────────────────────────────

    # Path 2 (Bug 1: direct match dropped package path)
    #
    # mbedtls3 third-party headers live under include/ relative to the
    # package dir. The symlink key is "mbedtls/aes.h", the source path
    # is "include/mbedtls/aes.h".
    #
    # Before: _infer returned "include" (package-relative). The coverage
    #   replacement became just "include" — completely wrong.
    # After:  _infer returns "xplat/.../mbedtls3/include" (cell-relative).
    #   The coverage replacement correctly points into the package.
    replacement = _coverage_replacement(
        srcs_paths = {"mbedtls/aes.h": "include/mbedtls/aes.h"},
        header_namespace = "",
        package = "xplat/mobilenetwork/third-party/mbedtls3",
        cell_prefix = "",
    )
    asserts.equals(
        "xplat/mobilenetwork/third-party/mbedtls3/include",
        replacement,
        "mbedtls3: coverage replacement must include full package path",
    )
    asserts.true(
        replacement != "include",
        "regression: old code dropped the package path, producing just 'include'",
    )

    # Path 3 (Bug 2: mixed-base-dir headers)
    #
    # mbedtls3 mixes headers from two base dirs via subdir_glob:
    #   ("include", "mbedtls/*.h")  — most headers
    #   ("", "config_wa_alt.h")     — a few files at package root
    #
    # Before: the root-dir entry exact-matched first → returned ""
    #   immediately, and the replacement was just cell_prefix (missing
    #   the include/ subdirectory).
    # After:  exact-match entries are skipped, _infer finds "include/"
    #   from the next entry.
    replacement = _coverage_replacement(
        srcs_paths = {
            "config_wa_alt.h": "config_wa_alt.h",
            "mbedtls/aes.h": "include/mbedtls/aes.h",
        },
        header_namespace = "",
        package = "xplat/mobilenetwork/third-party/mbedtls3",
        cell_prefix = "",
    )
    asserts.equals(
        "xplat/mobilenetwork/third-party/mbedtls3/include",
        replacement,
        "mixed-base-dir: must skip root-dir entry and find include/ prefix",
    )
    asserts.true(
        replacement != "",
        "regression: old code stopped at exact-match entry and returned ''",
    )

    # Path 7 (consequence of Bug 2 fix: all headers at package root)
    #
    # Every header has short_path == key (all in the package root dir).
    #
    # Before: first exact-match returned "" → replacement was just
    #   cell_prefix, missing the package path.
    # After:  all exact matches are skipped, _infer returns None,
    #   and _get_prefix_map_replacement falls back to cell_prefix/package.
    replacement = _coverage_replacement(
        srcs_paths = {
            "bar.h": "bar.h",
            "foo.h": "foo.h",
        },
        header_namespace = "",
        package = "pkg",
        cell_prefix = "fbcode",
    )
    asserts.equals(
        "fbcode/pkg",
        replacement,
        "all-root headers: must fall back to cell_prefix/package",
    )
    asserts.true(
        replacement != "fbcode",
        "regression: old code returned '' from first exact-match, producing just cell_prefix",
    )
