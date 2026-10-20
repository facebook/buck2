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
    """The prefix-map replacement for a header tree:
    _infer_include_prefix → _get_prefix_map_replacement."""
    return headers_for_tests.prefix_map_replacement(
        cell = "CELL",
        cell_prefix = cell_prefix,
        package = package,
        header_namespace = header_namespace,
        include_prefix = _infer(srcs_paths, header_namespace, package),
    )

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

def _check_source_root(name, srcs_paths, package, cell_prefix, expected):
    """Checks `common_source_root` and that the replacement it describes maps
    every source header to its path in the repo, for a package in a cell
    located at `cell_prefix`."""
    source_root = headers_for_tests.common_source_root(srcs_paths, package)
    asserts.equals(expected, source_root, "{}: unexpected source root".format(name))
    if source_root == None:
        return
    repo_paths = {tree_path: paths.join(cell_prefix, package, short_path) for tree_path, short_path in srcs_paths.items() if short_path != None}
    example, parents = source_root
    replacement = "/".join(repo_paths[example].split("/")[:-parents])
    asserts.true(replacement != "", "{}: an empty replacement makes paths absolute".format(name))
    for tree_path, repo_path in repo_paths.items():
        asserts.equals(repo_path, paths.join(replacement, tree_path), "{}: {} must map to its source path".format(name, tree_path))

def test_common_source_root():
    # raw_headers converted to a tree keyed relative to the include directory.
    _check_source_root(
        "raw headers",
        {
            "scripting_model/HostObject.h": "code/include/scripting_model/HostObject.h",
            "scripting_model/attributes/SingletonConstructorAttribute.h": "code/include/scripting_model/attributes/SingletonConstructorAttribute.h",
        },
        package = "arvr/projects/mhe/libraries/scripting_model",
        cell_prefix = "",
        expected = ("scripting_model/HostObject.h", 2),
    )

    # Explicit header_namespace with headers at the package root.
    _check_source_root(
        "explicit namespace",
        {"moodycamel/concurrentqueue.h": "concurrentqueue.h"},
        package = "third-party/moodycamel",
        cell_prefix = "",
        expected = ("moodycamel/concurrentqueue.h", 2),
    )

    # Headers under include/ in the package, header_namespace = "".
    _check_source_root(
        "include dir",
        {"mbedtls/aes.h": "include/mbedtls/aes.h"},
        package = "xplat/mobilenetwork/third-party/mbedtls3",
        cell_prefix = "",
        expected = ("mbedtls/aes.h", 2),
    )

    # subdir_glob where the key partially overlaps the package path, in a
    # cell that isn't at the repo root.
    _check_source_root(
        "subdir_glob partial overlap",
        {"security/lionhead/utils/lib.h": "utils/lib.h"},
        package = "xplat/security/lionhead",
        cell_prefix = "fbcode",
        expected = ("security/lionhead/utils/lib.h", 4),
    )

    # All headers at the package root.
    _check_source_root(
        "package root",
        {"bar.h": "bar.h", "foo.h": "foo.h"},
        package = "pkg",
        cell_prefix = "fbcode",
        expected = ("bar.h", 1),
    )

    # Generated headers have no source path and don't take part.
    _check_source_root(
        "generated and source headers",
        {"a.h": "include/a.h", "gen/x.h": None},
        package = "pkg",
        cell_prefix = "fbcode",
        expected = ("a.h", 1),
    )
    _check_source_root(
        "generated headers only",
        {"gen/x.h": None},
        package = "pkg",
        cell_prefix = "fbcode",
        expected = None,
    )

    # Default namespace (the package path): the directory would be the cell
    # root.
    _check_source_root(
        "default namespace",
        {"folly/Foo.h": "Foo.h", "folly/io/IOBuf.h": "io/IOBuf.h"},
        package = "folly",
        cell_prefix = "fbcode",
        expected = None,
    )

    # Headers from different directories.
    _check_source_root(
        "several include dirs",
        {
            "core/Types.h": "code/include/core/Types.h",
            "core/detail/Impl.h": "code/source/core/detail/Impl.h",
        },
        package = "arvr/projects/mhe/libraries/core",
        cell_prefix = "",
        expected = None,
    )

    # Header renamed in the tree.
    _check_source_root(
        "renamed header",
        {"totally/different.h": "some/other/path.h"},
        package = "pkg",
        cell_prefix = "fbcode",
        expected = None,
    )
