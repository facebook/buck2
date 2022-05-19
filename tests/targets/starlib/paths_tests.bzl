# Copyright 2017 The Bazel Authors. All rights reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

"""Unit tests for paths.bzl"""

load("@fbcode//buck2/prelude:paths.bzl", "paths")
load("@fbcode//buck2/tests/targets/starlib:unittest.bzl", "asserts")

def basename_test():
    """Unit tests for paths.basename."""

    # Verify some degenerate cases.
    asserts.equals("", paths.basename(""))
    asserts.equals("", paths.basename("/"))
    asserts.equals("bar", paths.basename("foo///bar"))

    # Verify some realistic cases.
    asserts.equals("foo", paths.basename("foo"))
    asserts.equals("foo", paths.basename("/foo"))
    asserts.equals("foo", paths.basename("bar/foo"))
    asserts.equals("foo", paths.basename("/bar/foo"))

    # Verify that we correctly duplicate Python's os.path.basename behavior,
    # where a trailing slash means the basename is empty.
    asserts.equals("", paths.basename("foo/"))
    asserts.equals("", paths.basename("/foo/"))

def dirname_test():
    """Unit tests for paths.dirname."""

    # Verify some degenerate cases.
    asserts.equals("", paths.dirname(""))
    asserts.equals("/", paths.dirname("/"))
    asserts.equals("foo", paths.dirname("foo///bar"))

    # Verify some realistic cases.
    asserts.equals("", paths.dirname("foo"))
    asserts.equals("/", paths.dirname("/foo"))
    asserts.equals("bar", paths.dirname("bar/foo"))
    asserts.equals("/bar", paths.dirname("/bar/foo"))

    # Verify that we correctly duplicate Python's os.path.dirname behavior,
    # where a trailing slash means the dirname is the same as the original
    # path (without the trailing slash).
    asserts.equals("foo", paths.dirname("foo/"))
    asserts.equals("/foo", paths.dirname("/foo/"))

def is_absolute_test():
    """Unit tests for paths.is_absolute."""

    # Try a degenerate case.
    asserts.false(paths.is_absolute(""))

    # Try some relative paths.
    asserts.false(paths.is_absolute("foo"))
    asserts.false(paths.is_absolute("foo/"))
    asserts.false(paths.is_absolute("foo/bar"))

    # Try some Linux absolute paths.
    asserts.true(paths.is_absolute("/"))
    asserts.true(paths.is_absolute("/foo"))
    asserts.true(paths.is_absolute("/foo/"))
    asserts.true(paths.is_absolute("/foo/bar"))

def join_test():
    """Unit tests for paths.join."""

    # Try a degenerate case.
    asserts.equals("", paths.join(""))

    # Try some basic paths.
    asserts.equals("foo", paths.join("foo"))
    asserts.equals("foo/bar", paths.join("foo", "bar"))
    asserts.equals("foo/bar/baz", paths.join("foo", "bar", "baz"))

    # Make sure an initially absolute path stays absolute.
    asserts.equals("/foo", paths.join("/foo"))
    asserts.equals("/foo/bar", paths.join("/foo", "bar"))

    # Make sure an absolute path later in the list resets the result.
    asserts.equals("/baz", paths.join("foo", "bar", "/baz"))
    asserts.equals("/baz", paths.join("foo", "/bar", "/baz"))
    asserts.equals("/bar/baz", paths.join("foo", "/bar", "baz"))
    asserts.equals("/bar", paths.join("/foo", "/bar"))

    # Make sure a leading empty segment doesn't make it absolute.
    asserts.equals("foo", paths.join("", "foo"))

    # Try some trailing slash scenarios.
    asserts.equals("foo/", paths.join("foo", ""))
    asserts.equals("foo/", paths.join("foo/"))
    asserts.equals("foo/", paths.join("foo/", ""))
    asserts.equals("foo//", paths.join("foo//", ""))
    asserts.equals("foo//", paths.join("foo//"))
    asserts.equals("foo/bar/baz/", paths.join("foo/", "bar/", "baz", ""))
    asserts.equals("foo/bar/baz/", paths.join("foo/", "bar/", "baz/"))
    asserts.equals("foo/bar/baz/", paths.join("foo/", "bar/", "baz/", ""))

    # Make sure that adjacent empty segments don't add extra path separators.
    asserts.equals("foo/", paths.join("foo", "", ""))
    asserts.equals("foo", paths.join("", "", "foo"))
    asserts.equals("foo/bar", paths.join("foo", "", "", "bar"))

def normalize_test():
    """Unit tests for paths.normalize."""

    # Try the most basic case.
    asserts.equals(".", paths.normalize(""))

    # Try some basic adjacent-slash removal.
    asserts.equals("foo/bar", paths.normalize("foo//bar"))
    asserts.equals("foo/bar", paths.normalize("foo////bar"))

    # Try some "." removal.
    asserts.equals("foo/bar", paths.normalize("foo/./bar"))
    asserts.equals("foo/bar", paths.normalize("./foo/bar"))
    asserts.equals("foo/bar", paths.normalize("foo/bar/."))
    asserts.equals("/", paths.normalize("/."))

    # Try some ".." removal.
    asserts.equals("bar", paths.normalize("foo/../bar"))
    asserts.equals("foo", paths.normalize("foo/bar/.."))
    asserts.equals(".", paths.normalize("foo/.."))
    asserts.equals(".", paths.normalize("foo/bar/../.."))
    asserts.equals("..", paths.normalize("foo/../.."))
    asserts.equals("/", paths.normalize("/foo/../.."))
    asserts.equals("../../c", paths.normalize("a/b/../../../../c/d/.."))

    # Make sure one or two initial slashes are preserved, but three or more are
    # collapsed to a single slash.
    asserts.equals("/foo", paths.normalize("/foo"))
    asserts.equals("//foo", paths.normalize("//foo"))
    asserts.equals("/foo", paths.normalize("///foo"))

    # Trailing slashes should be removed unless the entire path is a trailing
    # slash.
    asserts.equals("/", paths.normalize("/"))
    asserts.equals("foo", paths.normalize("foo/"))
    asserts.equals("foo/bar", paths.normalize("foo/bar/"))

def relativize_test():
    """Unit tests for paths.relativize."""

    # Make sure that relative-to-current-directory works in all forms.
    asserts.equals("foo", paths.relativize("foo", ""))
    asserts.equals("foo", paths.relativize("foo", "."))

    # Try some regular cases.
    asserts.equals("bar", paths.relativize("foo/bar", "foo"))
    asserts.equals("baz", paths.relativize("foo/bar/baz", "foo/bar"))
    asserts.equals("bar/baz", paths.relativize("foo/bar/baz", "foo"))

    # Try a case where a parent directory is normalized away.
    asserts.equals("baz", paths.relativize("foo/bar/../baz", "foo"))

def replace_extension_test():
    """Unit tests for paths.replace_extension."""

    # Try some degenerate cases.
    asserts.equals(".foo", paths.replace_extension("", ".foo"))
    asserts.equals("/.foo", paths.replace_extension("/", ".foo"))
    asserts.equals("foo.bar", paths.replace_extension("foo", ".bar"))

    # Try a directory with an extension and basename that doesn't have one.
    asserts.equals("foo.bar/baz.quux", paths.replace_extension("foo.bar/baz", ".quux"))

    # Now try some things with legit extensions.
    asserts.equals("a.z", paths.replace_extension("a.b", ".z"))
    asserts.equals("a.b.z", paths.replace_extension("a.b.c", ".z"))
    asserts.equals("a/b.z", paths.replace_extension("a/b.c", ".z"))
    asserts.equals("a.b/c.z", paths.replace_extension("a.b/c.d", ".z"))
    asserts.equals(".a/b.z", paths.replace_extension(".a/b.c", ".z"))
    asserts.equals(".a.z", paths.replace_extension(".a.b", ".z"))

    # Verify that we don't insert a period on the extension if none is provided.
    asserts.equals("foobaz", paths.replace_extension("foo.bar", "baz"))

def split_extension_test():
    """Unit tests for paths.split_extension."""

    # Try some degenerate cases.
    asserts.equals(("", ""), paths.split_extension(""))
    asserts.equals(("/", ""), paths.split_extension("/"))
    asserts.equals(("foo", ""), paths.split_extension("foo"))

    # Try some paths whose basenames start with ".".
    asserts.equals((".", ""), paths.split_extension("."))
    asserts.equals((".bashrc", ""), paths.split_extension(".bashrc"))
    asserts.equals(("foo/.bashrc", ""), paths.split_extension("foo/.bashrc"))
    asserts.equals((".foo/.bashrc", ""), paths.split_extension(".foo/.bashrc"))

    # Try some directories with extensions with basenames that don't have one.
    asserts.equals(("foo.bar/baz", ""), paths.split_extension("foo.bar/baz"))
    asserts.equals(("foo.bar/.bashrc", ""), paths.split_extension("foo.bar/.bashrc"))

    # Now try some things that will actually get split.
    asserts.equals(("a", ".b"), paths.split_extension("a.b"))
    asserts.equals(("a.b", ".c"), paths.split_extension("a.b.c"))
    asserts.equals(("a/b", ".c"), paths.split_extension("a/b.c"))
    asserts.equals(("a.b/c", ".d"), paths.split_extension("a.b/c.d"))
    asserts.equals((".a/b", ".c"), paths.split_extension(".a/b.c"))
    asserts.equals((".a", ".b"), paths.split_extension(".a.b"))

def strip_suffix_test():
    """Unit tests for paths.strip_suffix."""

    asserts.equals("a", paths.strip_suffix("a/b", "b"))
    asserts.equals("a", paths.strip_suffix("a/b/c", "b/c"))
    asserts.equals("", paths.strip_suffix("a/b", "a/b"))
    asserts.equals(None, paths.strip_suffix("a/b", "c/b"))
    asserts.equals(None, paths.strip_suffix("a/b", "c"))

def starts_with_test():
    """Unit tests for paths.starts_with."""

    asserts.equals(True, paths.starts_with("a/b", "a"))
    asserts.equals(True, paths.starts_with("a/b", "a/b"))
    asserts.equals(False, paths.starts_with("a/b", "b"))
    asserts.equals(False, paths.starts_with("ab/c", "a"))

def test():
    """Creates the test targets for paths.bzl tests."""
    basename_test()
    dirname_test()
    is_absolute_test()
    join_test()
    normalize_test()
    relativize_test()
    replace_extension_test()
    split_extension_test()
    strip_suffix_test()
