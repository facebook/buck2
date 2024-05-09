# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# Given a list of files, return a tree structure with the shape:
#
# type Tree = dict[component, None | Tree]
#
# Where None indicates a file, and a Tree indicates a directory.
def _build_tree(files):
    tree = {}
    for file in files:
        map = tree

        # For every python file, starting from distillery, figure out every subdirectory and add it to the map　if it's not there already
        components = file.split("/")
        for directory_chunk in components[:-1]:
            map = map.setdefault(directory_chunk, {})
        map[components[-1]] = None

    return tree

def _reduce_tree(path, tree, directory):
    files = []
    dirs = []
    for k, v in tree.items():
        path2 = path + ("/" if path else "") + k
        if v == None:
            files.append(path2)
        else:
            dirs.append(_reduce_tree(path2, v, directory))
    return directory(path, dirs, files)

# Given a list of files, perform a reduction on the tree structure.
# The `directory` argument is a function that takes a path, a list of subdirectory results, and a list of files.
# For example, given the paths `foo/bar.txt` and `foo/baz.txt` it would be called thusly:
#
# directory("", [directory("foo", [], ["foo/bar.txt", "foo/baz.txt"])], [])
def directory_fold(files, directory):
    return _reduce_tree("", _build_tree(files), directory)

def _test_tree_functions():
    input = ["foo/bar/baz.txt", "foo/bar.txt", "foo.txt", "foo/bar/quux.txt", "foo/baz/quux.txt"]
    output = {
        "foo": {
            "bar": {"baz.txt": None, "quux.txt": None},
            "bar.txt": None,
            "baz": {"quux.txt": None},
        },
        "foo.txt": None,
    }
    result = _build_tree(input)
    if result != output:
        fail("_build_tree(), unexpected output. Wanted `{output}`, got `{tree}`".format(output = output, result = result))

    original = directory_fold(input, lambda _name, dirs, files: files + [x for xs in dirs for x in xs])
    if sorted(original) != sorted(input):
        fail("_directory_fold(), unexpected output. Wanted `{input}`, got `{original}`".format(input = input, original = original))

_test_tree_functions()
