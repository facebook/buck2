# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

import json
import plistlib
import sys

from typing import Any, Dict, IO, List, Optional, TextIO, Tuple

from apple.tools.plistlib_utils import detect_format_and_load


def _deepmerge_plist_dicts(source: Dict[str, Any], destination: Dict[str, Any]):
    for key in source:
        if key not in destination:
            destination[key] = source[key]
        elif isinstance(source[key], dict) and isinstance(destination[key], dict):
            _deepmerge_plist_dicts(source[key], destination[key])
        elif isinstance(source[key], list) and isinstance(destination[key], list):
            for item in source[key]:
                if item not in destination[key]:
                    destination[key].append(item)
        elif destination[key] == source[key]:
            pass
        else:
            print(
                f'Cannot merge value "{source[key]}" into "{destination[key]}" at "{key}"',
                file=sys.stderr,
            )
            exit(3)


# Corresponding v1 code is contained in `com/facebook/buck/apple/PlistProcessStep.java`, `PlistProcessStep::execute` method.
def _merge_plist_dicts(
    source: Dict[str, Any],
    destination: Dict[str, Any],
    override: bool = False,
) -> None:
    for key, value in source.items():
        if key not in destination:
            destination[key] = value
        elif isinstance(value, dict) and isinstance(destination[key], dict):
            destination[key].update(value)
        elif override:
            # override the value
            destination[key] = value


def _apply_merge(plist_data: Dict[str, Any], merge_path: str) -> None:
    """Apply a merge mutation to plist data."""
    try:
        # Load the plist file to merge
        with open(merge_path, "rb") as merge_file:
            merge_data = detect_format_and_load(merge_file)
            # Merge the data into the main plist
            _deepmerge_plist_dicts(
                source=merge_data,
                destination=plist_data,
            )
    except OSError:
        # If the file can't be found or read, skip this merge operation
        # This maintains compatibility with the current behavior
        pass


def _traverse_keypath(plist_data: Dict[str, Any], keypath_str: str) -> Tuple[str, Any]:
    """Return the internal object in plist_data found at the keypath_str, save
    for the last component."""

    keypath = keypath_str.split(".")
    plist_pointer = plist_data

    for key in keypath[:-1]:
        if isinstance(plist_pointer, list):
            found = False
            for elem in plist_pointer:
                if key in elem:
                    plist_pointer = elem
                    found = True
                    break
            if not found:
                new_object = {key: []}
                plist_pointer.append(new_object)
                plist_pointer = new_object
        if key.endswith("]"):
            key, indexstr = key[:-1].split("[")
            index = int(indexstr)
            plist_pointer = plist_pointer[key][index]
        else:
            if key not in plist_pointer:
                plist_pointer[key] = []
            plist_pointer = plist_pointer[key]

    return keypath[-1], plist_pointer


def _apply_insert(plist_data: Dict[str, Any], insert_params: Dict[str, Any]) -> None:
    """Apply an insert mutation to plist data.
    Sample data:
    <key>CFBundleURLTypes</key>
        <array>
                <dict>
                        <key>CFBundleURLSchemes</key>
                        <array>
                                <string>spotify</string>
                        </array>
                </dict>
                …
        </array>
    </key>

    Sample mutation:
    {"keypath": "CFBundleURLTypes.CFBundleURLSchemes", "value": "fb-messenger-public"}

    Result:
    <key>CFBundleURLTypes</key>
        <array>
                <dict>
                        <key>CFBundleURLSchemes</key>
                        <array>
                                <string>spotify</string>
                                <string>fb-messenger-public</string>
                        </array>
                </dict>
                …
        </array>
    </key>
    """
    keypath = insert_params["keypath"]
    value = insert_params["value"]
    key, plist_pointer = _traverse_keypath(plist_data, keypath)

    if isinstance(plist_pointer, list):
        # e.g. See example above where CFBundleURLSchemes is contained in a
        # list inside a dict.
        found = False
        for element in plist_pointer:
            if key in element:
                plist_pointer = element
                found = True
                break

        if not found:
            new_dict = {key: []}
            plist_pointer.append(new_dict)
            plist_pointer = new_dict

    if not isinstance(plist_pointer, dict):
        print(
            f"Expected dictionary at keypath {keypath}, but found: {type(plist_pointer)}",
            file=sys.stderr,
        )
        exit(3)
    if key not in plist_pointer:
        plist_pointer[key] = []
    plist_pointer = plist_pointer[key]

    if value not in plist_pointer:
        plist_pointer.insert(0, value)


def _apply_set(plist_data: Dict[str, Any], set_params: Dict[str, Any]) -> None:
    """Apply a set mutation to plist data."""
    keypath = set_params["keypath"]
    value = set_params["value"]
    key, plist_pointer = _traverse_keypath(plist_data, keypath)
    plist_pointer[key] = value


def _apply_mutations(
    plist_data: Dict[str, Any], mutations: List[tuple[str, str] | tuple[str, dict]]
) -> None:
    """Apply a list of mutation operations to plist data."""
    for mutation in mutations:
        operation, arguments = mutation
        if operation == "merge":
            _apply_merge(plist_data, arguments)
        elif operation == "insert":
            _apply_insert(plist_data, arguments)
        elif operation == "set":
            _apply_set(plist_data, arguments)
        else:
            print(
                f"Attempting to apply unknown plist mutation operation {operation}",
                file=sys.stderr,
            )
            sys.exit(2)


def process(
    input_file: IO[bytes],
    output_file: IO[bytes],
    override_input_file: Optional[IO[bytes]] = None,
    additional_keys: Optional[Dict[str, Any]] = None,
    additional_keys_file: Optional[TextIO] = None,
    override_keys_file: Optional[TextIO] = None,
    mutations_file: Optional[TextIO] = None,
    output_format: plistlib.PlistFormat = plistlib.FMT_BINARY,
) -> None:
    root = detect_format_and_load(input_file)
    if override_input_file is not None:
        extra = detect_format_and_load(override_input_file)
        # Overriding here for backwards compatibility with v1,
        # see `PlistProcessStep::execute` implementation
        _merge_plist_dicts(source=extra, destination=root, override=True)
    if additional_keys is not None:
        _merge_plist_dicts(source=additional_keys, destination=root)
    if additional_keys_file is not None:
        additional_keys_from_file = json.load(additional_keys_file)
        _merge_plist_dicts(source=additional_keys_from_file, destination=root)
    if override_keys_file is not None:
        override_keys_from_file = json.load(override_keys_file)
        _merge_plist_dicts(
            source=override_keys_from_file, destination=root, override=True
        )

    # Apply mutations if provided
    if mutations_file is not None:
        mutations = [tuple(item) for item in json.load(mutations_file)]
        _apply_mutations(root, mutations)

    plistlib.dump(root, output_file, fmt=output_format)
