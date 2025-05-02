# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

import argparse
import json
from pathlib import Path
from typing import Dict, List


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Check whether the provided list of files contains any duplicate class names in them."
    )
    parser.add_argument(
        "--target-name-to-class-names-map-file",
        type=Path,
        help="A file that contains the mapping between target names and file paths containing the class names for that target.",
        required=True,
    )
    parser.add_argument(
        "--validation-output",
        type=Path,
        required=True,
    )
    args = parser.parse_args()

    target_to_class_names_map = get_class_to_target_mapping(
        args.target_name_to_class_names_map_file
    )

    duplicate_classes = {
        class_name: target_name_list
        for class_name, target_name_list in target_to_class_names_map.items()
        if len(target_name_list) > 1
    }

    with open(args.validation_output, "w") as f:
        json.dump(
            {
                "version": 1,
                "data": {
                    "status": "failure" if duplicate_classes else "success",
                    "message": build_validation_message(duplicate_classes),
                },
            },
            f,
        )


def get_class_to_target_mapping(
    target_name_to_class_names_map_file: Path,
) -> Dict[str, List[str]]:
    with open(target_name_to_class_names_map_file) as f:
        target_to_class_name_file_map = json.loads(f.read())

    class_to_target_mapping = {}
    for target_name, path in target_to_class_name_file_map.items():
        with open(path) as f:
            class_names = f.readlines()
            for class_name in class_names:
                if "$" in class_name or "module-info" in class_name:
                    continue
                class_to_target_mapping.setdefault(class_name, []).append(target_name)

    return class_to_target_mapping


def build_validation_message(
    duplicate_classes: Dict[str, List[str]],
) -> str:
    if not duplicate_classes:
        return "No duplicate class names found"

    messages = []
    for class_name, targets in duplicate_classes.items():
        targets_str = ", ".join(sorted(targets))
        messages.extend(
            [f"* {class_name} exists in the following targets: {targets_str}", "\n"]
        )
    return f"""
Duplicate class name(s) found:
{''.join(messages)}
This means multiple copies of the same class is being included in the final apk.
Check the class names and the target names listed above and make sure to only include one copy of each class.
        """


if __name__ == "__main__":
    main()
