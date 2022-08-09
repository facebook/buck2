#!/usr/bin/env fbpython
# (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

""" Python tool to convert JSON project files into XML.

This binary requires two arguments : --input_path, --output_path
Usage:
buck run project_writer -- --input_path=/path/to/input  --output_path=/path/to/output

"""

import argparse
import json
import re
from pathlib import Path
from typing import Any, Dict

# pyre-ignore[21]
import jinja2

IML = ".iml"
XML = ".xml"
JSON = ".json"
JAVA = ".java"

TARGET_INFO_FILE = "target-info.json"
MODULES_DIR = "modules"
LIBRARIES_DIR = "libraries"

FUNC_NAME = "func_name"
TEMPLATE_NAME = "template_name"
EXTENSION = "extension"

# pyre-ignore[11]
_JINJA_ENV: jinja2.Environment = jinja2.Environment(
    loader=jinja2.PackageLoader(
        __package__,
        "templates",
    ),
    undefined=jinja2.DebugUndefined,
    lstrip_blocks=True,
    trim_blocks=True,
    keep_trailing_newline=True,
)


def get_extension(file_name: str, dir_name: str):
    """Gets the appropriate file extension from the map."""

    file_lookup_key: str = file_name
    if dir_name in [MODULES_DIR, LIBRARIES_DIR]:
        file_lookup_key = dir_name
    return FUNCTION_MAP[file_lookup_key][EXTENSION]


def apply_template(
    cur_root_path: Path, file_name: str, template_name: str, output_path: Path
) -> None:
    """Loads and applies the corresponding Jinja template."""
    with open(cur_root_path.joinpath(file_name)) as file_content:
        json_content = json.load(file_content)
        template = _JINJA_ENV.get_template(template_name)
        output_content = template.render(**json_content)
        extension = get_extension(file_name, output_path.name)
        output_file_name = file_name.split(".")[0] + extension
        output_path.mkdir(parents=True, exist_ok=True)
        output_file_path = output_path.joinpath(output_file_name)
        with open(output_file_path, "w") as out_file:
            out_file.write(output_content)


def copy_file(cur_root_path: Path, file_name: str, _, output_path: Path) -> None:
    """For files that don't need templates will be simply copied over.
    The paths listed in the 'target-info.json' are updated to be xml/iml.
    """

    with open(cur_root_path / file_name) as src_file:
        output_path.mkdir(parents=True, exist_ok=True)
        with open(output_path / file_name, "w") as dest_file:
            if file_name == TARGET_INFO_FILE:
                content = src_file.read()
                # The regex replaces the json paths for `libraries` folder with `xml``
                # and `modules` folder with `iml`.
                content_new = re.sub(
                    r"(.idea/libraries/)(.*)(.json)",
                    r"\1\2.xml",
                    re.sub(r"(.idea/modules/)(.*)(.json)", r"\1\2.iml", content),
                )
                dest_file.write(content_new)
            else:
                dest_file.write(src_file.read())


def walk_dir(cur_root_path: Path, dir_name: str, _, output_path: Path) -> None:
    """To process nested directories like 'android_gen'."""

    for cur_file in cur_root_path.joinpath(dir_name).rglob("*" + JSON):
        relative_dir_path = cur_file.relative_to(cur_root_path).parent
        apply_function(
            cur_file.parent, cur_file.name, output_path.joinpath(relative_dir_path)
        )


def process_dir(
    cur_root_path: Path, dir_name: str, template_name: str, output_path: Path
) -> None:
    """Applies template to each file in 'libraries' or 'modules' folder."""

    for file_name in cur_root_path.joinpath(dir_name).glob("*" + JSON):
        apply_template(
            cur_root_path.joinpath(dir_name),
            file_name.name,
            template_name,
            output_path.joinpath(dir_name),
        )


# This dict helps to identify the files in the IntelliJ folder that needs processing
# and also maps the function, template and extension to apply on it.
FUNCTION_MAP: Dict[str, Dict[str, Any]] = {
    "AndroidManifest.json": {
        FUNC_NAME: apply_template,
        TEMPLATE_NAME: "android_manifest_template.j2",
        EXTENSION: XML,
    },
    "Manifest.json": {
        FUNC_NAME: apply_template,
        TEMPLATE_NAME: "generated_java_template.j2",
        EXTENSION: JAVA,
    },
    "misc.json": {
        FUNC_NAME: apply_template,
        TEMPLATE_NAME: "misc_template.j2",
        EXTENSION: XML,
    },
    "modules.json": {
        FUNC_NAME: apply_template,
        TEMPLATE_NAME: "module_index_template.j2",
        EXTENSION: XML,
    },
    "R.json": {
        FUNC_NAME: apply_template,
        TEMPLATE_NAME: "generated_java_template.j2",
        EXTENSION: JAVA,
    },
    "target-configuration-info.json": {
        FUNC_NAME: copy_file,
        TEMPLATE_NAME: None,
    },
    TARGET_INFO_FILE: {
        FUNC_NAME: copy_file,
        TEMPLATE_NAME: None,
    },
    "workspace.json": {
        FUNC_NAME: apply_template,
        TEMPLATE_NAME: "workspace_template.j2",
        EXTENSION: XML,
    },
    "android_gen": {
        FUNC_NAME: walk_dir,
        TEMPLATE_NAME: None,
    },
    LIBRARIES_DIR: {
        FUNC_NAME: process_dir,
        TEMPLATE_NAME: "library_template.j2",
        EXTENSION: XML,
    },
    MODULES_DIR: {
        FUNC_NAME: process_dir,
        TEMPLATE_NAME: "module_template.j2",
        EXTENSION: IML,
    },
}


def apply_function(input_path: Path, file_name: str, output_path: Path) -> None:
    """Retrieves the corresponding function and template to apply on a file/dir."""

    if file_name in FUNCTION_MAP:
        func_name = FUNCTION_MAP[file_name][FUNC_NAME]
        template_name = FUNCTION_MAP[file_name][TEMPLATE_NAME]
        func_name(input_path, file_name, template_name, output_path)


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--input_path",
        type=Path,
        required=True,
        help="Path to the idea directory where the input json project files are available",
    )
    parser.add_argument(
        "--output_path",
        required=True,
        type=Path,
        help="Path to the output directory where the xml project files will be generated",
    )
    args = parser.parse_args()
    input_path = Path(args.input_path).resolve()
    output_path = Path(args.output_path).resolve()

    output_path.mkdir(parents=True, exist_ok=True)
    try:
        for file_path in input_path.iterdir():
            apply_function(input_path, file_path.name, output_path)
    except Exception as e:
        raise e


if __name__ == "__main__":
    main()
