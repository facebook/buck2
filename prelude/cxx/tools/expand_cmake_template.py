#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

"""
A compatibility layer for cmake to buck2 migration that expands a template file with substitutions
Supports the following expansions:
 - @substitution@ -> value
 - #cmakedefine substitution -> #define substitution
 - #cmakedefine01 substitution -> #define substitution 1 or #define substitution 0
"""

import argparse
import logging
from pathlib import Path

import json
import re
import logging
import sys
from typing import Union

at_replace = re.compile(r"(@\w+@)")
variable_replace = re.compile(r"(\${\w+})")
regex_cmakedefine = re.compile(r"#cmakedefine (\w+)(?:\s+(.+))?")
cmakedefine01 = re.compile(r"#cmakedefine01 (\w+)")

substitutions_encountered_in_template = set()


def load_file(path:str) -> str:
    with open(path) as fp:
        return fp.read()
    
def resolve_one_embedding(json_spec:dict) -> str:
    result = ""
    result += json_spec["prefix"]
    file_contents = [
        json_spec["item_prefix"] + load_file(file) + json_spec["item_suffix"] for file in json_spec["files"]
    ]
    concatenated_contents = json_spec["delimiter"].join(file_contents)
    if json_spec["trailing_delimiter"]:
            concatenated_contents += json_spec["delimiter"]
    result += concatenated_contents

    result += json_spec["suffix"]
    return result

def resolve_all_embeddings(substitutions:list[str]) -> dict[str, str]:
    embedding_specs = {}
    for spec_file in substitutions:
        with open(spec_file) as fp:
            spec = json.load(fp)
        label = spec["label"]
        embedding = resolve_one_embedding(spec)
        embedding_specs[label] = embedding

    return embedding_specs



def read_substitutions(substitution_file: str) -> dict[str, Union[int,str,bool,None]]:
    substitutions = []
    with open(substitution_file) as f:
        return json.load(f)

def prepare_substitutions(key_value_pairs: list[str], escape_quotes: bool) -> dict[str, Union[int,str,bool,None]]:
    substitutions = {}
    for s in key_value_pairs:
        k, v = s.split("=")
        if escape_quotes:
            v = v.replace('"', '\\"')
        substitutions[k] = v
    return substitutions

def perform_substitutions(
        input: list[str],
        substitutions: dict[str, Union[int,bool,str,None]],
        copy_only: bool,
        at_replacements: bool,
        var_replacements: bool,
        embeddings: list[str]) -> list[str]:
    
    resolved_embeddings = resolve_all_embeddings(embeddings)

    formatted = []
    substitutions.update(resolved_embeddings)
    for line in input:
        line = line.rstrip("\r\n")

        if not copy_only:
            if at_replacements:
                line = substitute_at_replace(line, substitutions)

            if var_replacements:
                line = substitute_variable_replace(line, substitutions)

            line = substitute_cmakedefine(line, substitutions)
            line = substitute_cmakedefine01(line, substitutions)
            
        formatted.append(line + "\n")
    return formatted

def get_substitution_value(key:str, substitutions: dict[str, Union[int, str, bool, None]], default=None):
    substitutions_encountered_in_template.add(key)
    return substitutions.get(key, default)

def substitute_at_replace(line: str, substitutions: dict[str, Union[int, str, bool, None]]) -> str:
    return re.sub(at_replace, lambda m: __at_replace_impl(m, substitutions), line)

def __at_replace_impl(match: re.Match[str], substitutions: dict[str, Union[int, str, bool, None]]) -> str:
    key = match.group(1).strip("@")

    value = get_substitution_value(key, substitutions, "")
    if value:
        return str(value)
    return ""

 
def substitute_variable_replace(line: str, substitutions: dict[str, Union[int, str, bool, None]]) -> str:
    return re.sub(variable_replace, lambda m: __variable_replace_impl(m, substitutions), line)

def __variable_replace_impl(match: re.Match[str], substitutions: dict[str, Union[int, str, bool, None]]) -> str:
    key = match.group(1)
    key = key[2:-1]

    value = get_substitution_value(key, substitutions)
    if value:
        return str(value)
    return ""

def substitute_cmakedefine(line: str, substitutions: dict[str, Union[int, str, bool, None]]) -> str:
    return re.sub(regex_cmakedefine, lambda m: __cmakedefine_impl(m, substitutions), line).rstrip()

def _is_value_false_like(value:Union[str,int,bool,None]) -> bool:
    if not value:
        return True
    if isinstance(value, str) and value.lower() in ['0', 'false', 'off', '']:
        return True
    return False

def __cmakedefine_impl(match: re.Match[str], substitutions: dict[str, Union[int, str, bool, None]]) -> str:
    groups = match.groups()

    # Support both of the following forms:
    #    #cmakedefine KEY
    #    #cmakedefine KEY VALUE
    key = groups[0]
    substitute_value = None
    if len(groups) >= 2:
        substitute_value = groups[1]

    template_value = get_substitution_value(key, substitutions)
    
    if _is_value_false_like(template_value):
        return f"/* #undef {key} */"
    
    result = f"#define {key}"
    if substitute_value:
        result += f" {substitute_value}"
    return result

def substitute_cmakedefine01(line: str, substitutions: dict[str, Union[int, str, bool, None]]) -> str:
    return re.sub(cmakedefine01, lambda m: __cmakedefine01_impl(m, substitutions), line)

def __cmakedefine01_impl(match: re.Match[str], substitutions: dict[str, Union[int, str, bool, None]]) -> str:
    key = match.group(1)
    value = get_substitution_value(key, substitutions)
    if _is_value_false_like(value):
        return f"#define {key} 0"
    else:
        return f"#define {key} 1"

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("-i", "--input", required=True)
    parser.add_argument("-o", "--output", required=True)
    parser.add_argument(
        "--strict",
        action='store_true',
        required=False,
        default=False,
        help="Fail on missing & unused substitutions",
    )
    parser.add_argument(
        "-s",
        "--substitutions",
        required=False,
        default=[],
        help="Substitutions as key=value pairs to make in the template file",
        nargs="*",
    )
    parser.add_argument(
        "-e",
        "--embeddings",
        required=False,
        default=[],
        help="File substitutions as json",
        nargs="*"
    )

    parser.add_argument(
        "--substitution-file",
        required=False,
        default=None,
        help="File containing key=value pairs to make in the template file",
    )
    parser.add_argument(
        "--enable-at-replacements",
        required=False,
        default=False,
        action="store_true",
        help="Perform @var@ substitution",
    )
    parser.add_argument(
        "--enable-var-replacements",
        required=False,
        default=False,
        action="store_true",
        help="Perform ${var} substitution",
    )
    parser.add_argument(
        "--escape-quotes",
        required=False,
        default=False,
        action="store_true",
        help="Escape any substituted quotes with backslashes (C-style).",
    )
    parser.add_argument(
        "--copy-only",
        required=False,
        default=False,
        action="store_true",
        help="Only copy the input file to the output file",
    )
    parser.add_argument(
        "--log-level",
        default="WARNING",
        choices=["DEBUG", "INFO", "WARNING", "ERROR", "CRITICAL"],
        help="Configure the logging level.",
    )
    
    args = parser.parse_args()
    logging.getLogger().setLevel(args.log_level)

    with open(args.input) as f:
        template = f.readlines()

    if args.substitution_file:
        substitutions = read_substitutions(args.substitution_file)
    else:
        substitutions = prepare_substitutions(args.substitutions, escape_quotes=args.escape_quotes)

    formatted = perform_substitutions(
        template, 
        substitutions=substitutions,
        at_replacements=args.enable_at_replacements,
        var_replacements=args.enable_var_replacements,
        embeddings=args.embeddings,
        copy_only=args.copy_only)
    
    if args.strict:
        unused_substitutions = set(substitutions.keys()).difference(substitutions_encountered_in_template)
        missing_substitutions = substitutions_encountered_in_template.difference(substitutions.keys())

        fail = False
        if unused_substitutions:
            fail = True
            print(f"Found {len(unused_substitutions)} unused substitutions:")
            for s in unused_substitutions:
                print(f"    * {s}")

        if missing_substitutions:
            fail = True
            print(f"Encountered {len(missing_substitutions)} substitutions in the template file with no corresponding replacement given:")
            for s in missing_substitutions:
                print(f"    * {s}")

        if fail:
            sys.exit(1)

    if args.output is not None:
        output = Path(args.output)
        output.parent.mkdir(parents=True, exist_ok=True)
        with open(output, "w") as f:
            f.writelines(formatted)
