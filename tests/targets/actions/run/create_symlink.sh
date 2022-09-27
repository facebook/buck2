#!/bin/sh

# create relative symlink to `create_symlink.sh`
script_path_relative_to_symlink="$(realpath --relative-to="$(dirname "$1")" "$0")"
ln -s "$script_path_relative_to_symlink" "$1"
