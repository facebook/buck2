#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

set -ueo pipefail

version=${1:-3.13}
release=${2:-}
REPO="astral-sh/python-build-standalone"

if [ -z "$release" ]; then
    release=$(gh release -R "$REPO" list --limit 1 | awk '{print $1}')
fi

URL_PREFIX="https://github.com/$REPO/releases/download/$release"

input=$(gh release -R "$REPO" download "$release" --pattern SHA256SUMS --output -)


function get_entry() {
    local version=$1
    local arch=$2
    local platform=$3

    local match_count
    local pat
    pat="cpython-$version.*$arch.*$platform.*install_only_stripped.tar.gz"
    match_count=$(echo "$input" | grep -c "$pat")
    if [ "$match_count" -eq 0 ]; then
        echo "No entry found for version $version, arch $arch, platform $platform" >&2
        exit 1
    fi

    match=$(echo "$input" | grep "$pat")

    if [[ "$match_count" -gt 1 ]]; then
        echo "Multiple entries ($match_count) found for version $version, arch $arch, platform $platform:" >&2
        echo "$match" >&2
        exit 2
    fi

    echo "$match"
}

output="{\n"
for platform in linux macos windows; do
    case "$platform" in
        linux)
            archive_platform="unknown-linux-gnu"
            ;;
        macos)
            archive_platform="apple-darwin"
            ;;
        windows)
            archive_platform="pc-windows-msvc"
            ;;
    esac
    output+="    \"$platform\": {\n"
    for arch in arm64 x86_64; do
        if [ "$platform" = "linux" ] && [ "$arch" = "x86_64" ]; then
             # lowest arch level by default
            # change this to x86_64_2 or 3 or 4 if you don't care about compatibility
            archive_arch="x86_64-"
        elif [ "$arch" = "arm64" ]; then
            archive_arch="aarch64"
        else
            archive_arch="$arch"
        fi
        entry=$(get_entry "$version" "$archive_arch" "$archive_platform")
        url="$URL_PREFIX/"$(awk '{print $2}' <<< "$entry")
        sha256=$(awk '{print $1}' <<< "$entry")
        output+="        \"$arch\": {\"sha256\": \"$sha256\", \"url\": \"$url\"},\n"
    done
    output+="    },\n"
done
output+="}"

printf "%b\n" "$output"
