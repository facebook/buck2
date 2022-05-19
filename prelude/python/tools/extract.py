#!/usr/bin/env python3

"""
Quick and dirty wrapper to extract zip files; python 3.6.2+

extract.py my_zip_file.zip --output=output_directory
"""

import argparse
import shutil
from pathlib import Path

if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Extract .zip files to a directory in a cross platform manner"
    )
    parser.add_argument(
        "--output", type=Path, required=True, help="The directory to write to"
    )
    parser.add_argument("src", type=Path, help="The archive to extract to --output")
    args = parser.parse_args()

    args.output.mkdir(parents=True, exist_ok=True)
    shutil.unpack_archive(args.src, args.output, "zip")
