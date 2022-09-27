import sys
import zipfile

# Verify zip file's entry content.
with zipfile.ZipFile(sys.argv[1], "r") as zip_file:
    """verifies zip file's entry content"""

    entry_to_check = sys.argv[2]
    expected_content = sys.argv[3]

    with zip_file.open(entry_to_check, "r") as entry_file:
        content = entry_file.read().decode("UTF-8").strip()
        if content != expected_content:
            raise Exception(
                "Extracted content: {}\n\n Expected content: {}\n\n File: {}\n".format(
                    content, expected_content, zip_file.filename
                )
            )
