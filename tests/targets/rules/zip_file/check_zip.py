import sys
import zipfile

# Verify zip file.
with zipfile.ZipFile(sys.argv[1], "r") as zip_file:
    """verify and prints out all files from zip_file"""

    expected_entries = list(sys.argv[2:]) if len(sys.argv) > 1 else []
    expected_count = len(expected_entries)
    files = []

    entries_found = False
    found_count = 0
    for zip_info in zip_file.infolist():
        file_name = zip_info.filename
        print(file_name)
        files.append(file_name)

        if file_name in expected_entries:
            found_count = found_count + 1

    if found_count != expected_count:
        raise Exception(
            "Expected to find `{}` files inside {}, but found {}.\n\nEntries: {}.\n\t\tExpected entries {}".format(
                expected_count, zip_file.filename, found_count, files, expected_entries
            )
        )

    if expected_count > 0 and len(files) != found_count:
        raise Exception(
            "Count of entries are different from count of the expected entries.\n Expected `{}`, but there are {} entries inside '{}'.\n\tEntries: {}.\n\t\tExpected entries {}".format(
                expected_count, len(files), zip_file.filename, files, expected_entries
            )
        )
