import sys
import zipfile

# Verify that jar has passed files.
with zipfile.ZipFile(sys.argv[1], "r") as jar_file:
    """verify that files with expected files and prints out every non .class file from jar_file"""

    expected_files = set(sys.argv[2:])
    params_count = len(expected_files)
    files = []

    found_count = 0
    for zip_info in jar_file.infolist():
        file_name = zip_info.filename
        if not file_name.endswith(".class"):
            print(file_name)
            files.append(file_name)

        if file_name in expected_files:
            found_count = found_count + 1

    if found_count != params_count:
        raise Exception(
            "Expected to find `{}` files inside {}, but found {}.\n\t\tResources: {}.\n\t\tExpected files {}".format(
                params_count, jar_file.filename, found_count, files, expected_files
            )
        )
