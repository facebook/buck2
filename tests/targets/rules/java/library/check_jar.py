import os
import sys
import zipfile

# Verify that jar has at least one compiled java file inside.
with zipfile.ZipFile(sys.argv[1], "r") as jar_file:
    """verify and prints out .class files from jar_file"""

    expected_classes = set(sys.argv[2:]) if len(sys.argv) > 1 else []
    expected_count = len(expected_classes)
    files = []

    class_files_found = False
    found_count = 0
    for zip_info in jar_file.infolist():
        file_name = zip_info.filename
        if file_name.endswith(".java"):
            raise Exception(
                "Did not expect to find source file in jar!\n{}".format(file_name)
            )
        if file_name.endswith(".class"):
            class_files_found = True
            print(file_name)
            files.append(file_name)

        if file_name.replace(os.sep, ".").replace(".class", "") in expected_classes:
            found_count = found_count + 1

    if not class_files_found:
        raise Exception(
            "Not found any *.class files inside {}".format(jar_file.filename)
        )

    if found_count != expected_count:
        raise Exception(
            "Expected to find `{}` files inside {}, but found {}.\n\tClasses: {}.\n\t\tExpected classes {}".format(
                expected_count, jar_file.filename, found_count, files, expected_classes
            )
        )

    if expected_count > 0 and len(files) != found_count:
        raise Exception(
            "Count of classes are different from count of the expected classes.\n Expected `{}`, but there are {} classes inside '{}'.\n\tClasses: {}.\n\t\tExpected classes {}".format(
                expected_count, len(files), jar_file.filename, files, expected_classes
            )
        )
