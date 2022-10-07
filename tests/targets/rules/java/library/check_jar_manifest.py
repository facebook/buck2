import sys
import zipfile

with zipfile.ZipFile(sys.argv[1], "r") as jar_file:
    """verify that jar has manifest with passed lines"""

    expected_lines = set(sys.argv[2:])

    lines = []
    manifest_found = False
    for zip_info in jar_file.infolist():
        file_name = zip_info.filename
        if file_name == "META-INF/MANIFEST.MF":
            manifest_found = True
            for line in jar_file.read(file_name).split(b"\r\n"):
                decoded_line = line.decode("utf-8")
                if decoded_line:
                    lines.append(decoded_line)
            print(lines)

    if not manifest_found:
        raise Exception("No manifest file inside {}".format(jar_file.filename))

    lines_to_find = len(expected_lines)
    found = 0
    for line in lines:
        if line in expected_lines:
            found += 1

    if found < lines_to_find:
        raise Exception(
            "Expected to find `{}` lines inside `{}`, but found `{}`.\n\t\tLines: {}.\n\t\tExpected lines {}".format(
                lines_to_find,
                jar_file.filename,
                found,
                sorted(lines),
                sorted(expected_lines),
            )
        )
