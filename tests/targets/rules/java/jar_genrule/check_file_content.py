import sys

file_to_check = sys.argv[1]
expected_content = sys.argv[2]

with open(file_to_check, "r") as file:
    content = file.read().strip()
    if content != expected_content:
        raise Exception(
            "Read content: {}\n\n Expected content: {}\n\n File: {}\n".format(
                content, expected_content, file_to_check
            )
        )
