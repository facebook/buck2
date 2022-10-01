import sys

file_to_check = sys.argv[1]
expected_patterns = set(sys.argv[2:])

with open(file_to_check, "r") as file:
    content = file.read().strip()
    for expected_pattern in expected_patterns:
        if expected_pattern not in content:
            raise Exception(
                "Read content: {}\n\n Doesn't contain expected pattern: {}\n\n File: {}\n".format(
                    content, expected_pattern, file_to_check
                )
            )
