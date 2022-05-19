PREFIX_MARKER = "^"
SUFFIX_MARKER = "^"
REGEX_MARKER = "^-"

ClassNameFilter = record(
    prefixes = [str.type],
    suffixes = [str.type],
    substrings = [str.type],
    exact_matches = [str.type],
    regular_expressions = [str.type],
)

def get_class_name_filter(patterns: [str.type]) -> "ClassNameFilter":
    prefixes = []
    suffixes = []
    substrings = []
    exact_matches = []
    regular_expressions = []

    for pattern in patterns:
        if pattern.startswith(REGEX_MARKER):
            regular_expressions.append(pattern[2:])
        else:
            is_prefix = pattern[0] == PREFIX_MARKER
            is_suffix = pattern[-1] == SUFFIX_MARKER
            if is_prefix and is_suffix:
                exact_matches.append(pattern[1:-1])
            elif is_prefix:
                prefixes.append(pattern[1:])
            elif is_suffix:
                suffixes.append(pattern[:-1])
            else:
                substrings.append(pattern)

    return ClassNameFilter(
        prefixes = prefixes,
        suffixes = suffixes,
        substrings = substrings,
        exact_matches = exact_matches,
        regular_expressions = regular_expressions,
    )

def class_name_matches_filter(class_name: str.type, class_name_filter: "ClassNameFilter") -> bool.type:
    if class_name in class_name_filter.exact_matches:
        return True

    for prefix in class_name_filter.prefixes:
        if class_name.startswith(prefix):
            return True

    for suffix in class_name_filter.suffixes:
        if class_name.endswith(suffix):
            return True

    for substring in class_name_filter.substrings:
        if substring in class_name:
            return True

    for regular_expression in class_name_filter.regular_expressions:
        if regex_match(regular_expression, class_name):
            return True

    return False
