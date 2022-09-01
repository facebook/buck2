load("@prelude//utils:utils.bzl", "expect")

_ROOT_SYMBOL = "//"
_TARGET_SYMBOL = ":"
_RECURSIVE_SYMBOL = "..."
_PATH_SYMBOL = "/"

_BuildTargetPatternKind = enum(
    "single",
    "package",
    "recursive",
)

BuildTargetPattern = record(
    kind = field(_BuildTargetPatternKind.type),
    cell = field([str.type, None], None),
    path = field(str.type),
    name = field([str.type, None], None),
)

def parse_build_target_pattern(pattern: str.type) -> BuildTargetPattern.type:
    expect(len(pattern) >= len(_ROOT_SYMBOL) + 1, "Invalid build target pattern, pattern too short: {}".format(pattern))

    root_position = pattern.find(_ROOT_SYMBOL)
    expect(root_position >= 0, "Invalid build target pattern, pattern should started with `{}` or a cell name followed by `{}`: ".format(_ROOT_SYMBOL, _ROOT_SYMBOL, pattern))

    cell = None
    if root_position > 0:
        cell = pattern[0:root_position]

    name = None
    end_of_path_position = -1
    if pattern.endswith(_TARGET_SYMBOL):
        kind = _BuildTargetPatternKind("package")
        end_of_path_position = len(pattern) - 1
    elif pattern.endswith(_RECURSIVE_SYMBOL):
        kind = _BuildTargetPatternKind("recursive")
        end_of_path_position = len(pattern) - len(_RECURSIVE_SYMBOL) - 1
        expect(pattern[end_of_path_position] == _PATH_SYMBOL, "Invalid build target pattern, `{}` should be preceded by a `{}`: {}".format(_RECURSIVE_SYMBOL, _PATH_SYMBOL, pattern))
    else:
        kind = _BuildTargetPatternKind("single")
        end_of_path_position = pattern.rfind(_TARGET_SYMBOL)
        if (end_of_path_position < 0):
            # Pattern does not have a target delimiter and thus a target name
            # Assume target name to be the same as the last component of the package
            end_of_path_position = len(pattern)
            start_of_package = pattern.rfind(_PATH_SYMBOL)
            name = pattern[start_of_package + len(_PATH_SYMBOL):]
        elif end_of_path_position < root_position:
            fail("Invalid build target pattern, cell name should not contain `{}`: {}".format(_PATH_SYMBOL, pattern))
        else:
            name = pattern[end_of_path_position + len(_TARGET_SYMBOL):]

    start_of_path_position = root_position + len(_ROOT_SYMBOL)

    expect(pattern[start_of_path_position] != _PATH_SYMBOL, "Invalid build target pattern, path cannot start with `{}`: {}".format(_PATH_SYMBOL, pattern))

    path = pattern[start_of_path_position:end_of_path_position]
    expect(path.find(_ROOT_SYMBOL) < 0, "Invalid build target pattern, `{}` can only appear once: {}".format(_ROOT_SYMBOL, pattern))
    expect(path.find(_RECURSIVE_SYMBOL) < 0, "Invalid build target pattern, `{}` can only appear once: {}".format(_RECURSIVE_SYMBOL, pattern))
    expect(path.find(_TARGET_SYMBOL) < 0, "Invalid build target pattern, `{}` can only appear once: {}".format(_TARGET_SYMBOL, pattern))
    expect(len(path) == 0 or path[-1:] != _PATH_SYMBOL, "Invalid build target pattern, path cannot end with `{}`: {}".format(_PATH_SYMBOL, pattern))

    return BuildTargetPattern(kind = kind, cell = cell, path = path, name = name)

def label_matches_build_target_pattern(label: "label", pattern: BuildTargetPattern.type) -> bool.type:
    if pattern.cell and pattern.cell != label.cell:
        return False

    if pattern.kind == _BuildTargetPatternKind("single"):
        return pattern.path == label.package and pattern.name == label.name
    elif pattern.kind == _BuildTargetPatternKind("package"):
        return pattern.path == label.package
    elif pattern.kind == _BuildTargetPatternKind("recursive"):
        if len(label.package) > len(pattern.path):
            return label.package.startswith(pattern.path + _PATH_SYMBOL)
        else:
            return pattern.path == label.package
    else:
        fail("Unknown build target pattern kind.")
