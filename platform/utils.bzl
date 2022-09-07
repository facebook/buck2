load(
    "@fbsource//tools/build_defs:buckconfig.bzl",
    _build_defs_read_bool = "read_bool",
    _build_defs_read_int = "read_int",
    _build_defs_read_list = "read_list",
    _build_defs_read_string = "read",
)

def read_list(section, key, delimiter = " "):
    return _build_defs_read_list(section, key, delimiter = delimiter, required = False)

def read_list_with_comma_as_delimiter(section, key):
    return read_list(section, key, delimiter = ",")

def read_bool(section, key, default = None):
    return _build_defs_read_bool(section, key, default = default, required = False)

def read_string(section, key):
    return _build_defs_read_string(section, key)

def read_optional_list(section, key):
    return _build_defs_read_list(section, key, delimiter = " ", required = False)

def read_optional_bool(section, key):
    return _build_defs_read_bool(section, key, required = False)

def read_int(section, key):
    return _build_defs_read_int(section, key, default = None, required = True)

def read_optional_int(section, key):
    return _build_defs_read_int(section, key, default = None, required = False)

attr_info = record(reader = "function", attr_type = "attribute")

bool_attr = attr_info(reader = read_bool, attr_type = attrs.bool())
optional_bool_attr = attr_info(reader = read_optional_bool, attr_type = attrs.option(attrs.bool()))

string_attr = attr_info(reader = read_string, attr_type = attrs.string())
optional_string_attr = attr_info(reader = read_string, attr_type = attrs.option(attrs.string()))
string_list_attr = attr_info(reader = read_list_with_comma_as_delimiter, attr_type = attrs.list(attrs.string()))

binary_attr = attr_info(reader = read_string, attr_type = attrs.dep(providers = [RunInfo]))
optional_binary_attr = attr_info(reader = read_string, attr_type = attrs.option(attrs.dep(providers = [RunInfo])))

binary_exec_attr = attr_info(reader = read_string, attr_type = attrs.exec_dep(providers = [RunInfo]))
optional_binary_exec_attr = attr_info(reader = read_string, attr_type = attrs.option(attrs.exec_dep(providers = [RunInfo])))

optional_binary_or_source_attr = attr_info(reader = read_string, attr_type = attrs.option(attrs.one_of(attrs.dep(), attrs.source())))
source_list_attr = attr_info(reader = read_list, attr_type = attrs.list(attrs.source()))

flags_attr = attr_info(reader = read_list, attr_type = attrs.list(attrs.arg()))
optional_flags_attr = attr_info(reader = read_optional_list, attr_type = attrs.option(attrs.list(attrs.arg())))

int_attr = attr_info(reader = read_int, attr_type = attrs.int())
optional_int_attr = attr_info(reader = read_optional_int, attr_type = attrs.option(attrs.int()))
