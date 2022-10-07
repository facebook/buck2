# (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

# @nolint

"""Provides macros for queries type information."""

_SELECT_TYPE = type(select({"DEFAULT": []}))

def is_select(thing):
    return type(thing) == _SELECT_TYPE

def is_unicode(arg):
    """Checks if provided instance has a unicode type.

    Args:
      arg: An instance to check. type: Any

    Returns:
      True for unicode instances, False otherwise. rtype: bool
    """
    return hasattr(arg, "encode")

_STRING_TYPE = type("")

def is_string(arg):
    """Checks if provided instance has a string type.

    Args:
      arg: An instance to check. type: Any

    Returns:
      True for string instances, False otherwise. rtype: bool
    """
    return type(arg) == _STRING_TYPE

_LIST_TYPE = type([])

def is_list(arg):
    """Checks if provided instance has a list type.

    Args:
      arg: An instance to check. type: Any

    Returns:
      True for list instances, False otherwise. rtype: bool
    """
    return type(arg) == _LIST_TYPE

_DICT_TYPE = type({})

def is_dict(arg):
    """Checks if provided instance has a dict type.

    Args:
      arg: An instance to check. type: Any

    Returns:
      True for dict instances, False otherwise. rtype: bool
    """
    return type(arg) == _DICT_TYPE

_TUPLE_TYPE = type(())

def is_tuple(arg):
    """Checks if provided instance has a tuple type.

    Args:
      arg: An instance to check. type: Any

    Returns:
      True for tuple instances, False otherwise. rtype: bool
    """
    return type(arg) == _TUPLE_TYPE

_BOOL_TYPE = type(True)

def is_bool(arg):
    """Checks if provided instance is a boolean value.

    Args:
      arg: An instance ot check. type: Any

    Returns:
      True for boolean values, False otherwise. rtype: bool
    """
    return type(arg) == _BOOL_TYPE

_NUMBER_TYPE = type(1)

def is_number(arg):
    """Checks if provided instance is a number value.

    Args:
      arg: An instance ot check. type: Any

    Returns:
      True for number values, False otherwise. rtype: bool
    """
    return type(arg) == _NUMBER_TYPE

type_utils = struct(
    is_bool = is_bool,
    is_number = is_number,
    is_string = is_string,
    is_unicode = is_unicode,
    is_list = is_list,
    is_dict = is_dict,
    is_tuple = is_tuple,
    is_select = is_select,
)
