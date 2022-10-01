# Utilities for checking and ignoring types

# Ignores the type, always returning "" (the wildcard type).
# Used where the type is true, but performance concerns preclude the type in normal operation.
#
# FIXME: Probably have a way to unconditionally enable such types, to ensure they remain accurate.
def unchecked(_):
    return ""

# Assert that a given value has a specific type, and return that value.
# Fails at runtime if the value does not have the right type.
def cast(value, type):
    def inner(_: type):
        pass

    inner(value)
    return value
