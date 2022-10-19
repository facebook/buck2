# A set is useful when the `dedupe` builtin is not applicable. Dedupe looks at
# identity of the value (some kind of pointer) rather than equality, so for
# example doesn't eliminate duplicates of the same string value obtained from
# different places:
#
#     things = ["huh", "huh"]
#     expect(len(dedupe(things)) == 2)
#
#     huh = "huh"
#     things = [huh, huh]
#     expect(len(dedupe(things)) == 1)
#
# In contrast a set compares its entries for equality, not identity, and will
# never contain one entry equal to another entry.
#
# Example usage:
#
#     things = set()
#     for x in somewhere:
#         things.add(x)
#     return things.list()

# Name the record `set_record` to enable users to use `set` to intialize a set.
set_record = record(
    _entries = field({"": None}, {}),
    list = field("function", lambda _: None),
    # Adds the value to the set, returning whether the value existed in the set
    add = field("function", lambda _value: bool.type),
    # Removes the value if the value is in the set, returning whether the value existed in the set
    remove = field("function", lambda _value: bool.type),
    # Adds the values to the set, returning the values that were added
    update = field("function", lambda _values: [""]),
    # Returns whether the value is in the set
    contains = field("function", lambda _value: bool.type),
    size = field("function", lambda _ = None: int.type),
)

# For typing a set, you may use `set_type` or `set_record.type`, the former is
# encouraged to avoid leaking the underlying implementation.
set_type = set_record.type

def set():
    self = None

    def set_list():
        return self._entries.keys()

    def set_add(v: "") -> bool.type:
        if self.contains(v):
            return True
        self._entries[v] = None
        return False

    def set_contains(v: "") -> bool.type:
        return v in self._entries

    def set_remove(v: "") -> bool.type:
        if self.contains(v):
            self._entries.pop(v)
            return True
        return False

    def set_update(values: [""]) -> [""]:
        return filter(None, [v for v in values if not self.add(v)])

    def set_size() -> int.type:
        return len(self._entries)

    self = set_record(
        _entries = {},
        list = set_list,
        add = set_add,
        remove = set_remove,
        update = set_update,
        contains = set_contains,
        size = set_size,
    )

    return self
