# Example:
#
#     things = set()
#     for x in somewhere:
#         things.insert(x)
#     return things.list()
#
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
def set():
    self = None

    def set_insert(v):
        self.entries[v] = None

    def set_list():
        return self.entries.keys()

    self = struct(
        entries = {},
        insert = set_insert,
        list = set_list,
    )

    return self
