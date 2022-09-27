import os
import subprocess
import sys

# Verify that archive metadata is deterministic.
with open(sys.argv[1], mode="rb") as f:
    header = f.read(8)
    assert header in (b"!<arch>\n", b"!<thin>\n")

    # Iterate over entries, verifying that timestamps, perms, and mode are
    # zero'd out.
    while True:
        name = f.read(16).strip()  # name
        if len(name) == 0:
            break
        # Skip special extended name section.
        if name == b"//":
            break
        timestamp = int(f.read(12).decode("utf-8").strip())  # timestamp
        owner = int(f.read(6).decode("utf-8").strip())  # owner ID
        group = int(f.read(6).decode("utf-8").strip())  # group ID
        mode = int(f.read(8).decode("utf-8").strip(), 8)  # mode
        size = int(f.read(10))  # size
        f.read(2)  # ending
        f.seek(size, os.SEEK_CUR)  # skip over file data
        if (mode, owner, group, timestamp) != (0, 0, 0, 0):
            raise Exception(
                "Unexpected non-deterministic info: {}".format(
                    (name, mode, owner, group, timestamp)
                )
            )
