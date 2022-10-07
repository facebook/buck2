# This command runs slow on both local and RE, but it runs slower locally. The
# upshot is that local should start before RE is done, but RE will finish
# first, and then will cancel local. If it doesn't cancel local, the test will
# fail.

import pathlib
import sys
import time

re_worker_path = "/run/re_worker/beacon"
if pathlib.Path(re_worker_path).exists():
    time.sleep(5)
    out = sys.argv[1]
    pathlib.Path(out).touch()
    sys.exit(0)

time.sleep(10)
sys.exit(1)
