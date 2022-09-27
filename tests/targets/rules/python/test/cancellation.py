import os
import time
import unittest


class TestCase(unittest.TestCase):
    def test(self):
        # Require using --env to run this.
        duration = int(os.environ["SLOW_DURATION"])
        with open(os.path.join(os.environ["PIDS"], str(os.getpid())), "w"):
            pass
        time.sleep(duration)
