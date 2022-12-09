import argparse
import os
import shutil
import subprocess


CONAN_DIR = ".conan"


def conan_dir(user_home):
    """Conan folder under the Conen user home."""
    return os.path.join(user_home, CONAN_DIR)


def conan_env(
        user_home=None,
        trace_log=None):
    """Generate environment variables needed to invoke Conan."""
    env = dict(os.environ)

    if user_home is not None:
        # Set the Conan base directory.
        env["CONAN_USER_HOME"] = os.path.abspath(user_home)

    if trace_log is not None:
        # Enable Conan debug trace.
        env["CONAN_TRACE_FILE"] = os.path.abspath(trace_log)

    # TODO[AH] Enable Conan revisions for reproducibility
    # Enable Conan revisions for reproducibility
    #env["CONAN_REVISIONS_ENABLED"] = "1"

    # Prevent over-allocation.
    env["CONAN_CPU_COUNT"] = "1"

    # Prevent interactive prompts.
    env["CONAN_NON_INTERACTIVE"] = "1"

    # Print every `self.run` invokation.
    # TODO[AH] Remove this debug output.
    env["CONAN_PRINT_RUN_COMMANDS"] = "1"

    # Disable the short paths feature on Windows.
    # TODO[AH] Enable if needed with a hermetic short path.
    env["CONAN_USER_HOME_SHORT"] = "None"

    return env


def run_conan(conan, *args, env={}):
    return subprocess.check_call([conan] + list(args), env=env)
