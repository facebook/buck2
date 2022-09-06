import os
import subprocess
import sys
from pathlib import Path

# Runs passed shell script or jar (fat jar main class + shell script + native libraries)
directory = Path(os.getcwd())
while os.path.basename(directory) != "buck-out":
    directory = directory.parent
project_root = directory.parent.absolute()

java_binary = os.path.realpath(sys.argv[1])
is_script = java_binary.endswith(".sh")
if is_script:
    run_java_binary_command = ["/bin/bash", java_binary]
else:
    run_java_binary_command = [
        "/usr/local/java-runtime/impl/11/bin/java",
        "-jar",
        java_binary,
    ]
subprocess.check_call(run_java_binary_command, cwd=project_root)
