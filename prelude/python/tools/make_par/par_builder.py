# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-ignore-all-errors

import os
import tempfile


class ParBuilder:
    """
    Asbtract class that represents the process of build a .par file
    """

    def __init__(self, options, manifest, mode=None, warnings=None):
        self.options = options  # lets not pass this around anymore
        self.manifest = manifest
        self.output = options.output
        self.strict_tabs = options.strict_tabs
        self.warnings = options.warnings
        self.mode = mode
        self.runtime_env = options.runtime_env
        self.ld_preload = options.ld_preload
        self.exe = None

        self.python = options.python
        self.python_home = options.python_home or None
        self.entry_point = options.runtime_binary or None

    def build(self):
        self._gen_file()
        self._postbuild()

    def _gen_file(self):
        (dirname, basename) = os.path.split(self.output)
        prefix = basename + ".tmp."
        with tempfile.TemporaryDirectory(dir=dirname, prefix=prefix) as tempdir:
            output_filename = os.path.join(tempdir, "output")
            with open(output_filename, "w+b") as output_file:
                # Call the function to generate the file contents
                self._gen_contents(output_file)

            # Rename the temporary output file to the real destination
            os.rename(output_filename, self.output)
            if self.mode:
                os.chmod(self.output, self.mode)

    def _gen_contents(self):
        raise NotImplementedError

    def _postbuild(self):
        raise NotImplementedError

    def _get_python_command(self, shebang=False, interp=None, base_dir=None):
        if self.entry_point:
            if not os.path.isabs(self.entry_point) and base_dir:
                cmd = [f"{base_dir}/{self.entry_point}"]
            else:
                cmd = [self.entry_point]
        elif interp:
            cmd = [interp]
        else:
            if shebang and not os.path.isabs(self.python):
                # If shebang is true, the command will be used as the #! line in a
                # script.  Unfortunately, the #! line can only parse at most 2
                # arguments, so if we need to use env we can't use any other
                # arguments.  Therefore we ignore strict_tabs in this case.
                #
                # We also need to specify an absolute path to env in this case.
                return "/usr/bin/env " + self.python

            cmd = [self.python]
            if not os.path.isabs(self.python):
                cmd.insert(0, "env")

        flags = []
        if self.strict_tabs:
            flags.append("tt")
        if self.warnings is not None:
            flags.append(f"W{self.warnings}")
        if flags:
            cmd.append("-" + "".join(flags))

        return " ".join(arg for arg in cmd)
