# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


def __invoke_main() -> None:
    import os
    import sys

    # Remove PYTHONHOME set by the bootstrap script which we use in order to make
    # sure the bundled runtime uses the correct standard library. We are safe
    # to remove it once we have initialized the runtime.
    # leaving it may cause errors if the user code calls a subprocess that
    # expects to be using the system python distribution eg: buck
    os.environ.pop("PYTHONHOME", None)

    module = os.getenv("FB_PAR_MAIN_MODULE")
    main_function = os.getenv("FB_PAR_MAIN_FUNCTION")

    sys.argv[0] = os.getenv("FB_LPAR_INVOKED_NAME", sys.argv[0])
    del sys.path[0]

    main_runner_module = os.environ["FB_PAR_MAIN_RUNNER_MODULE"]
    main_runner_function = os.environ["FB_PAR_MAIN_RUNNER_FUNCTION"]

    from importlib import import_module

    mod = import_module(main_runner_module)
    run_as_main = getattr(mod, main_runner_function)
    run_as_main(module, main_function)


__invoke_main()
