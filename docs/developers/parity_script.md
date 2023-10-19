---
id: parity_script
title: Parity Testing
---

This page covers the parity testing/command replay script found in
`scripts/buck_replay/main.py`.

## Overview

The `buck_replay` script is meant to test parity between v1 and v2
implementations of commands by querying for logs of the repo and execution state
(args, directory) of v2 command invocations, reproducing it locally, making the
necessary conversions from v2 to v1 args, and then running both versions of the
command so output can be checked/compared.

When output differs/parity testing fails, the results are logged into a Scuba
table for future reference/analysis.

## Flags

The following is a list of arguments/flags currently supported by the list:

- `--verbose` - supplying this turns on debug logging. By default, the replay
  script logs updates on script progress and any errors that happen. When
  `--verbose` is given, debug logging will also provide updates on commit and
  directory changes while parity testing.
- `--dry-run` - toggles logging to a test Scuba table instead of the production
  one. Useful if you're making edits/testing the script itself.
- `--epoch` - the time after which to query Scuba for logs of commands for, as a
  Unix timestamp. If not supplied, it defaults to the last 24 hours.
- `--limit` - limits the number of rows queried from Scuba. The default limit is
  100000 rows.

## Running the script

The script can be run with buck:

```shell
buck run //buck2/scripts/buck_replay:buck_replay
```

Example with flags:

```shell
buck run //buck2/scripts/buck_replay:buck_replay -- --verbose --dry-run --epoch 1626739329 --limit 100000
```

## Development

The script does not yet support commands beyond `audit config`. Because of
differences in flags (different names, new/dropped flags, and so on) in v1 and
v2 implementations of commands, there needs to be some conversion when going
from one set of arguments to the other. As such, support for a command requires
the implementation of a `Command` class for that command, and with it, several
methods:

- `format_common_args`, `format_args_v1`, `format_args_v2` - to format the
  flags/arguments in common between the v1 and v2 versions of a command as well
  as the ones specific to v1 and v2, respectively.
- `run_v1` and `run_v2` - meant to run the v1 and v2 commands and capture the
  relevant output.
- `test_parity` - meant to compare the output the v1 and v2 outputs (note that
  the standard for what's "equal" may change between commands) and log whatever
  is necessary.

You can also work on features surrounding the replay script; specifically,
adding logging to more commands (since in v2 only `audit config` logging is
supported) and Ingress tailer support (currently command logging is handled by
`CommandReporterProcessor`).
