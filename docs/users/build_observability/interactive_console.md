---
id: interactive_console
title: Buck2 Interactive Console
---

This will work as long as stdin is a TTY, which will be true most of the time if
you're not piping anything into Buck2.

To see what's available you can press `?`.

To disable to allow alternate use of stdin, or for follow up pasted commands to
not get swallowed:

Environment Variable: `BUCK_NO_INTERACTIVE_CONSOLE` or flag:
`--no-interactive-console`

Note: Not available yet for Windows
