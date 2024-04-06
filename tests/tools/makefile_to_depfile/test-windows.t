  $ $DEP_FILE_PROCESSOR "makefile" "$TESTDIR/fixtures/windows.mk" "$CRAMTMP/windows" true
  $ cat "$CRAMTMP/windows"
  foo/a
  bar//b
  baz
