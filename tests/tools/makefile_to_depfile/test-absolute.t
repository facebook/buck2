  $ sed "s:__ROOT__:$(pwd):g" < "$TESTDIR/fixtures/absolute.mk" > "$CRAMTMP/absolute.mk"
  $ $DEP_FILE_PROCESSOR "makefile" "$CRAMTMP/absolute.mk" "$CRAMTMP/absolute" true
  $ cat "$CRAMTMP/absolute"
  foo/bar
  foo
  bar
  baz
