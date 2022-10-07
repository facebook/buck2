  $ sed "s:__ROOT__:$(pwd):g" < "$TESTDIR/fixtures/absolute.mk" > "$CRAMTMP/absolute.mk"
  $ $MAKEFILE_TO_DEP_FILE "$CRAMTMP/absolute.mk" "$CRAMTMP/absolute" true
  $ cat "$CRAMTMP/absolute"
  foo/bar
  foo
  bar
  baz
