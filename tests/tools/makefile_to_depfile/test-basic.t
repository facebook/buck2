  $ "$MAKEFILE_TO_DEP_FILE" "$TESTDIR/fixtures/basic.mk" "$CRAMTMP/basic" true
  $ cat "$CRAMTMP/basic"
  app.c
  header.h
