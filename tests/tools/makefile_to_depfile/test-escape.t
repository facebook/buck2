  $ $DEP_FILE_PROCESSOR "makefile" "$TESTDIR/fixtures/escape.mk" "$CRAMTMP/escape" true
  $ cat "$CRAMTMP/escape"
  app.c
  header.h
  header 2.h
