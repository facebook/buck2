  $ "$MAKEFILE_TO_DEP_FILE" "$TESTDIR/fixtures/edge-cases.mk" "$CRAMTMP/edge-cases" true
  $ cat "$CRAMTMP/edge-cases"
  leading-separator
   leading-separator-in-file
  trailing-separator-in-file 
  multiple  separator
  trailing-separator

