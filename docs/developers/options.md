# Buck 2 specific options

Buck 2 introduces some options that don't exist in v1 and are accessed in the
root cell:

- `project.watchman_merge_base`: defines the merge base to use for SCM-aware
  queries to Watchman. This is read when the daemon starts and cannot be
  changed later without a restart.
- `test.v2_test_executor`: defines the program to invoke as the test executor
  in `buck test`. This is read every time a test command executes.
