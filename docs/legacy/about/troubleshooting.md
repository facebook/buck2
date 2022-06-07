# Troubleshooting

If Buck stops working, then there are several things that you can try to do to fix it.

## Run `buck clean`

Ideally, this solution will never work. Seriously. If Buck is working correctly, then it should know which files have been modified and which files need to be rebuilt.
That said, Buck is not perfect, so it is possible that you have found a defect. In this case, give `buck clean` a shot and file a bug if you have found a reproducible bug.

## Delete all generated files in your project.

Buck is designed so that all generated files are written to the `buck-out` directory, which makes `buck clean` trivial to implement. However, you may use additional tools (such as an IDE) that generate files in other parts of the tree. Such files may inadvertently get included via [`glob()`](https://buck.build/function/glob.html) rules, which would interfere with Buck.
For example, if you are using Git, then you can run:

```
git clean -xfdn
```

to get a list of files in your project that are not under version control. The `-n` switch is for "dry run," which means that Git will not delete any files when you run `git clean`. If you want to use Git to remove the generated files while preserving some non-versioned files (such as `.buckconfig.local`), then use it with the `-e` switch:

```
git clean -xfd -e .buckconfig.local
```

Note that `-e` can be specified multiple times.
