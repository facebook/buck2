# FAQ

#### **Q: Why is it called Buck?**

A: The word "buck" is similar to the word "build" and is quick to type. It also has awesome mascot potential.

#### **Q: Why is Buck built with Ant instead of Buck?**

A: Self-hosting systems can be more difficult to maintain and debug.
If Buck built itself using Buck, then every time a change was made to Buck's source, the commit would have to include a new Buck binary that included that change. It would be easy to forget to include the binary, difficult to verify that it was the correct binary, and wasteful to bloat the Git history of the repository with binaries that could be rebuilt from source. Building Buck using Ant ensures we are always building from source, which is simpler to verify.
Also, because Ant is a more mature build system than Buck, it has support for features that we have not had time to include in Buck yet, such as generating Javadoc, static analysis via [PMD](http://pmd.sourceforge.net/), Python unit tests, etc.
That said, as a sanity check, Buck is capable of building itself. Once you build Buck using Ant, you can re-build Buck using Buck by running `./bin/buck build buck`.
