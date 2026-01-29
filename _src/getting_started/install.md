---
id: install
title: Installing Buck2
---

import { FbInternalOnly } from 'docusaurus-plugin-internaldocs-fb/internal';

<FbInternalOnly>

## Internal Meta User

For Internal Meta Users, Buck2 is already configured and available for you.
Simply cloning the
[`fbsource`](https://www.internalfb.com/wiki/Repositories/fbsource/#cloning)
repository is all that's required to get started; no separate installation steps
for Buck2 are necessary.

If you have any issues, please check [here](../../users/faq/meta_installation).

</FbInternalOnly>

## Installing Buck2

The latest set of `buck2` executables can be found under the
[`latest` release page](https://github.com/facebook/buck2/releases/tag/latest).

Additionally, for each bi-monthly release there is a
[dotslash](https://dotslash-cli.com) file that is appropriate for committing to
a repository. This will automatically fetch the correct version and architecture
for each user, and ensures a consistent build environment for each commit in the
repo.

To get started, first install [rustup](https://rustup.rs/), then compile the
`buck2` executable:

```bash
rustup install nightly-2025-08-01
cargo +nightly-2025-08-01 install --git https://github.com/facebook/buck2.git buck2
```

The above commands install `buck2` into a suitable directory, such as
`$HOME/.cargo/bin`, which you should then add to your `$PATH`:

Linux / macOS

```sh
export PATH=$HOME/.cargo/bin:$PATH
```

Windows Powershell

```powershell
$Env:PATH += ";$HOME\.cargo\bin"
```

With Buck2 installed, you can build projects with `buck2`!

You can verify that it's working by running `buck2 --help`.
