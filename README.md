# Contributing to Mercury's Buck2 fork (mercury-head)

This repo is Mercury's internal fork of [Buck2](https://github.com/facebook/buck2). The `mercury-head` branch is our maintained head.

## Workflow

### 1. Branch from mercury-head

```bash
git fetch origin
git checkout -b my-feature origin/mercury-head
```

### 2. Make a PR to mercury-head

Open a pull request targeting the `mercury-head` branch (not `main`).

### 3. After merging, create a tag

Once your PR is merged, tag the new `mercury-head` HEAD using the format:

```
mwb-<date>-base-<fb-base-tag>
```

Where:
- `<date>` is today's date in `YYYY-MM-DD` format
- `<fb-base-tag>` is the date tag of the FB upstream commit that `mercury` is currently based on (e.g. `2026-01-19`)

Example:

```bash
# Find the current mercury HEAD
git fetch origin
git rev-parse origin/mercury-head

# Create and push the tag
git tag mwb-2026-03-25-base-2026-01-19 origin/mercury
git push origin mwb-2026-03-25-base-2026-01-19
```

To find the correct `<fb-base-tag>`: look at the FB upstream tags in this repo (e.g. `2026-01-19`, `2026-03-15`) and identify which one the current `mercury-head` branch is based on.

### 4. Update buck2-source in mwb

In the [mercury-web-backend](https://github.com/MercuryTechnologies/mercury-web-backend) repo, update the `buck2-source` reference to point to your new tag.
