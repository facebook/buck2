# Contributing to Buck2

We want to make contributing to this project as easy and transparent as possible.

## Our Development Process

Buck2 is currently developed in Meta's internal repositories. Code that is developed internally gets reviewed, sent through CI, committed, and then automatically mirrored out to GitHub every 15 minutes. Code that arrives through a PR is reviewed by a Meta developer on GitHub, then once accepted, moved into our internal workflow where it is reviewed, sent through CI, committed and added to the repo. We maintain both external CI (the results of which are visible on GitHub) and a more thorough internal CI (building internal projects etc). Alas, our full test suite is not yet mirrored to the open source repo, but we hope to fix that in due course.

## Pull Requests

We actively welcome your pull requests.

1. Fork the repo and create your branch from `main`.
2. If you've added code that should be tested, add tests.
3. If you've changed APIs, update the documentation.
4. Ensure the test suite passes.
5. Make sure your code passes any lints.
6. If you haven't already, complete the Contributor License Agreement ("CLA").

## Contributor License Agreement ("CLA")

In order to accept your pull request, we need you to submit a CLA. You only need
to do this once to work on any of Meta's open source projects.

Complete your CLA here: <https://code.facebook.com/cla>

## Issues

We use GitHub issues to track public bugs. Please ensure your description is
clear and has sufficient instructions to be able to reproduce the issue.

Meta has a [bounty program](https://www.facebook.com/whitehat/) for the safe
disclosure of security bugs. In those cases, please go through the process
outlined on that page and do not file a public issue.

## Coding Style

Follow the automatic `rust fmt` configuration.

## License

By contributing to Buck2, you agree that your contributions will be
licensed under both the [LICENSE-MIT](LICENSE-MIT) and [LICENSE-APACHE](LICENSE-APACHE)
files in the root directory of this source tree.
