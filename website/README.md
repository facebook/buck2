# Website

This website is built using [Docusaurus 2](https://docusaurus.io/), a modern static website generator.

## Installation

```shell
$ yarn global add node-gyp
$ yarn
```

If on Eden you might get faster builds by doing `eden redirect add $PWD/node_modules bind` first.

## Build

To build a copy of the static content against the version of `buck2` on your path:

```shell
$ yarn build
```

To build a copy of the static content using `../.buck2.sh` (which builds buck2 from the repo before invoking it):

```shell
$ yarn build_local
```

Both of these commands generate static content into the `build` directory and can be served using any static contents hosting service.

## Local Development

```shell
$ yarn start
```

This command starts a local development server and opens up a browser window. Any changes to generated Starlark API documentation require running the build command above, but changes to the .md files that are checked into the repository should be reflected live without having to restart the server.


## Deployment

```shell
$ GIT_USER=<Your GitHub username> USE_SSH=true yarn deploy
```

If you are using GitHub pages for hosting, this command is a convenient way to build the website and push to the `gh-pages` branch.
