---
id: index
title: Introduction
---

Welcome to Buck2, a large scale, fast, reliable, and extensible build tool
developed and used by Meta. Buck2 supports a variety of languages on many
platforms.

Buck2's core is written in [Rust](https://www.rust-lang.org/).
[Starlark](https://github.com/bazelbuild/starlark), which is a deterministic,
immutable dialect of Python, is used to extend the Buck2 build system, enabling
Buck2 to be language-agnostic. With Starlark, users can define their own custom
rules.

Buck2 leverages the Bazel spec of
[Remote Build Execution](https://bazel.build/remote/rbe) as the primary means of
parallelization and caching, which increases the importance of idempotency (no
matter how many times an operation is performed, it yields the same result) and
hermeticity (code is sealed off from the world), giving the right results,
reliably.

Buck2 multi-language support includes C++, Python, Java, Go, Rust, Erlang,
OCaml, and more.

The following sub-sections contain a list of links to key points in the Buck2
Documentation website that explain the advantages of using Buck2 for you and
your team.

## Buck2 Documentation Website Links

### For end users

- [Getting Started](getting_started.md) - how to get started with using Buck2.
- [Benefits](benefits.md) - the benefits of using Buck2.

<FbInternalOnly>

- [Migration Guide](users/migration_guide.fb.md) - how to port projects from
  Buck to Buck2, including the issues you might face and notable differences.
- [Buck2 and Build Observability](users/build_observability/observability.fb.md) -
  how to use Buck2's datasets to analyze specific invocations or classes of
  invocations.
- [Migrating builds to work VPNless](users/advanced/vpnless.fb.md) - how to
  migrate builds to work without VPN or lighthouse access.

</FbInternalOnly>

### For people writing rules

- [Writing Rules](rule_authors/writing_rules.md) - how to write rules to support
  new languages.
- [Build APIs](api/build/globals) - documentation for the APIs available when
  writing rules.
- [Starlark Types](https://github.com/facebookexperimental/starlark-rust/blob/main/docs/types.md) -
  rules are written in Starlark (which is approximately Python), but our version
  adds types.

<FbInternalOnly>

- [Rule Writing Tips](rule_authors/rule_writing_tips.fb.md) - tips for migrating
  rules from Buck1 to Buck2.

</FbInternalOnly>

### For people integrating with Buck2

- [Extending Buck via BXL](developers/bxl.md) - powerful Starlark scripts for
  introspection of Buck2's graphs.
- [Reindeer](https://github.com/facebookincubator/reindeer) - a set of tools for
  importing Rust crates from crates.io, git repos etc and generating a BUCK file
  for using them.
- [ocaml-scripts](https://github.com/facebook/ocaml-scripts) - scripts to
  generate a BUCK file enabling the use of OCaml packages from an OPAM switch.

### External articles about Buck2

- [Introducing Buck2](https://engineering.fb.com/2023/04/06/open-source/buck2-open-source-large-scale-build-system/) -
  our initial introduction when we open sourced Buck2.
- [Reddit AMA](https://old.reddit.com/r/rust/comments/136qs44/hello_rrust_we_are_meta_engineers_who_created_the/)
  where the Buck2 team answered a number of questions.
- [Using buck to build Rust projects](https://steveklabnik.com/writing/using-buck-to-build-rust-projects) -
  working through an initial small Rust project, by
  [Steve Klabnik](https://steveklabnik.com/). Followed up by
  [building from crates.io](https://steveklabnik.com/writing/using-cratesio-with-buck)
  and [updating Buck2](https://steveklabnik.com/writing/updating-buck).
- [Awesome Buck2](https://github.com/sluongng/awesome-buck2) is a collection of
  resources about Buck2.
- [Buck2 Unboxing](https://www.buildbuddy.io/blog/buck2-review/) is a general
  review of Buck2 by [Son Luong Ngoc](https://github.com/sluongng/).
- [A tour around Buck2](https://www.tweag.io/blog/2023-07-06-buck2/) gives an
  overview of Buck2 and how it differs from Bazel.

### External videos about Buck2

- [antlir2: Deterministic image bulids with Buck2](https://www.youtube.com/watch?v=Wv-ilbckSx4)
  talks about layering a packaging system over Buck2.

### External projects using Buck2

- [System Initiative](https://www.systeminit.com/) build their DevOps product
  [using Buck2](https://nickgerace.dev/post/system-initiative-the-second-wave-of-devops/#under-the-hood),
  with their own custom prelude.
- [Rust `cxx` library](https://github.com/dtolnay/cxx) has examples and tests
  with a wide variety of build systems, including Buck2.
- [`ocamlrep` library](https://github.com/facebook/ocamlrep) allows for interop
  between OCaml and Rust code, and can be
  [built with Buck2](https://github.com/facebook/ocamlrep/blob/main/README-BUCK.md).
- [`buck2-nix`](https://github.com/thoughtpolice/buck2-nix) is an experiment to
  integrate Buck2, [Sapling](https://sapling-scm.com) and
  [Nix](https://nixos.org) together in a harmonious way.

Feel free to
[send a PR](https://github.com/facebook/buck2/edit/main/docs/index.md) adding
your project.

<FbInternalOnly>

### For people developing Buck2

- [Basic README](https://www.internalfb.com/code/fbsource/fbcode/buck2/README.md) -
  how to get started, compile Buck2 and the basic workflows.
- [Notes for Developers](developers/developers.fb.md) - more advanced workflows
  and notes around debugging, profiling etc.

## Specialised groups

We have Workplace groups and task tags for various projects. Most task folders
are _not monitored_, so post all questions and bug reports to a Workplace group.

### Workplace groups

- [Admarket](https://fb.workplace.com/groups/2011248092366093) - collaboration
  between Admarket, DevX and Build Infra teams in their effort to migrate
  Admarket to Buck2.
- [Android](https://fb.workplace.com/groups/4318511658259181) - discussions on
  anything related to the migration of fbandroid to Buck2.
- [Apple](https://fb.workplace.com/groups/305599448025888/) - discussions
  related to the migration of fbobjc to Buck2.
- [Fbcode TD](https://fb.workplace.com/groups/603286664133355/) - migrations for
  TDs, including fbcode, mobile, and rl TDs, as well as UTD.
- [Fbcode](https://fb.workplace.com/groups/1080276222750085) - collaboration
  between fbcode teams, DevX and Build Infra in their effort to migrate fbcode
  services to Buck2.
- [Hack](https://fb.workplace.com/groups/496546384752884) - discussions, ideas,
  updates, and more as we move Hack to Buck2.
- [Haskell](https://fb.workplace.com/groups/202582585277200/) - discussions,
  ideas, updates, and more as we move Haskell to Buck2.
- [Infer](https://fb.workplace.com/groups/601798364244831/) - discussions
  related to ideas, bugs, jobs, and feedback on Infer.
- [Open source](https://fb.workplace.com/groups/3434452653448246) - people
  particularly enthusiastic about open sourcing Buck2.
- [Reality labs](https://fb.workplace.com/groups/930797200910874/) - unmoderated
  non-support group for talking about arvr's integration and onboarding to
  Buck2.
- [Shots](https://fb.workplace.com/groups/4899204743424118) - Shots engineers
  who are experimenting with Buck2.
- [Tpx](https://fb.workplace.com/groups/900436963938958/) - Buck2/Tpx
  coordination group.
- [Unicorn](https://fb.workplace.com/groups/503973410692177) - collaboration
  between Unicorn, DevX and Build Infra teams in their effort to migrate Unicorn
  to Buck2.
- [WhatsApp](https://fb.workplace.com/groups/whatsapp.buck2) - Buck2 in the
  WhatsApp server.
- [Windows](https://fb.workplace.com/groups/580747310463852/) - discussions
  related to Buck2 on Windows.

### Task folders

- [Admarket on Buck V2](https://www.internalfb.com/tasks?q=163089765955500)
- [Apple Build Infra](https://www.internalfb.com/tasks?q=1710478139132259)
- [Buck2](https://www.internalfb.com/tasks?q=446583836738538)
- [DICE - BuckV2](https://www.internalfb.com/tasks?q=413466250534831)
- [Eden on Buck V2](https://www.internalfb.com/tasks?q=406698320868619)
- [FbCode TD on Buck2](https://www.internalfb.com/tasks?q=980682532796984)
- [Unicorn on Buck V2](https://www.internalfb.com/tasks?q=262220628906648)

</FbInternalOnly>
