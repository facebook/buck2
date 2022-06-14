# Buck2 User Guide

Welcome to Buck2! A successor to [Buck](https://buck.build). Buck2 is largely compatible with Buck, so start typing `buck2` today.

## Communication channels

* [Buck2 prototyping](https://fb.workplace.com/groups/buck2prototyping) - where we have discussions about what features Buck2 should have, how it's going, status updates etc.
* [Buck2 users](https://fb.workplace.com/groups/buck2users) - questions from users, reports of bugs.
* [Buck2 rule authors](https://fb.workplace.com/groups/347532827186692) - discussions around specific language rules, e.g. C++.
* [Buck2 oncall](https://www.internalfb.com/intern/monitor/oncall_profile?oncall=buck2) - urgent tasks and escalation.

## Specialised groups

We have both Workplace groups, associated chat channels and task tags for various projects. Most task folders are _not monitored_, so post all questions and bug reports to a group.

* Groups: [Admarket](https://fb.workplace.com/groups/2011248092366093), [Android](https://fb.workplace.com/groups/4318511658259181), [Apple](https://fb.workplace.com/groups/305599448025888/), [Fbcode TD](https://fb.workplace.com/groups/603286664133355/), [Fbcode](https://fb.workplace.com/groups/1080276222750085), [Hack](https://fb.workplace.com/groups/496546384752884), [Haskell](https://fb.workplace.com/groups/202582585277200/), [Infer](https://fb.workplace.com/groups/601798364244831/), [Open source](https://fb.workplace.com/groups/3434452653448246), [Reality labs](https://fb.workplace.com/groups/930797200910874/), [Shots](https://fb.workplace.com/groups/4899204743424118), [Tpx](https://fb.workplace.com/groups/900436963938958/), [Unicorn](https://fb.workplace.com/groups/503973410692177), [WhatsApp](https://fb.workplace.com/groups/whatsapp.buck2)
* Task folders: [Admarket](https://www.internalfb.com/tasks?q=163089765955500), [Android](https://www.internalfb.com/tasks?q=406698320868619), [Apple](https://www.internalfb.com/tasks?q=1710478139132259), [Buck2](https://www.internalfb.com/tasks?q=446583836738538), [DICE](https://www.internalfb.com/tasks?q=413466250534831)
[Eden](https://www.internalfb.com/tasks?q=406698320868619), [Fbcode TD](https://www.internalfb.com/tasks?q=980682532796984), [Unicorn](https://www.internalfb.com/tasks?q=262220628906648)

## For end users

Documents on how to use Buck2 to build/test/run stuff, and migrate existing projects using Buck to Buck2.

* [Benefits](benefits.md) - the benefits of using Buck2.
* [Migration guide](migration_guide.md) - how to port projects from Buck to Buck2, including the issues you might face and notable differences.
* [Observability](observability.md) - How to use Buck2's datasets to analyze
specific invocations or classes of invocations.

## For people writing rules

* [Writing rules](writing_rules.md) - how to write rules to support new languages.
* [Rule API](rule_api.md) - gives the API available when writing rules.
* [Types in Starlark](https://github.com/facebookexperimental/starlark-rust/blob/main/docs/types.md) - rules are written in Starlark (which is approximately Python), but our version adds types.
* [Rule writing tips](rule_writing_tips.md) - tips for migrating rules from buck1 to buck2.

## For people integrating with Buck2
* [Extending Buck via BXL](bxl.md) - powerful Starlark scripts for introspection of buck2's graphs

## For people developing Buck2

* [Basic README](https://www.internalfb.com/code/fbsource/fbcode/buck2/README.md) - how to get started, compile Buck2 and the basic workflows.
* [Developer notes](developers.md) - more advanced workflows and notes around debugging, profiling etc.

## RFCs

### Accepted

* [configured_alias](rfcs/configured-alias.md)
* [BQL => Buck Extension Language (BXL)](rfcs/bxl.md)
* [bxl analysis](rfcs/bxl-analysis.md)

### Drafts

* [@configuration syntax](rfcs/drafts/configuration-at-syntax.md)
* [bxl actions API](rfcs/drafts/bxl-actions.md)
