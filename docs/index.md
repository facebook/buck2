---
id: index
title: Introduction
---

Welcome to Buck2, a successor to [Buck](https://buck.build)!

Buck2 is largely compatible with Buck, so you can start building with `buck2` today.

<FbInternalOnly>

## Communication channels

* [Buck2 Engineering](https://fb.workplace.com/groups/buck2prototyping) - Workplace group for discussions about what features Buck2 should have, how it's going, status updates, and much more.
* [Buck2 Users](https://fb.workplace.com/groups/buck2users) - Workplace group featuring questions from users and reports of bugs.
[Buck2 Rule Authors](https://fb.workplace.com/groups/347532827186692) - Workplace group for discussions about language-specific rules.
* [Buck2 Oncall Hub](https://www.internalfb.com/intern/monitor/oncall_profile?oncall=buck2) - urgent tasks and escalation.

## Specialised groups

We have Workplace groups and task tags for various projects. Most task folders are *not monitored*, so post all questions and bug reports to a Workplace group.

### Workplace groups

* [Admarket](https://fb.workplace.com/groups/2011248092366093) - collaboration between Admarket, DevX and Build Infra teams in their effort to migrate Admarket to Buck2.
* [Android](https://fb.workplace.com/groups/4318511658259181) - discussions on anything related to the migration of fbandroid to Buck2.
* [Apple](https://fb.workplace.com/groups/305599448025888/) - discussions related to the migration of fbobjc to Buck2.
* [Fbcode TD](https://fb.workplace.com/groups/603286664133355/) - migrations for TDs, including fbcode, mobile, and rl TDs, as well as UTD.
* [Fbcode](https://fb.workplace.com/groups/1080276222750085) - collaboration between fbcode teams, DevX and Build Infra in their effort to migrate fbcode services to Buck2.
* [Hack](https://fb.workplace.com/groups/496546384752884) - discussions, ideas, updates, and more as we move Hack to Buck2.
* [Haskell](https://fb.workplace.com/groups/202582585277200/) - discussions, ideas, updates, and more as we move Haskell to Buck2.
* [Infer](https://fb.workplace.com/groups/601798364244831/) - discussions related to ideas, bugs, jobs, and feedback on Infer.
* [Open source](https://fb.workplace.com/groups/3434452653448246) - people particularly enthusiastic about open sourcing Buck2.
* [Reality labs](https://fb.workplace.com/groups/930797200910874/) - unmoderated non-support group for talking about arvr's integration and onboarding to Buck2.
* [Shots](https://fb.workplace.com/groups/4899204743424118) - Shots engineers who are experimenting with Buck2.
* [Tpx](https://fb.workplace.com/groups/900436963938958/) - Buck2/Tpx coordination group.
* [Unicorn](https://fb.workplace.com/groups/503973410692177) - collaboration between Unicorn, DevX and Build Infra teams in their effort to migrate Unicorn to Buck2.
* [WhatsApp](https://fb.workplace.com/groups/whatsapp.buck2) - Buck2 in the WhatsApp server.
* [Windows](https://fb.workplace.com/groups/580747310463852/) - discussions related to Buck2 on Windows.

### Task folders

* [Admarket on Buck V2](https://www.internalfb.com/tasks?q=163089765955500)
* [Apple Build Infra](https://www.internalfb.com/tasks?q=1710478139132259)
* [Buck2](https://www.internalfb.com/tasks?q=446583836738538)
* [DICE - BuckV2](https://www.internalfb.com/tasks?q=413466250534831)
* [Eden on Buck V2](https://www.internalfb.com/tasks?q=406698320868619)
* [FbCode TD on Buck2](https://www.internalfb.com/tasks?q=980682532796984)
* [Unicorn on Buck V2](https://www.internalfb.com/tasks?q=262220628906648)

</FbInternalOnly>

## For end users

* [Benefits](benefits.md) - the benefits of using Buck2.

<FbInternalOnly>

* [Migration Guide](migration_guide.fb.md) - how to port projects from Buck to Buck2, including the issues you might face and notable differences.
* [Buck2 and Build Observability](developers/observability.fb.md) - how to use Buck2's datasets to analyze specific invocations or classes of invocations.

</FbInternalOnly>

## For people writing rules

* [Writing Rules](rule_authors/writing_rules.md) - how to write rules to support new languages.
* [Rule APIs](rule_authors/rule_api.md) - gives the API available when writing rules.
* [Starlark Types](https://github.com/facebookexperimental/starlark-rust/blob/main/docs/types.md) - rules are written in Starlark (which is approximately Python), but our version adds types.

<FbInternalOnly>

* [Rule Writing Tips](rule_authors/rule_writing_tips.fb.md) - tips for migrating rules from Buck1 to Buck2.

</FbInternalOnly>

## For people integrating with Buck2

* [Extending Buck via BXL](developers/bxl.md) - powerful Starlark scripts for introspection of buck2's graphs

<FbInternalOnly>

## For people developing Buck2

* [Basic README](https://www.internalfb.com/code/fbsource/fbcode/buck2/README.md) - how to get started, compile Buck2 and the basic workflows.
* [Notes for Developers](developers/developers.fb.md) - more advanced workflows and notes around debugging, profiling etc.

</FbInternalOnly>

## RFCs

### Accepted

* [configured_alias](rfcs/configured-alias.md)
* [Buck Extension Language (BXL)](rfcs/bxl.md)
* [Bxl Support for performing analysis on targets](rfcs/bxl-analysis.md)

### Drafts

* [@configuration syntax](rfcs/drafts/configuration-at-syntax.md)
* [bxl actions and Build API](rfcs/drafts/bxl-actions.md)

### Implemented

* [ProviderCollection[]](rfcs/implemented/provider-collection-at.md)
