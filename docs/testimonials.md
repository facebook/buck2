# Testimonials

In [why to migrate](benefits.md) we argue that Buck2 is fast and easy to use. To convince you, on this page we share real feedback from real users.

## It's fast

When working on the Platform010 transition for Unicorn, [Lucian found](https://fb.workplace.com/groups/503973410692177/posts/617815372641313/):

> buck2 is superfast ❤️ - buck1 needed 38min, buck2 finished in 11m. 3.5x faster! 🔥

[Alisdair migrated Multifeed](https://fb.workplace.com/groups/715433795171905/posts/7635147689867113) to Buck2 and benchmarked it:

> Aggregator now fully builds with buck2, and, in opt-clang mode, is about 20-25% quicker for some common scenarios, and up to about 75-99% quicker for less common scenarios.

Anton used Buck2 as part of his [day to day development of Adindexer](https://fb.workplace.com/groups/2011248092366093/posts/2148586721965562):

> Buck2 is so fast! Out of 35 builds I did in the past 2 days only one is 4.3min and all the others are ❤ (meaning less than 3 minutes and that I love it). What would those builds have been in v1? Easily all over 5-6min.

John from WhatsApp server [tried out Buck2](https://fb.workplace.com/groups/whatsapp.buck2/posts/590361172244469) as a replacement for `rebar3`:

> I mean, just wow. It feels like I've been slowly suffocating from slow build times over the last year or two and now suddenly ... it's like a breath of fresh air. Compiles go from taking multiple minutes to mere seconds. It's the difference between taking out my phone or checking my email while I wait to just staying laser focused on the task at hand.

The [Remote Execution team switched](https://fb.workplace.com/groups/200907040536486/posts/948810132412836), and Barys reported:

> TL;DR; As the RE team we are using Buck2, and our day-to-day work has been significantly improved! ... Incremental builds are blazing fast. ... The UI is cool and informative.

Jason [migrated repo stats to Buck2](https://fb.workplace.com/groups/191525405325048/posts/641012290376355):

> We saw: A 15x speed improvement; Our success rate bumped to 100%. This has helped unblock the ability to generate Cinder statistics, and made it possible to run broader and more complex queries successfully.

Karl moved [some C++, Python and Go projects from DCA:Bootstrap](https://fb.workplace.com/groups/provinternals/posts/4781192448624349):

> For most of our builds the average build time is _significantly_ lower on buck2, there are similar results for running unittests.

Yan reports on [moving Mononoke to Buck2](https://fb.workplace.com/groups/sourcecontrolteam/posts/4817681415019833):

> I think the biggest win here is developer velocity. As well as the builds being faster in average, developers are also less afraid of running buck command and losing 5+ seconds for no good reason when there are few/no changes.

Roman looked at [payments Contbuilds for a month](https://fb.workplace.com/groups/316128930443878/posts/349980043725433):

> DEV mode: Average improvement over 59 contbuilds: 67.60%. OPT mode: Average improvement over 59 contbuilds: 39.79%. ... those are very great results!

Edgar looked at [developer payments builds for a month](https://fb.workplace.com/permalink.php?story_fbid=pfbid02E5ZVRZ6TUuTGoSPoA9P5HxLLiKBzcUNfLi4W8e5c4wPzDgRQbZxM2qB8XNmCrXzbl&id=100001234709253):

> we have saved 50 engineering days this half! (nearly an engineering month just in July). ... Use buck2! You will save time and have a better experience while working at payments.

Timotej and Ke [moved the FAUSTA WhatsApp CI jobs to Buck2](https://fb.workplace.com/groups/695833467664477/posts/1066729297241557):

> Fausta skycastle jobs are now using buck2 to compile waserver. This speeds up the fausta signal by ~7 minutes (about 25%).

The Meta Financial Technologies team [moved to Buck2](https://fb.workplace.com/notes/164371382676769/), finding that in dev mode:

> Nearly all runs which took from 10 minutes to over 1 hour using Buck1 now take less than 5 minutes with Buck2.

WhatsApp server [moved their canaries to Buck2](https://fb.workplace.com/groups/wainfratools/permalink/3201355640124512/):

> With buck2, most canary experiments could be started under 2 minutes even with cold checkout while before it could require waiting up to 7 minutes.

Robert Quitt [moved the Contbuild Indexing Service to Buck2](https://fb.workplace.com/groups/1473739712817308/posts/1911725572352051/):

> Now creating the hotfix index set takes ~4 minutes (was ~11m) and creating the entire index set takes about 9 minutes (was ~30m). Memory usage has decreased as well

The GMB team [finalised their migration](https://fb.workplace.com/groups/1161130867291230/posts/7811781675559416):

> ~30-40% improvement to build times across the board. Tangible operation cost savings can be noted here as well due to engineers having to wait less time for their tests to complete.

Sriram of the IG Server team reported on [their move to Buck2](https://fb.workplace.com/groups/igsrv.devinfra.xfn.fyi/posts/1536781566778386/):

> With buck2, the OPT built times went down from 1 hour to 20 minutes. With buck2, the LTO build times went down from 6 hours to 3 hours. The build memory footprint also came down by 16%.

Stella shared that the [Training_platform fbpkg is Buck2 ready](https://fb.workplace.com/groups/pyper/posts/1080885459459502):

> With buck2 build time was reduced by 73% for TTK backend worker and 56% for TTK workflow. 12.98 eng halves can be saved every 2 years with the current 72% adoption rate.

Shoaib used Buck2 for [upgrading Android native code to a new compiler version](https://fb.workplace.com/groups/4318511658259181/posts/4996007023842971):

> Buck1 took 12 minutes to build that; Buck2 only took 6.5 🙂 It's super awesome to see how far we've come with Buck2 for Android and be able to reap the build speed benefits;

[AI Infra migrated 28% of the buck builds](https://fb.workplace.com/groups/1455586898129582/permalink/1762555647432704/) to buck2 (under Max Stepin's reporting)

> This brings us at 36.81 Eng Halves Saved in 2 years / ~1500 Eng Build Hours Saved each month.

[Mobile toolchains moved](https://fb.workplace.com/groups/buck2users/posts/3230854887170870) their non-fbsource repo to Buck2 and found:

> Buck2 is faster than Buck1 (from empirical measurements), but when paired with RE caching, it's insanely fast.

## Good UX

[Lucian prefers the experience](https://fb.workplace.com/groups/503973410692177/posts/595893258166858/) for a large variety of reasons:

> I want to highlight a few UX reasons for which I vastly prefer buck2 to buck. 1) Instant response on incremental builds. 2) Instant buck2 run. ... 6) No need to use & remember rule flavor strings

## Better for rule authors

Simon started [working on the Haskell rules](https://fb.prod.workplace.com/groups/333784157210625/posts/928214407767594):

> This is all rather fun! Buck2 rules are so much more hackable than Buck1.

Andrew [modified the C++ rules](https://fb.workplace.com/groups/buck2prototyping/posts/2633666273597523):

> Starting working in the rule definitions of v2 for the first time this week and the differences between working there and in v1 are staggering — iteration speed is orders of magnitude faster, writing starlark is way less verbose than java, the strong API boundary encapsulating the core prevents a lot of issues/overhead needed in v1, and printf debugging is super easy to use.

Jeremy [worked on the Rust rules](https://fb.workplace.com/groups/buck2prototyping/posts/2668844656746351):

> Overall, Starlark+types works _really_ well for rules - it feels massively more productive than working in Buck1.

Jason started [working on the Python rules](https://fb.workplace.com/groups/buck2users/posts/2795649764065839):

> This is the best thing since sliced bread. This is all so clear. Its true that I have only had to change buckv1 python native rules like once or twice but it was always painful always confusing. These new rules look very simple and easy to reason about.

## Better for integrators

Martin moved [Infer to Buck2](https://fb.workplace.com/groups/601798364244831/posts/627186868372647):

> One thing that is striking is how simple and straight-forward this integration is compared the existing buck1 flavour solution. Orders of magnitudes less code.

## Summaries

Lucian suggests [Unicorn and Search developers use Buck2](https://fb.workplace.com/groups/1619462355002848/posts/3187555311526870/):

> The biggest FBCode developer experience improvement of the last few years: switch from `buck` to `buck2`!
