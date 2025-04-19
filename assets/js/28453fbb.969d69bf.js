"use strict";(self.webpackChunkwebsite=self.webpackChunkwebsite||[]).push([[9035],{28453:(e,t,n)=>{n.d(t,{R:()=>o,x:()=>i});var r=n(96540);const a={},s=r.createContext(a);function o(e){const t=r.useContext(s);return r.useMemo((function(){return"function"==typeof e?e(t):{...t,...e}}),[t,e])}function i(e){let t;return t=e.disableParentContext?"function"==typeof e.components?e.components(a):e.components||a:o(e.components),r.createElement(s.Provider,{value:t},e.children)}},72895:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>u,contentTitle:()=>c,default:()=>h,frontMatter:()=>i,metadata:()=>r,toc:()=>l});const r=JSON.parse('{"id":"users/faq/buck_hanging","title":"Why is Buck2 hanging?","description":"Let\'s look at how to troubleshoot when buck2 hangs, i.e. it just sits there","source":"@site/../docs/users/faq/buck_hanging.md","sourceDirName":"users/faq","slug":"/users/faq/buck_hanging","permalink":"/docs/users/faq/buck_hanging","draft":false,"unlisted":false,"tags":[],"version":"current","frontMatter":{"id":"buck_hanging","title":"Why is Buck2 hanging?"},"sidebar":"main","previous":{"title":"Debugging Excess Starlark Peak Memory","permalink":"/docs/users/faq/starlark_peak_mem"},"next":{"title":"Buck2 Consoles","permalink":"/docs/users/build_observability/interactive_console"}}');var a=n(74848),s=n(28453),o=n(78191);const i={id:"buck_hanging",title:"Why is Buck2 hanging?"},c=void 0,u={},l=[{value:"How to debug a \u201cbusy\u201d hang",id:"how-to-debug-a-busy-hang",level:2},{value:"Getting a stack trace",id:"getting-a-stack-trace",level:3},{value:"Interpreting the stack trace",id:"interpreting-the-stack-trace",level:3},{value:"How to debug a \u201cdoing nothing\u201d hang",id:"how-to-debug-a-doing-nothing-hang",level:2}];function d(e){const t={a:"a",code:"code",em:"em",h2:"h2",h3:"h3",p:"p",pre:"pre",strong:"strong",...(0,s.R)(),...e.components};return(0,a.jsxs)(a.Fragment,{children:[(0,a.jsx)(t.p,{children:'Let\'s look at how to troubleshoot when buck2 hangs, i.e. it just sits there\nsaying "Jobs: In progress: 0, ..." but it\u2019s not finishing...'}),"\n",(0,a.jsxs)(t.p,{children:["When buck2 hangs, there are two possibilities: It\u2019s either hanging doing\n",(0,a.jsx)(t.em,{children:"something"}),", or it\u2019s hanging doing ",(0,a.jsx)(t.em,{children:"nothing"}),". The first thing you should do is\nfigure out which of those is happening. That\u2019s because the tools to debug either\nof those are ",(0,a.jsx)(t.em,{children:"very"})," different! We will mainly focus on the first in this case."]}),"\n",(0,a.jsxs)(t.p,{children:["To figure out which hang you have on your hands, just look at how much CPU buck2\nis using when the hang occurs using your favorite activity monitor (e.g. ",(0,a.jsx)(t.code,{children:"top"}),",\n",(0,a.jsx)(t.code,{children:"htop"}),"). Remember that you can find the buck2 daemon\u2019s PID using ",(0,a.jsx)(t.code,{children:"buck2 status"}),".\nIdeally, break the utilization down by threads (in top, that\u2019s ",(0,a.jsx)(t.code,{children:"top -Hp $PID"}),")."]}),"\n",(0,a.jsx)(t.p,{children:"If any thread is using 100% CPU for some period of time, then you probably have\na busy hang (buck2 is doing \u201csomething\u201d) which are usually easier to debug."}),"\n",(0,a.jsx)(t.h2,{id:"how-to-debug-a-busy-hang",children:"How to debug a \u201cbusy\u201d hang"}),"\n",(0,a.jsx)(t.h3,{id:"getting-a-stack-trace",children:"Getting a stack trace"}),"\n",(0,a.jsxs)(t.p,{children:["When debugging a busy hang, the first thing to do is to work out what the\nprocess is doing. There are many tools you can use for this (like a profiler),\nbut the absolute simplest one is quickstack: just run ",(0,a.jsx)(t.code,{children:"quickstack -p $PID"}),", and\nit\u2019ll show you a stack dump for all the threads in your process. If you prefer\n",(0,a.jsx)(t.code,{children:"gdb"}),", you can use ",(0,a.jsx)(t.code,{children:"gdb -p $PID"}),", then ",(0,a.jsx)(t.code,{children:"thread apply all bt"}),", and that\u2019s the\nsame thing."]}),"\n",(0,a.jsxs)(t.p,{children:["Note that a stack trace tells you what the process is doing at a point in time,\nso don\u2019t just look at the very last frame and call it the culprit. Instead, look\nat the stack as a whole. If you need more perspective, use a sampling profiler\n",(0,a.jsx)(o.FbInternalOnly,{children:"(strobeclient run --pid $PID)"}),". You can also\njust grab stack traces at a few points in time and see if they look similar:\nthis is exactly what a sampling profiler does, albeit at a higher frequency."]}),"\n",(0,a.jsx)(t.h3,{id:"interpreting-the-stack-trace",children:"Interpreting the stack trace"}),"\n",(0,a.jsxs)(t.p,{children:["Let's consider an example user report ",(0,a.jsxs)(o.FbInternalOnly,{children:["( see\n",(0,a.jsx)(t.a,{href:"https://fb.workplace.com/groups/buck2users/permalink/3232782826978076/",children:"here"}),")"]}),"\nwith the following stack trace:"]}),"\n",(0,a.jsx)(t.pre,{children:(0,a.jsx)(t.code,{children:"#01  0x0000000005b1ec26 in <buck2_build_api::artifact_groups::artifact_group_values::TransitiveSetIterator<buck2_build_api::artifact_groups::artifact_group_values::ArtifactGroupValues, (buck2_build_api::actions::artifact::Artifact, buck2_execute::artifact_value::ArtifactValue), buck2_build_api::artifact_groups::artifact_group_values::ArtifactValueIdentity> as core::iter::traits::iterator::Iterator>::next () from ...\n#02  0x0000000005b23998 in <buck2_build_api::artifact_groups::artifact_group_values::TransitiveSetIterator<buck2_build_api::artifact_groups::artifact_group_values::ArtifactGroupValues, (buck2_build_api::actions::artifact::Artifact, buck2_execute::artifact_value::ArtifactValue), buck2_build_api::artifact_groups::artifact_group_values::ArtifactValueIdentity> as itertools::Itertools>::exactly_one () from ...\n#03  0x00000000059dbb2c in buck2_server_commands::commands::build::create_unhashed_outputs () from ...\n#04  0x0000000005c3c677 in <core::future::from_generator::GenFuture<<buck2_server_commands::commands::build::BuildServerCommand as buck2_server_ctx::template::ServerCommandTemplate>::command::{closure#0}> as core::future::future::Future>::poll () from ...\n#05  0x00000000054c58a3 in <core::future::from_generator::GenFuture<<alloc::boxed::Box<dyn buck2_server_ctx::ctx::ServerCommandContextTrait> as buck2_server_ctx::ctx::ServerCommandDiceContext>::with_dice_ctx<buck2_server_ctx::template::run_server_command<buck2_server_commands::commands::build::BuildServerCommand>::{closure#0}::{closure#0}::{closure#0}, core::pin::Pin<alloc::boxed::Box<dyn core::future::future::Future<Output = core::result::Result<cli_proto::BuildResponse, anyhow::Error>> + core::marker::Send>>, cli_proto::BuildResponse>::{closure#0}> as core::future::future::Future>::poll () from ...\n#06  0x00000000054c7ae3 in <core::future::from_generator::GenFuture<buck2_server_ctx::template::run_server_command<buck2_server_commands::commands::build::BuildServerCommand>::{closure#0}::{closure#0}> as core::future::future::Future>::poll () from ...\n#07  0x0000000005370df8 in <buck2_events::dispatch::Span>::call_in_span::<core::task::poll::Poll<(core::result::Result<cli_proto::BuildResponse, anyhow::Error>, buck2_data::CommandEnd)>, <buck2_events::dispatch::EventDispatcher>::span_async<buck2_data::CommandStart, buck2_data::CommandEnd, core::future::from_generator::GenFuture<buck2_server_ctx::template::run_server_command<buck2_server_commands::commands::build::BuildServerCommand>::{closure#0}::{closure#0}>, core::result::Result<cli_proto::BuildResponse, anyhow::Error>>::{closure#0}::{closure#0}::{closure#0}> () from ...\n#08  0x00000000054f7288 in <core::future::from_generator::GenFuture<<cli::commands::daemon::BuckdServerDependenciesImpl as buck2_server::daemon::server::BuckdServerDependencies>::build::{closure#0}> as core::future::future::Future>::poll () from...\n ...\n"})}),"\n",(0,a.jsxs)(t.p,{children:["At this point, you can look at the code, and note that there is no span around\nthe output symlink creation function (",(0,a.jsx)(t.code,{children:"create_unhashed_outputs"}),"). This suggests\nyou\u2019ve found your culprit: there is indeed a buck2 bug and we\u2019re spending ages\ncreating unhashed output symlinks, and since you need a span to get any console\nfeedback, the console says nothing is happening."]}),"\n",(0,a.jsxs)(t.p,{children:[(0,a.jsx)(t.strong,{children:"An easy fix"}),": In this particular instance, Thomas spotted\n",(0,a.jsx)(t.a,{href:"https://github.com/facebook/buck2/commit/d677e41253b73a31aafa1255a532c38992482efd",children:"an easy optimization"}),"\nwhich resolved the issue. But, of course, that\u2019s not always possible. If the\neasy fix hadn't been available, we\u2019d be at a dead end, so what do we do next?"]}),"\n",(0,a.jsxs)(t.p,{children:[(0,a.jsx)(t.strong,{children:"A harder fix"}),": If it is not clear what the root-cause is, ",(0,a.jsx)(o.OssOnly,{children:"you can\nbisect"}),(0,a.jsx)(o.FbInternalOnly,{children:(0,a.jsx)(t.a,{href:"users/faq/how_to_bisect.fb.md",children:"you can bisect"})}),":\ni.e. do a binary search across commits for the commit that introduced a given\nbreakage/perf degradation. ",(0,a.jsxs)(o.FbInternalOnly,{children:[" Thanks to the fact that we enforce a\nlinear history, bisecting is pretty straightforward in\n",(0,a.jsx)(t.code,{children:"fbsource"}),"."]})," Then, once you identify their commit that caused\nbreakage, investigate what caused the issue."]}),"\n",(0,a.jsx)(t.h2,{id:"how-to-debug-a-doing-nothing-hang",children:"How to debug a \u201cdoing nothing\u201d hang"}),"\n",(0,a.jsxs)(t.p,{children:[(0,a.jsx)(t.strong,{children:"Cycle in dependencies"}),": If buck2 seems to be doing nothing (e.g. CPU usage is\n0%), one of the reasons could be a cycle in your dependencies, which may cause\nbuck2 to hang (buck2 does implement a form of cycle detection, but it\nunfortunately has false negatives). You can confirm this by running buck1, which\nwill report cycles properly."]})]})}function h(e={}){const{wrapper:t}={...(0,s.R)(),...e.components};return t?(0,a.jsx)(t,{...e,children:(0,a.jsx)(d,{...e})}):d(e)}}}]);