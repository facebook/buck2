"use strict";(self.webpackChunkwebsite=self.webpackChunkwebsite||[]).push([[5920],{28453:(e,t,r)=>{r.d(t,{R:()=>i,x:()=>o});var d=r(96540);const a={},l=d.createContext(a);function i(e){const t=d.useContext(l);return d.useMemo((function(){return"function"==typeof e?e(t):{...t,...e}}),[t,e])}function o(e){let t;return t=e.disableParentContext?"function"==typeof e.components?e.components(a):e.components||a:i(e.components),d.createElement(l.Provider,{value:t},e.children)}},58870:(e,t,r)=>{r.r(t),r.d(t,{assets:()=>s,contentTitle:()=>n,default:()=>g,frontMatter:()=>o,metadata:()=>d,toc:()=>c});const d=JSON.parse('{"id":"api/bxl/CqueryContext","title":"CqueryContext","description":"The context for performing cquery operations in bxl. The functions offered on this ctx are the same behaviour as the query functions available within cquery command.","source":"@site/../docs/api/bxl/CqueryContext.md","sourceDirName":"api/bxl","slug":"/api/bxl/CqueryContext","permalink":"/docs/api/bxl/CqueryContext","draft":false,"unlisted":false,"tags":[],"version":"current","frontMatter":{},"sidebar":"apiSidebar","previous":{"title":"Context","permalink":"/docs/api/bxl/Context"},"next":{"title":"EnsuredArtifact","permalink":"/docs/api/bxl/EnsuredArtifact"}}');var a=r(74848),l=r(28453),i=r(56289);const o={},n="CqueryContext",s={},c=[{value:"CqueryContext.allpaths",id:"cquerycontextallpaths",level:2},{value:"CqueryContext.attrfilter",id:"cquerycontextattrfilter",level:2},{value:"CqueryContext.attrregexfilter",id:"cquerycontextattrregexfilter",level:2},{value:"CqueryContext.buildfile",id:"cquerycontextbuildfile",level:2},{value:"CqueryContext.deps",id:"cquerycontextdeps",level:2},{value:"CqueryContext.eval",id:"cquerycontexteval",level:2},{value:"CqueryContext.filter",id:"cquerycontextfilter",level:2},{value:"CqueryContext.inputs",id:"cquerycontextinputs",level:2},{value:"CqueryContext.kind",id:"cquerycontextkind",level:2},{value:"CqueryContext.nattrfilter",id:"cquerycontextnattrfilter",level:2},{value:"CqueryContext.owner",id:"cquerycontextowner",level:2},{value:"CqueryContext.rdeps",id:"cquerycontextrdeps",level:2},{value:"CqueryContext.somepath",id:"cquerycontextsomepath",level:2},{value:"CqueryContext.testsof",id:"cquerycontexttestsof",level:2},{value:"CqueryContext.testsof_with_default_target_platform",id:"cquerycontexttestsof_with_default_target_platform",level:2}];function u(e){const t={code:"code",h1:"h1",h2:"h2",header:"header",hr:"hr",p:"p",pre:"pre",...(0,l.R)(),...e.components};return(0,a.jsxs)(a.Fragment,{children:[(0,a.jsx)(t.header,{children:(0,a.jsx)(t.h1,{id:"cquerycontext",children:"CqueryContext"})}),"\n",(0,a.jsxs)(t.p,{children:["The context for performing ",(0,a.jsx)(t.code,{children:"cquery"})," operations in bxl. The functions offered on this ctx are the same behaviour as the query functions available within cquery command."]}),"\n",(0,a.jsxs)(t.p,{children:["Query results are ",(0,a.jsx)(t.code,{children:"target_set"}),"s of ",(0,a.jsx)(t.code,{children:"target_node"}),"s, which supports iteration,\nindexing, ",(0,a.jsx)(t.code,{children:"len()"}),", set addition/subtraction, and ",(0,a.jsx)(t.code,{children:"equals()"}),"."]}),"\n",(0,a.jsx)(t.h2,{id:"cquerycontextallpaths",children:"CqueryContext.allpaths"}),"\n",(0,a.jsx)("pre",{class:"language-python",children:(0,a.jsxs)("code",{children:["def CqueryContext.allpaths(\nfrom: ",(0,a.jsx)(i.default,{to:"/docs/api/build/ConfiguredTargetLabel",children:"ConfiguredTargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/build/TargetLabel",children:"TargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/ConfiguredTargetNode",children:"bxl.ConfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/UnconfiguredTargetNode",children:"bxl.UnconfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/starlark/str",children:"str"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/ConfiguredTargetSet",children:"target_set"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/UnconfiguredTargetSet",children:"target_set"})," | list[",(0,a.jsx)(i.default,{to:"/docs/api/build/ConfiguredTargetLabel",children:"ConfiguredTargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/build/TargetLabel",children:"TargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/ConfiguredTargetNode",children:"bxl.ConfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/UnconfiguredTargetNode",children:"bxl.UnconfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/starlark/str",children:"str"}),"],\nto: ",(0,a.jsx)(i.default,{to:"/docs/api/build/ConfiguredTargetLabel",children:"ConfiguredTargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/build/TargetLabel",children:"TargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/ConfiguredTargetNode",children:"bxl.ConfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/UnconfiguredTargetNode",children:"bxl.UnconfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/starlark/str",children:"str"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/ConfiguredTargetSet",children:"target_set"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/UnconfiguredTargetSet",children:"target_set"})," | list[",(0,a.jsx)(i.default,{to:"/docs/api/build/ConfiguredTargetLabel",children:"ConfiguredTargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/build/TargetLabel",children:"TargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/ConfiguredTargetNode",children:"bxl.ConfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/UnconfiguredTargetNode",children:"bxl.UnconfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/starlark/str",children:"str"}),"],\nfilter: None | ",(0,a.jsx)(i.default,{to:"/docs/api/starlark/str",children:"str"})," = None,\n) -> ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/ConfiguredTargetSet",children:"target_set"})]})}),"\n",(0,a.jsxs)(t.p,{children:["The ",(0,a.jsx)(t.code,{children:"allpaths"})," query for computing all dependency paths."]}),"\n",(0,a.jsx)(t.hr,{}),"\n",(0,a.jsx)(t.h2,{id:"cquerycontextattrfilter",children:"CqueryContext.attrfilter"}),"\n",(0,a.jsx)("pre",{class:"language-python",children:(0,a.jsxs)("code",{children:["def CqueryContext.attrfilter(\nattr: ",(0,a.jsx)(i.default,{to:"/docs/api/starlark/str",children:"str"}),",\nvalue: ",(0,a.jsx)(i.default,{to:"/docs/api/starlark/str",children:"str"}),",\ntargets: ",(0,a.jsx)(i.default,{to:"/docs/api/build/ConfiguredTargetLabel",children:"ConfiguredTargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/build/TargetLabel",children:"TargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/ConfiguredTargetNode",children:"bxl.ConfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/UnconfiguredTargetNode",children:"bxl.UnconfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/starlark/str",children:"str"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/ConfiguredTargetSet",children:"target_set"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/UnconfiguredTargetSet",children:"target_set"})," | list[",(0,a.jsx)(i.default,{to:"/docs/api/build/ConfiguredTargetLabel",children:"ConfiguredTargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/build/TargetLabel",children:"TargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/ConfiguredTargetNode",children:"bxl.ConfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/UnconfiguredTargetNode",children:"bxl.UnconfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/starlark/str",children:"str"}),"],\n) -> ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/ConfiguredTargetSet",children:"target_set"})]})}),"\n",(0,a.jsx)(t.p,{children:"The attrfilter query for rule attribute filtering."}),"\n",(0,a.jsx)(t.hr,{}),"\n",(0,a.jsx)(t.h2,{id:"cquerycontextattrregexfilter",children:"CqueryContext.attrregexfilter"}),"\n",(0,a.jsx)("pre",{class:"language-python",children:(0,a.jsxs)("code",{children:["def CqueryContext.attrregexfilter(\nattribute: ",(0,a.jsx)(i.default,{to:"/docs/api/starlark/str",children:"str"}),",\nvalue: ",(0,a.jsx)(i.default,{to:"/docs/api/starlark/str",children:"str"}),",\ntargets: ",(0,a.jsx)(i.default,{to:"/docs/api/build/ConfiguredTargetLabel",children:"ConfiguredTargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/build/TargetLabel",children:"TargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/ConfiguredTargetNode",children:"bxl.ConfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/UnconfiguredTargetNode",children:"bxl.UnconfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/starlark/str",children:"str"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/ConfiguredTargetSet",children:"target_set"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/UnconfiguredTargetSet",children:"target_set"})," | list[",(0,a.jsx)(i.default,{to:"/docs/api/build/ConfiguredTargetLabel",children:"ConfiguredTargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/build/TargetLabel",children:"TargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/ConfiguredTargetNode",children:"bxl.ConfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/UnconfiguredTargetNode",children:"bxl.UnconfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/starlark/str",children:"str"}),"],\n) -> ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/ConfiguredTargetSet",children:"target_set"})]})}),"\n",(0,a.jsx)(t.p,{children:"The attrregexfilter query for rule attribute filtering with regex."}),"\n",(0,a.jsx)(t.p,{children:"Sample usage:"}),"\n",(0,a.jsx)(t.pre,{children:(0,a.jsx)(t.code,{className:"language-python",children:'def _impl_attrregexfilter(ctx):\n    filtered = ctx.cquery().attrregexfilter("foo", "he.lo", "bin/kind/...")\n    ctx.output.print(filtered)\n'})}),"\n",(0,a.jsx)(t.hr,{}),"\n",(0,a.jsx)(t.h2,{id:"cquerycontextbuildfile",children:"CqueryContext.buildfile"}),"\n",(0,a.jsx)("pre",{class:"language-python",children:(0,a.jsxs)("code",{children:["def CqueryContext.buildfile(\ntargets: ",(0,a.jsx)(i.default,{to:"/docs/api/build/ConfiguredTargetLabel",children:"ConfiguredTargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/build/TargetLabel",children:"TargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/ConfiguredTargetNode",children:"bxl.ConfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/UnconfiguredTargetNode",children:"bxl.UnconfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/starlark/str",children:"str"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/ConfiguredTargetSet",children:"target_set"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/UnconfiguredTargetSet",children:"target_set"})," | list[",(0,a.jsx)(i.default,{to:"/docs/api/build/ConfiguredTargetLabel",children:"ConfiguredTargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/build/TargetLabel",children:"TargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/ConfiguredTargetNode",children:"bxl.ConfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/UnconfiguredTargetNode",children:"bxl.UnconfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/starlark/str",children:"str"}),"],\n) -> file_set"]})}),"\n",(0,a.jsx)(t.p,{children:"Find the build file(s) that defines a target or a target set."}),"\n",(0,a.jsx)(t.p,{children:"Sample usage:"}),"\n",(0,a.jsx)(t.pre,{children:(0,a.jsx)(t.code,{className:"language-python",children:'def _buildfile_impl(ctx):\n    owner = ctx.cquery().owner(["bin/TARGET", "bin/kind"])\n    result = ctx.cquery().buildfile(owner)\n    ctx.output.print(result)\n'})}),"\n",(0,a.jsx)(t.hr,{}),"\n",(0,a.jsx)(t.h2,{id:"cquerycontextdeps",children:"CqueryContext.deps"}),"\n",(0,a.jsx)("pre",{class:"language-python",children:(0,a.jsxs)("code",{children:["def CqueryContext.deps(\nuniverse: ",(0,a.jsx)(i.default,{to:"/docs/api/build/ConfiguredTargetLabel",children:"ConfiguredTargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/build/TargetLabel",children:"TargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/ConfiguredTargetNode",children:"bxl.ConfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/UnconfiguredTargetNode",children:"bxl.UnconfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/starlark/str",children:"str"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/ConfiguredTargetSet",children:"target_set"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/UnconfiguredTargetSet",children:"target_set"})," | list[",(0,a.jsx)(i.default,{to:"/docs/api/build/ConfiguredTargetLabel",children:"ConfiguredTargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/build/TargetLabel",children:"TargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/ConfiguredTargetNode",children:"bxl.ConfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/UnconfiguredTargetNode",children:"bxl.UnconfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/starlark/str",children:"str"}),"],\ndepth: None | ",(0,a.jsx)(i.default,{to:"/docs/api/starlark/int",children:"int"})," = None,\nfilter: None | ",(0,a.jsx)(i.default,{to:"/docs/api/starlark/str",children:"str"})," = None,\n) -> ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/ConfiguredTargetSet",children:"target_set"})]})}),"\n",(0,a.jsx)(t.p,{children:"The deps query for finding the transitive closure of dependencies."}),"\n",(0,a.jsx)(t.p,{children:"Sample usage:"}),"\n",(0,a.jsx)(t.pre,{children:(0,a.jsx)(t.code,{className:"language-python",children:'def _impl_deps(ctx):\n    result = ctx.cquery().deps("root//bin:the_binary", 1)\n    ctx.output.print(result)\n'})}),"\n",(0,a.jsx)(t.hr,{}),"\n",(0,a.jsx)(t.h2,{id:"cquerycontexteval",children:"CqueryContext.eval"}),"\n",(0,a.jsx)("pre",{class:"language-python",children:(0,a.jsxs)("code",{children:["def CqueryContext.eval(\nquery: ",(0,a.jsx)(i.default,{to:"/docs/api/starlark/str",children:"str"}),",\nquery_args: None | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/UnconfiguredTargetSet",children:"target_set"})," | list[",(0,a.jsx)(i.default,{to:"/docs/api/starlark/str",children:"str"}),"] = None,\ntarget_universe: None | list[",(0,a.jsx)(i.default,{to:"/docs/api/starlark/str",children:"str"}),"] | tuple[",(0,a.jsx)(i.default,{to:"/docs/api/starlark/str",children:"str"}),", ...] = None,\n)"]})}),"\n",(0,a.jsxs)(t.p,{children:["Evaluates some general query string. ",(0,a.jsx)(t.code,{children:"query_args"})," can be a target_set of unconfigured nodes, or a list of strings. Returns a ",(0,a.jsx)(t.code,{children:"dict"})," of target labels mapped to their ",(0,a.jsx)(t.code,{children:"target_set"})," results if ",(0,a.jsx)(t.code,{children:"query_args"})," was passed in, otherwise returns a single ",(0,a.jsx)(t.code,{children:"target_set"}),"."]}),"\n",(0,a.jsx)(t.p,{children:"Sample usage:"}),"\n",(0,a.jsx)(t.pre,{children:(0,a.jsx)(t.code,{className:"language-python",children:'def _impl_eval(ctx):\n    result1 = ctx.cquery().eval("inputs(root//bin:the_binary)")\n    ctx.output.print(result1)\n\n    result2 = ctx.cquery().eval("inputs(%s)", query_args = ["cell//path/to/file:target"])\n    ctx.output.print(result2)\n'})}),"\n",(0,a.jsx)(t.hr,{}),"\n",(0,a.jsx)(t.h2,{id:"cquerycontextfilter",children:"CqueryContext.filter"}),"\n",(0,a.jsx)("pre",{class:"language-python",children:(0,a.jsxs)("code",{children:["def CqueryContext.filter(\nregex: ",(0,a.jsx)(i.default,{to:"/docs/api/starlark/str",children:"str"}),",\ntargets: ",(0,a.jsx)(i.default,{to:"/docs/api/build/ConfiguredTargetLabel",children:"ConfiguredTargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/build/TargetLabel",children:"TargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/ConfiguredTargetNode",children:"bxl.ConfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/UnconfiguredTargetNode",children:"bxl.UnconfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/starlark/str",children:"str"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/ConfiguredTargetSet",children:"target_set"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/UnconfiguredTargetSet",children:"target_set"})," | list[",(0,a.jsx)(i.default,{to:"/docs/api/build/ConfiguredTargetLabel",children:"ConfiguredTargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/build/TargetLabel",children:"TargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/ConfiguredTargetNode",children:"bxl.ConfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/UnconfiguredTargetNode",children:"bxl.UnconfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/starlark/str",children:"str"}),"],\n) -> ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/ConfiguredTargetSet",children:"target_set"})]})}),"\n",(0,a.jsx)(t.p,{children:"The filter query for filtering targets by name."}),"\n",(0,a.jsx)(t.p,{children:"Sample usage:"}),"\n",(0,a.jsx)(t.pre,{children:(0,a.jsx)(t.code,{className:"language-python",children:'def _impl_filter(ctx):\n    result = ctx.cquery().filter(".*the_binary", "root//...")\n    ctx.output.print(result)\n'})}),"\n",(0,a.jsx)(t.hr,{}),"\n",(0,a.jsx)(t.h2,{id:"cquerycontextinputs",children:"CqueryContext.inputs"}),"\n",(0,a.jsx)("pre",{class:"language-python",children:(0,a.jsxs)("code",{children:["def CqueryContext.inputs(\ntargets: ",(0,a.jsx)(i.default,{to:"/docs/api/build/ConfiguredTargetLabel",children:"ConfiguredTargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/build/TargetLabel",children:"TargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/ConfiguredTargetNode",children:"bxl.ConfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/UnconfiguredTargetNode",children:"bxl.UnconfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/starlark/str",children:"str"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/ConfiguredTargetSet",children:"target_set"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/UnconfiguredTargetSet",children:"target_set"})," | list[",(0,a.jsx)(i.default,{to:"/docs/api/build/ConfiguredTargetLabel",children:"ConfiguredTargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/build/TargetLabel",children:"TargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/ConfiguredTargetNode",children:"bxl.ConfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/UnconfiguredTargetNode",children:"bxl.UnconfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/starlark/str",children:"str"}),"],\n) -> file_set"]})}),"\n",(0,a.jsx)(t.p,{children:"The inputs query for finding input files."}),"\n",(0,a.jsx)(t.p,{children:"Sample usage:"}),"\n",(0,a.jsx)(t.pre,{children:(0,a.jsx)(t.code,{className:"language-python",children:'def _impl_inputs(ctx):\n    result = ctx.cquery().inputs("root//bin:the_binary")\n    ctx.output.print(result)\n'})}),"\n",(0,a.jsx)(t.hr,{}),"\n",(0,a.jsx)(t.h2,{id:"cquerycontextkind",children:"CqueryContext.kind"}),"\n",(0,a.jsx)("pre",{class:"language-python",children:(0,a.jsxs)("code",{children:["def CqueryContext.kind(\nregex: ",(0,a.jsx)(i.default,{to:"/docs/api/starlark/str",children:"str"}),",\ntargets: ",(0,a.jsx)(i.default,{to:"/docs/api/build/ConfiguredTargetLabel",children:"ConfiguredTargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/build/TargetLabel",children:"TargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/ConfiguredTargetNode",children:"bxl.ConfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/UnconfiguredTargetNode",children:"bxl.UnconfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/starlark/str",children:"str"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/ConfiguredTargetSet",children:"target_set"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/UnconfiguredTargetSet",children:"target_set"})," | list[",(0,a.jsx)(i.default,{to:"/docs/api/build/ConfiguredTargetLabel",children:"ConfiguredTargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/build/TargetLabel",children:"TargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/ConfiguredTargetNode",children:"bxl.ConfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/UnconfiguredTargetNode",children:"bxl.UnconfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/starlark/str",children:"str"}),"],\n) -> ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/ConfiguredTargetSet",children:"target_set"})]})}),"\n",(0,a.jsxs)(t.p,{children:["Filter targets by rule type. Returns a subset of ",(0,a.jsx)(t.code,{children:"targets"})," where the rule type matches the specified ",(0,a.jsx)(t.code,{children:"regex"}),". The specified pattern can be a regular expression."]}),"\n",(0,a.jsx)(t.p,{children:"Sample usage:"}),"\n",(0,a.jsx)(t.pre,{children:(0,a.jsx)(t.code,{className:"language-python",children:'def _impl_kind(ctx):\n    kind = ctx.cquery().kind("cpp.*", "bin/libs/...")\n    ctx.output.print(nodes)\n'})}),"\n",(0,a.jsx)(t.hr,{}),"\n",(0,a.jsx)(t.h2,{id:"cquerycontextnattrfilter",children:"CqueryContext.nattrfilter"}),"\n",(0,a.jsx)("pre",{class:"language-python",children:(0,a.jsxs)("code",{children:["def CqueryContext.nattrfilter(\nattr: ",(0,a.jsx)(i.default,{to:"/docs/api/starlark/str",children:"str"}),",\nvalue: ",(0,a.jsx)(i.default,{to:"/docs/api/starlark/str",children:"str"}),",\ntargets: ",(0,a.jsx)(i.default,{to:"/docs/api/build/ConfiguredTargetLabel",children:"ConfiguredTargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/build/TargetLabel",children:"TargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/ConfiguredTargetNode",children:"bxl.ConfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/UnconfiguredTargetNode",children:"bxl.UnconfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/starlark/str",children:"str"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/ConfiguredTargetSet",children:"target_set"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/UnconfiguredTargetSet",children:"target_set"})," | list[",(0,a.jsx)(i.default,{to:"/docs/api/build/ConfiguredTargetLabel",children:"ConfiguredTargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/build/TargetLabel",children:"TargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/ConfiguredTargetNode",children:"bxl.ConfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/UnconfiguredTargetNode",children:"bxl.UnconfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/starlark/str",children:"str"}),"],\n) -> ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/ConfiguredTargetSet",children:"target_set"})]})}),"\n",(0,a.jsxs)(t.p,{children:["The nattrfilter query for rule attribute filtering. It is the opposite of ",(0,a.jsx)(t.code,{children:"attrfilter"}),", i.e. it filters targets by attribute but excludes those that match."]}),"\n",(0,a.jsx)(t.hr,{}),"\n",(0,a.jsx)(t.h2,{id:"cquerycontextowner",children:"CqueryContext.owner"}),"\n",(0,a.jsx)("pre",{class:"language-python",children:(0,a.jsxs)("code",{children:["def CqueryContext.owner(\nfiles: file_set | ",(0,a.jsx)(i.default,{to:"/docs/api/starlark/str",children:"str"})," | list[",(0,a.jsx)(i.default,{to:"/docs/api/starlark/str",children:"str"}),"] | tuple[",(0,a.jsx)(i.default,{to:"/docs/api/starlark/str",children:"str"}),", ...],\nuniverse: ",(0,a.jsx)(i.default,{to:"/docs/api/build/ConfiguredTargetLabel",children:"ConfiguredTargetLabel"})," | None | ",(0,a.jsx)(i.default,{to:"/docs/api/build/TargetLabel",children:"TargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/ConfiguredTargetNode",children:"bxl.ConfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/UnconfiguredTargetNode",children:"bxl.UnconfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/starlark/str",children:"str"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/ConfiguredTargetSet",children:"target_set"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/UnconfiguredTargetSet",children:"target_set"})," | list[",(0,a.jsx)(i.default,{to:"/docs/api/build/ConfiguredTargetLabel",children:"ConfiguredTargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/build/TargetLabel",children:"TargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/ConfiguredTargetNode",children:"bxl.ConfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/UnconfiguredTargetNode",children:"bxl.UnconfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/starlark/str",children:"str"}),"] = None,\n) -> ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/ConfiguredTargetSet",children:"target_set"})]})}),"\n",(0,a.jsxs)(t.p,{children:["The owner query for finding targets that own specified files. Note that if you do not pass in a cell path (where the format is ",(0,a.jsx)(t.code,{children:"<cell>//path/to/file"}),"), the path is resolved against the cell that the BXL script lives in. If you need to evaluate a file path that lives in a different cell, you must pass in the fully qualified cell path."]}),"\n",(0,a.jsx)(t.p,{children:"Sample usage:"}),"\n",(0,a.jsx)(t.pre,{children:(0,a.jsx)(t.code,{className:"language-python",children:'def _owner_impl(ctx):\n    owner = ctx.cquery().owner("bin/TARGETS.fixture", "foo//target/universe/...")\n    ctx.output.print(owner)\n'})}),"\n",(0,a.jsx)(t.hr,{}),"\n",(0,a.jsx)(t.h2,{id:"cquerycontextrdeps",children:"CqueryContext.rdeps"}),"\n",(0,a.jsx)("pre",{class:"language-python",children:(0,a.jsxs)("code",{children:["def CqueryContext.rdeps(\nuniverse: ",(0,a.jsx)(i.default,{to:"/docs/api/build/ConfiguredTargetLabel",children:"ConfiguredTargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/build/TargetLabel",children:"TargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/ConfiguredTargetNode",children:"bxl.ConfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/UnconfiguredTargetNode",children:"bxl.UnconfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/starlark/str",children:"str"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/ConfiguredTargetSet",children:"target_set"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/UnconfiguredTargetSet",children:"target_set"})," | list[",(0,a.jsx)(i.default,{to:"/docs/api/build/ConfiguredTargetLabel",children:"ConfiguredTargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/build/TargetLabel",children:"TargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/ConfiguredTargetNode",children:"bxl.ConfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/UnconfiguredTargetNode",children:"bxl.UnconfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/starlark/str",children:"str"}),"],\nfrom: ",(0,a.jsx)(i.default,{to:"/docs/api/build/ConfiguredTargetLabel",children:"ConfiguredTargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/build/TargetLabel",children:"TargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/ConfiguredTargetNode",children:"bxl.ConfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/UnconfiguredTargetNode",children:"bxl.UnconfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/starlark/str",children:"str"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/ConfiguredTargetSet",children:"target_set"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/UnconfiguredTargetSet",children:"target_set"})," | list[",(0,a.jsx)(i.default,{to:"/docs/api/build/ConfiguredTargetLabel",children:"ConfiguredTargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/build/TargetLabel",children:"TargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/ConfiguredTargetNode",children:"bxl.ConfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/UnconfiguredTargetNode",children:"bxl.UnconfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/starlark/str",children:"str"}),"],\ndepth: ",(0,a.jsx)(i.default,{to:"/docs/api/starlark/int",children:"int"})," = ...,\nfilter: None | ",(0,a.jsx)(i.default,{to:"/docs/api/starlark/str",children:"str"})," = None,\n) -> ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/ConfiguredTargetSet",children:"target_set"})]})}),"\n",(0,a.jsx)(t.p,{children:"The rdeps query for finding the transitive closure of reverse dependencies."}),"\n",(0,a.jsx)(t.p,{children:"Sample usage:"}),"\n",(0,a.jsx)(t.pre,{children:(0,a.jsx)(t.code,{className:"language-python",children:'def _impl_rdeps(ctx):\n    result = ctx.cquery().rdeps("root//bin:the_binary", "//lib:file1", 100)\n    ctx.output.print(result)\n'})}),"\n",(0,a.jsx)(t.hr,{}),"\n",(0,a.jsx)(t.h2,{id:"cquerycontextsomepath",children:"CqueryContext.somepath"}),"\n",(0,a.jsx)("pre",{class:"language-python",children:(0,a.jsxs)("code",{children:["def CqueryContext.somepath(\nfrom: ",(0,a.jsx)(i.default,{to:"/docs/api/build/ConfiguredTargetLabel",children:"ConfiguredTargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/build/TargetLabel",children:"TargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/ConfiguredTargetNode",children:"bxl.ConfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/UnconfiguredTargetNode",children:"bxl.UnconfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/starlark/str",children:"str"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/ConfiguredTargetSet",children:"target_set"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/UnconfiguredTargetSet",children:"target_set"})," | list[",(0,a.jsx)(i.default,{to:"/docs/api/build/ConfiguredTargetLabel",children:"ConfiguredTargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/build/TargetLabel",children:"TargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/ConfiguredTargetNode",children:"bxl.ConfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/UnconfiguredTargetNode",children:"bxl.UnconfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/starlark/str",children:"str"}),"],\nto: ",(0,a.jsx)(i.default,{to:"/docs/api/build/ConfiguredTargetLabel",children:"ConfiguredTargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/build/TargetLabel",children:"TargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/ConfiguredTargetNode",children:"bxl.ConfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/UnconfiguredTargetNode",children:"bxl.UnconfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/starlark/str",children:"str"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/ConfiguredTargetSet",children:"target_set"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/UnconfiguredTargetSet",children:"target_set"})," | list[",(0,a.jsx)(i.default,{to:"/docs/api/build/ConfiguredTargetLabel",children:"ConfiguredTargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/build/TargetLabel",children:"TargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/ConfiguredTargetNode",children:"bxl.ConfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/UnconfiguredTargetNode",children:"bxl.UnconfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/starlark/str",children:"str"}),"],\nfilter: None | ",(0,a.jsx)(i.default,{to:"/docs/api/starlark/str",children:"str"})," = None,\n) -> ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/ConfiguredTargetSet",children:"target_set"})]})}),"\n",(0,a.jsx)(t.hr,{}),"\n",(0,a.jsx)(t.h2,{id:"cquerycontexttestsof",children:"CqueryContext.testsof"}),"\n",(0,a.jsx)("pre",{class:"language-python",children:(0,a.jsxs)("code",{children:["def CqueryContext.testsof(\ntargets: ",(0,a.jsx)(i.default,{to:"/docs/api/build/ConfiguredTargetLabel",children:"ConfiguredTargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/build/TargetLabel",children:"TargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/ConfiguredTargetNode",children:"bxl.ConfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/UnconfiguredTargetNode",children:"bxl.UnconfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/starlark/str",children:"str"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/ConfiguredTargetSet",children:"target_set"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/UnconfiguredTargetSet",children:"target_set"})," | list[",(0,a.jsx)(i.default,{to:"/docs/api/build/ConfiguredTargetLabel",children:"ConfiguredTargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/build/TargetLabel",children:"TargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/ConfiguredTargetNode",children:"bxl.ConfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/UnconfiguredTargetNode",children:"bxl.UnconfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/starlark/str",children:"str"}),"],\n) -> ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/ConfiguredTargetSet",children:"target_set"})]})}),"\n",(0,a.jsx)(t.p,{children:"The testsof query for listing the tests of the specified targets."}),"\n",(0,a.jsx)(t.hr,{}),"\n",(0,a.jsx)(t.h2,{id:"cquerycontexttestsof_with_default_target_platform",children:"CqueryContext.testsof_with_default_target_platform"}),"\n",(0,a.jsx)("pre",{class:"language-python",children:(0,a.jsxs)("code",{children:["def CqueryContext.testsof_with_default_target_platform(\ntargets: ",(0,a.jsx)(i.default,{to:"/docs/api/build/ConfiguredTargetLabel",children:"ConfiguredTargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/build/TargetLabel",children:"TargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/ConfiguredTargetNode",children:"bxl.ConfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/UnconfiguredTargetNode",children:"bxl.UnconfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/starlark/str",children:"str"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/ConfiguredTargetSet",children:"target_set"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/UnconfiguredTargetSet",children:"target_set"})," | list[",(0,a.jsx)(i.default,{to:"/docs/api/build/ConfiguredTargetLabel",children:"ConfiguredTargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/build/TargetLabel",children:"TargetLabel"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/ConfiguredTargetNode",children:"bxl.ConfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/UnconfiguredTargetNode",children:"bxl.UnconfiguredTargetNode"})," | ",(0,a.jsx)(i.default,{to:"/docs/api/starlark/str",children:"str"}),"],\n) -> ",(0,a.jsx)(i.default,{to:"/docs/api/bxl/ConfiguredTargetSet",children:"target_set"})]})}),"\n",(0,a.jsx)(t.p,{children:"The testsof query for listing the tests of the specified targets. Performs default target platform resolution under the hood for the tests found."})]})}function g(e={}){const{wrapper:t}={...(0,l.R)(),...e.components};return t?(0,a.jsx)(t,{...e,children:(0,a.jsx)(u,{...e})}):u(e)}}}]);