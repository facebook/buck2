"use strict";(self.webpackChunkwebsite=self.webpackChunkwebsite||[]).push([[8543],{28453:(e,t,r)=>{r.d(t,{R:()=>o,x:()=>i});var a=r(96540);const d={},l=a.createContext(d);function o(e){const t=a.useContext(l);return a.useMemo((function(){return"function"==typeof e?e(t):{...t,...e}}),[t,e])}function i(e){let t;return t=e.disableParentContext?"function"==typeof e.components?e.components(d):e.components||d:o(e.components),a.createElement(l.Provider,{value:t},e.children)}},58479:(e,t,r)=>{r.r(t),r.d(t,{assets:()=>n,contentTitle:()=>s,default:()=>x,frontMatter:()=>i,metadata:()=>a,toc:()=>c});const a=JSON.parse('{"id":"api/bxl/AqueryContext","title":"AqueryContext","description":"The context for performing aquery operations in bxl. The functions offered on this ctx are the same behaviour as the query functions available within aquery command.","source":"@site/../docs/api/bxl/AqueryContext.md","sourceDirName":"api/bxl","slug":"/api/bxl/AqueryContext","permalink":"/docs/api/bxl/AqueryContext","draft":false,"unlisted":false,"tags":[],"version":"current","frontMatter":{},"sidebar":"apiSidebar","previous":{"title":"AnalysisResult","permalink":"/docs/api/bxl/AnalysisResult"},"next":{"title":"AuditContext","permalink":"/docs/api/bxl/AuditContext"}}');var d=r(74848),l=r(28453),o=r(56289);const i={},s="AqueryContext",n={},c=[{value:"AqueryContext.all_actions",id:"aquerycontextall_actions",level:2},{value:"AqueryContext.all_outputs",id:"aquerycontextall_outputs",level:2},{value:"AqueryContext.attrfilter",id:"aquerycontextattrfilter",level:2},{value:"AqueryContext.deps",id:"aquerycontextdeps",level:2},{value:"AqueryContext.eval",id:"aquerycontexteval",level:2}];function u(e){const t={code:"code",h1:"h1",h2:"h2",header:"header",hr:"hr",p:"p",pre:"pre",...(0,l.R)(),...e.components};return(0,d.jsxs)(d.Fragment,{children:[(0,d.jsx)(t.header,{children:(0,d.jsx)(t.h1,{id:"aquerycontext",children:"AqueryContext"})}),"\n",(0,d.jsxs)(t.p,{children:["The context for performing ",(0,d.jsx)(t.code,{children:"aquery"})," operations in bxl. The functions offered on this ctx are the same behaviour as the query functions available within aquery command."]}),"\n",(0,d.jsxs)(t.p,{children:["Query results are ",(0,d.jsx)(t.code,{children:"target_set"}),"s of ",(0,d.jsx)(t.code,{children:"action_query_node"}),"s, which supports iteration,\nindexing, ",(0,d.jsx)(t.code,{children:"len()"}),", set addition/subtraction, and ",(0,d.jsx)(t.code,{children:"equals()"}),"."]}),"\n",(0,d.jsx)(t.h2,{id:"aquerycontextall_actions",children:"AqueryContext.all_actions"}),"\n",(0,d.jsx)("pre",{class:"language-python",children:(0,d.jsxs)("code",{children:["def AqueryContext.all_actions(\ntargets: ",(0,d.jsx)(o.default,{to:"/docs/api/build/ConfiguredTargetLabel",children:"ConfiguredTargetLabel"})," | ",(0,d.jsx)(o.default,{to:"/docs/api/build/TargetLabel",children:"TargetLabel"})," | ",(0,d.jsx)(o.default,{to:"/docs/api/bxl/ConfiguredTargetNode",children:"bxl.ConfiguredTargetNode"})," | ",(0,d.jsx)(o.default,{to:"/docs/api/bxl/UnconfiguredTargetNode",children:"bxl.UnconfiguredTargetNode"})," | ",(0,d.jsx)(o.default,{to:"/docs/api/build/Label",children:"label"})," | ",(0,d.jsx)(o.default,{to:"/docs/api/build/ProvidersLabel",children:"providers_label"})," | ",(0,d.jsx)(o.default,{to:"/docs/api/starlark/str",children:"str"})," | target_set | ",(0,d.jsx)(o.default,{to:"/docs/api/bxl/UnconfiguredTargetSet",children:"target_set"})," | ",(0,d.jsx)(o.default,{to:"/docs/api/bxl/ConfiguredTargetSet",children:"target_set"})," | ",(0,d.jsx)(o.default,{to:"/docs/api/bxl/UnconfiguredTargetSet",children:"target_set"})," | list[",(0,d.jsx)(o.default,{to:"/docs/api/build/ConfiguredTargetLabel",children:"ConfiguredTargetLabel"})," | ",(0,d.jsx)(o.default,{to:"/docs/api/build/TargetLabel",children:"TargetLabel"})," | ",(0,d.jsx)(o.default,{to:"/docs/api/bxl/ActionQueryNode",children:"bxl.ActionQueryNode"})," | ",(0,d.jsx)(o.default,{to:"/docs/api/bxl/ConfiguredTargetNode",children:"bxl.ConfiguredTargetNode"})," | ",(0,d.jsx)(o.default,{to:"/docs/api/bxl/UnconfiguredTargetNode",children:"bxl.UnconfiguredTargetNode"})," | ",(0,d.jsx)(o.default,{to:"/docs/api/build/Label",children:"label"})," | ",(0,d.jsx)(o.default,{to:"/docs/api/build/ProvidersLabel",children:"providers_label"})," | ",(0,d.jsx)(o.default,{to:"/docs/api/starlark/str",children:"str"}),"],\n) -> target_set"]})}),"\n",(0,d.jsx)(t.p,{children:"Obtain all the actions declared within the analysis of a given target."}),"\n",(0,d.jsx)(t.p,{children:"This operation only makes sense on a target literal (it is a simple passthrough when passed\nan action)."}),"\n",(0,d.jsx)(t.hr,{}),"\n",(0,d.jsx)(t.h2,{id:"aquerycontextall_outputs",children:"AqueryContext.all_outputs"}),"\n",(0,d.jsx)("pre",{class:"language-python",children:(0,d.jsxs)("code",{children:["def AqueryContext.all_outputs(\ntargets: ",(0,d.jsx)(o.default,{to:"/docs/api/build/ConfiguredTargetLabel",children:"ConfiguredTargetLabel"})," | ",(0,d.jsx)(o.default,{to:"/docs/api/build/TargetLabel",children:"TargetLabel"})," | ",(0,d.jsx)(o.default,{to:"/docs/api/bxl/ConfiguredTargetNode",children:"bxl.ConfiguredTargetNode"})," | ",(0,d.jsx)(o.default,{to:"/docs/api/bxl/UnconfiguredTargetNode",children:"bxl.UnconfiguredTargetNode"})," | ",(0,d.jsx)(o.default,{to:"/docs/api/build/Label",children:"label"})," | ",(0,d.jsx)(o.default,{to:"/docs/api/build/ProvidersLabel",children:"providers_label"})," | ",(0,d.jsx)(o.default,{to:"/docs/api/starlark/str",children:"str"})," | target_set | ",(0,d.jsx)(o.default,{to:"/docs/api/bxl/UnconfiguredTargetSet",children:"target_set"})," | ",(0,d.jsx)(o.default,{to:"/docs/api/bxl/ConfiguredTargetSet",children:"target_set"})," | ",(0,d.jsx)(o.default,{to:"/docs/api/bxl/UnconfiguredTargetSet",children:"target_set"})," | list[",(0,d.jsx)(o.default,{to:"/docs/api/build/ConfiguredTargetLabel",children:"ConfiguredTargetLabel"})," | ",(0,d.jsx)(o.default,{to:"/docs/api/build/TargetLabel",children:"TargetLabel"})," | ",(0,d.jsx)(o.default,{to:"/docs/api/bxl/ActionQueryNode",children:"bxl.ActionQueryNode"})," | ",(0,d.jsx)(o.default,{to:"/docs/api/bxl/ConfiguredTargetNode",children:"bxl.ConfiguredTargetNode"})," | ",(0,d.jsx)(o.default,{to:"/docs/api/bxl/UnconfiguredTargetNode",children:"bxl.UnconfiguredTargetNode"})," | ",(0,d.jsx)(o.default,{to:"/docs/api/build/Label",children:"label"})," | ",(0,d.jsx)(o.default,{to:"/docs/api/build/ProvidersLabel",children:"providers_label"})," | ",(0,d.jsx)(o.default,{to:"/docs/api/starlark/str",children:"str"}),"],\n) -> target_set"]})}),"\n",(0,d.jsxs)(t.p,{children:["Obtain the actions for all the outputs provided by the ",(0,d.jsx)(t.code,{children:"DefaultInfo"})," for the targets passed as input. This includes both the ",(0,d.jsx)(t.code,{children:"default_outputs"})," and ",(0,d.jsx)(t.code,{children:"other_outputs"}),"."]}),"\n",(0,d.jsx)(t.p,{children:"This operation only makes sense on a target literal (it does nothing if passed something\nelse)."}),"\n",(0,d.jsx)(t.hr,{}),"\n",(0,d.jsx)(t.h2,{id:"aquerycontextattrfilter",children:"AqueryContext.attrfilter"}),"\n",(0,d.jsx)("pre",{class:"language-python",children:(0,d.jsxs)("code",{children:["def AqueryContext.attrfilter(\nattr: ",(0,d.jsx)(o.default,{to:"/docs/api/starlark/str",children:"str"}),",\nvalue: ",(0,d.jsx)(o.default,{to:"/docs/api/starlark/str",children:"str"}),",\ntargets: ",(0,d.jsx)(o.default,{to:"/docs/api/build/ConfiguredTargetLabel",children:"ConfiguredTargetLabel"})," | ",(0,d.jsx)(o.default,{to:"/docs/api/build/TargetLabel",children:"TargetLabel"})," | ",(0,d.jsx)(o.default,{to:"/docs/api/bxl/ConfiguredTargetNode",children:"bxl.ConfiguredTargetNode"})," | ",(0,d.jsx)(o.default,{to:"/docs/api/bxl/UnconfiguredTargetNode",children:"bxl.UnconfiguredTargetNode"})," | ",(0,d.jsx)(o.default,{to:"/docs/api/build/Label",children:"label"})," | ",(0,d.jsx)(o.default,{to:"/docs/api/build/ProvidersLabel",children:"providers_label"})," | ",(0,d.jsx)(o.default,{to:"/docs/api/starlark/str",children:"str"})," | target_set | ",(0,d.jsx)(o.default,{to:"/docs/api/bxl/UnconfiguredTargetSet",children:"target_set"})," | ",(0,d.jsx)(o.default,{to:"/docs/api/bxl/ConfiguredTargetSet",children:"target_set"})," | ",(0,d.jsx)(o.default,{to:"/docs/api/bxl/UnconfiguredTargetSet",children:"target_set"})," | list[",(0,d.jsx)(o.default,{to:"/docs/api/build/ConfiguredTargetLabel",children:"ConfiguredTargetLabel"})," | ",(0,d.jsx)(o.default,{to:"/docs/api/build/TargetLabel",children:"TargetLabel"})," | ",(0,d.jsx)(o.default,{to:"/docs/api/bxl/ActionQueryNode",children:"bxl.ActionQueryNode"})," | ",(0,d.jsx)(o.default,{to:"/docs/api/bxl/ConfiguredTargetNode",children:"bxl.ConfiguredTargetNode"})," | ",(0,d.jsx)(o.default,{to:"/docs/api/bxl/UnconfiguredTargetNode",children:"bxl.UnconfiguredTargetNode"})," | ",(0,d.jsx)(o.default,{to:"/docs/api/build/Label",children:"label"})," | ",(0,d.jsx)(o.default,{to:"/docs/api/build/ProvidersLabel",children:"providers_label"})," | ",(0,d.jsx)(o.default,{to:"/docs/api/starlark/str",children:"str"}),"],\n) -> target_set"]})}),"\n",(0,d.jsx)(t.p,{children:"The attrfilter query for rule attribute filtering."}),"\n",(0,d.jsx)(t.hr,{}),"\n",(0,d.jsx)(t.h2,{id:"aquerycontextdeps",children:"AqueryContext.deps"}),"\n",(0,d.jsx)("pre",{class:"language-python",children:(0,d.jsxs)("code",{children:["def AqueryContext.deps(\nuniverse: ",(0,d.jsx)(o.default,{to:"/docs/api/build/ConfiguredTargetLabel",children:"ConfiguredTargetLabel"})," | ",(0,d.jsx)(o.default,{to:"/docs/api/build/TargetLabel",children:"TargetLabel"})," | ",(0,d.jsx)(o.default,{to:"/docs/api/bxl/ConfiguredTargetNode",children:"bxl.ConfiguredTargetNode"})," | ",(0,d.jsx)(o.default,{to:"/docs/api/bxl/UnconfiguredTargetNode",children:"bxl.UnconfiguredTargetNode"})," | ",(0,d.jsx)(o.default,{to:"/docs/api/build/Label",children:"label"})," | ",(0,d.jsx)(o.default,{to:"/docs/api/build/ProvidersLabel",children:"providers_label"})," | ",(0,d.jsx)(o.default,{to:"/docs/api/starlark/str",children:"str"})," | target_set | ",(0,d.jsx)(o.default,{to:"/docs/api/bxl/UnconfiguredTargetSet",children:"target_set"})," | ",(0,d.jsx)(o.default,{to:"/docs/api/bxl/ConfiguredTargetSet",children:"target_set"})," | ",(0,d.jsx)(o.default,{to:"/docs/api/bxl/UnconfiguredTargetSet",children:"target_set"})," | list[",(0,d.jsx)(o.default,{to:"/docs/api/build/ConfiguredTargetLabel",children:"ConfiguredTargetLabel"})," | ",(0,d.jsx)(o.default,{to:"/docs/api/build/TargetLabel",children:"TargetLabel"})," | ",(0,d.jsx)(o.default,{to:"/docs/api/bxl/ActionQueryNode",children:"bxl.ActionQueryNode"})," | ",(0,d.jsx)(o.default,{to:"/docs/api/bxl/ConfiguredTargetNode",children:"bxl.ConfiguredTargetNode"})," | ",(0,d.jsx)(o.default,{to:"/docs/api/bxl/UnconfiguredTargetNode",children:"bxl.UnconfiguredTargetNode"})," | ",(0,d.jsx)(o.default,{to:"/docs/api/build/Label",children:"label"})," | ",(0,d.jsx)(o.default,{to:"/docs/api/build/ProvidersLabel",children:"providers_label"})," | ",(0,d.jsx)(o.default,{to:"/docs/api/starlark/str",children:"str"}),"],\ndepth: None | ",(0,d.jsx)(o.default,{to:"/docs/api/starlark/int",children:"int"})," = None,\nfilter: None | ",(0,d.jsx)(o.default,{to:"/docs/api/starlark/str",children:"str"})," = None,\n) -> target_set"]})}),"\n",(0,d.jsx)(t.p,{children:"The deps query for finding the transitive closure of dependencies."}),"\n",(0,d.jsx)(t.hr,{}),"\n",(0,d.jsx)(t.h2,{id:"aquerycontexteval",children:"AqueryContext.eval"}),"\n",(0,d.jsx)("pre",{class:"language-python",children:(0,d.jsxs)("code",{children:["def AqueryContext.eval(\nquery: ",(0,d.jsx)(o.default,{to:"/docs/api/starlark/str",children:"str"}),",\nquery_args: None | ",(0,d.jsx)(o.default,{to:"/docs/api/bxl/UnconfiguredTargetSet",children:"target_set"})," | list[",(0,d.jsx)(o.default,{to:"/docs/api/starlark/str",children:"str"}),"] = None,\n)"]})}),"\n",(0,d.jsxs)(t.p,{children:["Evaluates some general query string. ",(0,d.jsx)(t.code,{children:"query_args"})," can be a target_set of unconfigured nodes, or a list of strings. Returns a ",(0,d.jsx)(t.code,{children:"dict"})," of target labels mapped to their ",(0,d.jsx)(t.code,{children:"target_set"})," results if ",(0,d.jsx)(t.code,{children:"query_args"})," was passed in, otherwise returns a single ",(0,d.jsx)(t.code,{children:"target_set"}),"."]}),"\n",(0,d.jsx)(t.p,{children:"Sample usage:"}),"\n",(0,d.jsx)(t.pre,{children:(0,d.jsx)(t.code,{className:"language-python",children:'def _impl_eval(ctx):\n    result = ctx.aquery().eval(":foo")\n    ctx.output.print(result)\n'})})]})}function x(e={}){const{wrapper:t}={...(0,l.R)(),...e.components};return t?(0,d.jsx)(t,{...e,children:(0,d.jsx)(u,{...e})}):u(e)}}}]);