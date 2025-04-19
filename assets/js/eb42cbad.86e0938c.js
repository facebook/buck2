"use strict";(self.webpackChunkwebsite=self.webpackChunkwebsite||[]).push([[7216],{28453:(e,n,t)=>{t.d(n,{R:()=>a,x:()=>o});var i=t(96540);const r={},s=i.createContext(r);function a(e){const n=i.useContext(s);return i.useMemo((function(){return"function"==typeof e?e(n):{...n,...e}}),[n,e])}function o(e){let n;return n=e.disableParentContext?"function"==typeof e.components?e.components(r):e.components||r:a(e.components),i.createElement(s.Provider,{value:n},e.children)}},47603:(e,n,t)=>{t.r(n),t.d(n,{assets:()=>c,contentTitle:()=>o,default:()=>u,frontMatter:()=>a,metadata:()=>i,toc:()=>l});const i=JSON.parse('{"id":"bxl/explanation/basics","title":"BXL Basics","description":"This page is a primer on common BXL functionalities and data types. Ramping up","source":"@site/../docs/bxl/explanation/basics.md","sourceDirName":"bxl/explanation","slug":"/bxl/explanation/basics","permalink":"/docs/bxl/explanation/basics","draft":false,"unlisted":false,"tags":[],"version":"current","frontMatter":{"id":"basics","title":"BXL Basics"},"sidebar":"main","previous":{"title":"How to Collect Telemetry Events","permalink":"/docs/bxl/how_tos/how_to_collect_telemetry_events"},"next":{"title":"Understanding Labels and Nodes in Buck2","permalink":"/docs/bxl/explanation/labels_and_nodes"}}');var r=t(74848),s=t(28453);const a={id:"basics",title:"BXL Basics"},o=void 0,c={},l=[{value:"Common BXL functionalities",id:"common-bxl-functionalities",level:2},{value:"Build",id:"build",level:3},{value:"Analysis",id:"analysis",level:3},{value:"Query",id:"query",level:3},{value:"Uquery",id:"uquery",level:4},{value:"Cquery",id:"cquery",level:4},{value:"Aquery",id:"aquery",level:4},{value:"Actions",id:"actions",level:3},{value:"Ensure",id:"ensure",level:3}];function d(e){const n={a:"a",code:"code",h2:"h2",h3:"h3",h4:"h4",li:"li",p:"p",ul:"ul",...(0,s.R)(),...e.components};return(0,r.jsxs)(r.Fragment,{children:[(0,r.jsxs)(n.p,{children:["This page is a primer on common BXL functionalities and data types. Ramping up\nin BXL may be challenging without much prior knowledge of Buck2 building blocks\n(ex: targets, configurations, queries), so please take a look at the\n",(0,r.jsx)(n.a,{href:"/docs/concepts/concept_map",children:"Concepts"})," documentation before reading on."]}),"\n",(0,r.jsx)(n.h2,{id:"common-bxl-functionalities",children:"Common BXL functionalities"}),"\n",(0,r.jsx)(n.h3,{id:"build",children:"Build"}),"\n",(0,r.jsxs)(n.p,{children:["You can build targets within BXL with\n",(0,r.jsx)(n.a,{href:"../../../api/bxl/Context/#contextbuild",children:(0,r.jsx)(n.code,{children:"ctx.build()"})}),". The result is a\n",(0,r.jsx)(n.a,{href:"../../../api/bxl/BuildResult",children:(0,r.jsx)(n.code,{children:"bxl.BuildResult"})}),", which has ",(0,r.jsx)(n.code,{children:"artifacts()"})," and\n",(0,r.jsx)(n.code,{children:"failures()"})," functions that provide iterators to the artifacts or failures,\nrespectively. You can pass in a single target or target pattern to build."]}),"\n",(0,r.jsx)(n.h3,{id:"analysis",children:"Analysis"}),"\n",(0,r.jsxs)(n.p,{children:["You can run analysis on targets within BXL via\n",(0,r.jsx)(n.a,{href:"../../../api/bxl/Context/#contextanalysis",children:(0,r.jsx)(n.code,{children:"ctx.analysis()"})}),". Analysis means to\nevaluate the underlying rule implementation for the inputted targets, and\nproduce the providers that the rule defined for the target. A common workflow is\nto inspect the resulting providers, and perhaps ensure parts of these providers\nor run actions using information from the providers (see ",(0,r.jsx)(n.a,{href:"#actions",children:"Actions"}),"\nbelow)."]}),"\n",(0,r.jsx)(n.h3,{id:"query",children:"Query"}),"\n",(0,r.jsxs)(n.p,{children:["Buck2 supports a couple different query types: querying the unconfigured graph\n(",(0,r.jsx)(n.code,{children:"buck2 uquery"}),"), the configured graph (",(0,r.jsx)(n.code,{children:"buck2 cquery"}),"), or the action graph\n(",(0,r.jsx)(n.code,{children:"buck2 aquery"}),"). These queries are all available in BXL as well:"]}),"\n",(0,r.jsxs)(n.ul,{children:["\n",(0,r.jsxs)(n.li,{children:[(0,r.jsx)(n.code,{children:"ctx.uquery()"})," returns a ",(0,r.jsx)(n.a,{href:"../../../api/bxl/UqueryContext",children:(0,r.jsx)(n.code,{children:"bxl.UqueryContext"})})]}),"\n",(0,r.jsxs)(n.li,{children:[(0,r.jsx)(n.code,{children:"ctx.cquery()"})," returns a ",(0,r.jsx)(n.a,{href:"../../../api/bxl/CqueryContext",children:(0,r.jsx)(n.code,{children:"bxl.CqueryContext"})})]}),"\n",(0,r.jsxs)(n.li,{children:[(0,r.jsx)(n.code,{children:"ctx.aquery()"})," returns a ",(0,r.jsx)(n.a,{href:"../../../api/bxl/AqueryContext",children:(0,r.jsx)(n.code,{children:"bxl.AqueryContext"})})]}),"\n"]}),"\n",(0,r.jsxs)(n.p,{children:["You can read more about the individual queries in the API docs. There are many\nqueries that are common between uquery, cquery, and aquery, but cquery and\naquery will have extra queries unique to the configured graph or the action\ngraph. One more thing to call out is the ",(0,r.jsx)(n.code,{children:"eval()"})," query, which is a special\nquery that takes in the entire query as a string literal. A common use for\n",(0,r.jsx)(n.code,{children:"eval()"})," is to migrate a complex query from Buck2 CLI to BXL by dropping the\nentire query string directly into ",(0,r.jsx)(n.code,{children:"eval()"}),"."]}),"\n",(0,r.jsxs)(n.p,{children:["The query results are target sets (iterable container) of\n",(0,r.jsxs)(n.a,{href:"../../../api/bxl/UnconfiguredTargetNode",children:[(0,r.jsx)(n.code,{children:"bxl.UnconfiguredTargetNode"}),"s"]})," for\nuquery, ",(0,r.jsxs)(n.a,{href:"../../../api/bxl/ConfiguredTargetNode",children:[(0,r.jsx)(n.code,{children:"bxl.ConfiguredTargetNode"}),"s"]})," for\ncquery, and ",(0,r.jsxs)(n.a,{href:"../../../api/bxl/ActionQueryNode",children:[(0,r.jsx)(n.code,{children:"bxl.ActionQueryNode"}),"s"]})," for\naquery. Each of these node types have accessors on their attributes. A common\nworkflow is to run some query in BXL, and iterate through the resulting nodes to\ninspect their attributes, and use those attributes to inform further\ncomputations in BXL."]}),"\n",(0,r.jsx)(n.h4,{id:"uquery",children:"Uquery"}),"\n",(0,r.jsx)(n.p,{children:"Querying the unconfigured graph means that no configurations (such as platforms\nand transitions) have been applied to the target graph yet. This means that it's\nvery possible that some parts of the target graph is broken due to lack of\nconfigurations. Generally to avoid this problem, cquery may be preferred\ninstead."}),"\n",(0,r.jsx)(n.h4,{id:"cquery",children:"Cquery"}),"\n",(0,r.jsxs)(n.p,{children:["Querying the configured graph means that configurations have been applied to the\ntarget graph. For cquery, we require that users use a\n",(0,r.jsx)(n.a,{href:"../../how_tos/how_to_use_target_universe",children:"target universe"})," for their query\ninputs."]}),"\n",(0,r.jsx)(n.h4,{id:"aquery",children:"Aquery"}),"\n",(0,r.jsx)(n.p,{children:"Aquery is a quite different from uquery and cquery. It is used to query the\naction graph, which is constructed after Buck2 runs analysis on the targets and\nproduces the list of providers and actions needed to build the target."}),"\n",(0,r.jsx)(n.h3,{id:"actions",children:"Actions"}),"\n",(0,r.jsxs)(n.p,{children:["You can create actions directly within the BXL API. The available action APIs\nare equivalent to the ones found on the\n",(0,r.jsx)(n.a,{href:"../../../api/build/AnalysisActions",children:(0,r.jsx)(n.code,{children:"AnalysisActions"})})," type for normal rules,\nwith the caveat that\n",(0,r.jsx)(n.a,{href:"../../how_tos/how_to_run_actions_based_on_the_content_of_artifact",children:"dynamic actions"}),"\nuse the ",(0,r.jsx)(n.a,{href:"../../../api/bxl/Context",children:(0,r.jsx)(n.code,{children:"bxl.Context"})})," (which provides richer\nfunctionalities)."]}),"\n",(0,r.jsxs)(n.p,{children:["A common workflow would be to run analysis on a target, and use some interesting\nbits found in the analysis result to construct an augmented\n",(0,r.jsx)(n.a,{href:"../../../api/build#cmd_args",children:(0,r.jsx)(n.code,{children:"cmd_args"})})," to run, and then ensure the action's\noutput (see below for ensuring). Also see\n",(0,r.jsx)(n.a,{href:"../../how_tos/basic_how_tos#running-actions",children:"Running actions"}),"."]}),"\n",(0,r.jsx)(n.h3,{id:"ensure",children:"Ensure"}),"\n",(0,r.jsxs)(n.p,{children:["Ensuring an artifact means that you want the artifact to be materialized\n(meaning, downloaded to your machine) at the end of the BXL execution. There are\ntwo APIs for ensuring: ",(0,r.jsx)(n.code,{children:"ctx.output.ensure()"})," and ",(0,r.jsx)(n.code,{children:"ctx.output.ensure_multiple()"}),"\n(see ",(0,r.jsx)(n.a,{href:"../../../api/bxl/OutputStream",children:(0,r.jsx)(n.code,{children:"bxl.OutputStream"})}),"). As the naming\nindicates, the former is for ensuring a single artifact, and the latter is for\nensuring multiple artifact-like inputs. Artifact-like inputs include\n",(0,r.jsx)(n.a,{href:"../../../api/build#cmd_args",children:(0,r.jsx)(n.code,{children:"cmd_args"})})," (can be found when inspecting\nproviders), ",(0,r.jsx)(n.a,{href:"../../../api/bxl/BuildResult",children:(0,r.jsx)(n.code,{children:"bxl.BuildResult"})})," (produced when\nbuilding something in BXL), or ",(0,r.jsx)(n.a,{href:"../../../api/build/Artifact",children:(0,r.jsx)(n.code,{children:"artifact"})})," (can be\nfound when inspecting providers, or creating your own actions)."]}),"\n",(0,r.jsxs)(n.p,{children:["A common workflow is to ensure an artifact that you created via some custom\nactions defined in your script, or ensuring some artifacts found in the\nproviders after running analysis. Also see\n",(0,r.jsx)(n.a,{href:"../../faq#what-do-i-need-to-know-about-ensured-artifacts",children:"What do I need to know about ensured artifacts"}),"."]})]})}function u(e={}){const{wrapper:n}={...(0,s.R)(),...e.components};return n?(0,r.jsx)(n,{...e,children:(0,r.jsx)(d,{...e})}):d(e)}}}]);