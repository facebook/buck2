"use strict";(self.webpackChunkwebsite=self.webpackChunkwebsite||[]).push([[9702],{28453:(e,n,t)=>{t.d(n,{R:()=>c,x:()=>a});var s=t(96540);const o={},i=s.createContext(o);function c(e){const n=s.useContext(i);return s.useMemo((function(){return"function"==typeof e?e(n):{...n,...e}}),[n,e])}function a(e){let n;return n=e.disableParentContext?"function"==typeof e.components?e.components(o):e.components||o:c(e.components),s.createElement(i.Provider,{value:n},e.children)}},56549:(e,n,t)=>{t.r(n),t.d(n,{assets:()=>l,contentTitle:()=>r,default:()=>p,frontMatter:()=>a,metadata:()=>s,toc:()=>d});const s=JSON.parse('{"id":"api/bxl/Actions","title":"Actions","description":"The bxl action context is the context for creating actions. This context is obtained after performing execution platform resolution based on a set of given dependencies and toolchains.","source":"@site/../docs/api/bxl/Actions.md","sourceDirName":"api/bxl","slug":"/api/bxl/Actions","permalink":"/docs/api/bxl/Actions","draft":false,"unlisted":false,"tags":[],"version":"current","frontMatter":{},"sidebar":"apiSidebar","previous":{"title":"ActionQueryNode","permalink":"/docs/api/bxl/ActionQueryNode"},"next":{"title":"AnalysisResult","permalink":"/docs/api/bxl/AnalysisResult"}}');var o=t(74848),i=t(28453),c=t(56289);const a={},r="Actions",l={},d=[{value:"Actions.actions",id:"actionsactions",level:2},{value:"Actions.exec_deps",id:"actionsexec_deps",level:2},{value:"Actions.toolchains",id:"actionstoolchains",level:2}];function h(e){const n={h1:"h1",h2:"h2",header:"header",hr:"hr",p:"p",...(0,i.R)(),...e.components};return(0,o.jsxs)(o.Fragment,{children:[(0,o.jsx)(n.header,{children:(0,o.jsx)(n.h1,{id:"actions",children:"Actions"})}),"\n",(0,o.jsx)(n.p,{children:"The bxl action context is the context for creating actions. This context is obtained after performing execution platform resolution based on a set of given dependencies and toolchains."}),"\n",(0,o.jsx)(n.p,{children:"You can access the analysis actions to create actions, and the resolved dependencies and\ntoolchains from this context"}),"\n",(0,o.jsx)(n.h2,{id:"actionsactions",children:"Actions.actions"}),"\n",(0,o.jsx)("pre",{class:"language-python",children:(0,o.jsxs)("code",{children:["Actions.actions: ",(0,o.jsx)(c.default,{to:"/docs/api/build/AnalysisActions",children:"actions"})]})}),"\n",(0,o.jsx)(n.p,{children:"Gets the analysis action context to create and register actions on the execution platform corresponding to this bxl action's execution platform resolution."}),"\n",(0,o.jsx)(n.hr,{}),"\n",(0,o.jsx)(n.h2,{id:"actionsexec_deps",children:"Actions.exec_deps"}),"\n",(0,o.jsx)("pre",{class:"language-python",children:(0,o.jsxs)("code",{children:["Actions.exec_deps: dict[",(0,o.jsx)(c.default,{to:"/docs/api/build/ProvidersLabel",children:"providers_label"}),", ",(0,o.jsx)(c.default,{to:"/docs/api/build/Dependency",children:"Dependency"}),"]"]})}),"\n",(0,o.jsx)(n.p,{children:"Gets the execution deps requested correctly configured for the current execution platform"}),"\n",(0,o.jsx)(n.hr,{}),"\n",(0,o.jsx)(n.h2,{id:"actionstoolchains",children:"Actions.toolchains"}),"\n",(0,o.jsx)("pre",{class:"language-python",children:(0,o.jsxs)("code",{children:["Actions.toolchains: dict[",(0,o.jsx)(c.default,{to:"/docs/api/build/ProvidersLabel",children:"providers_label"}),", ",(0,o.jsx)(c.default,{to:"/docs/api/build/Dependency",children:"Dependency"}),"]"]})}),"\n",(0,o.jsx)(n.p,{children:"Gets the toolchains requested configured for the current execution platform"})]})}function p(e={}){const{wrapper:n}={...(0,i.R)(),...e.components};return n?(0,o.jsx)(n,{...e,children:(0,o.jsx)(h,{...e})}):h(e)}}}]);