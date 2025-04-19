"use strict";(self.webpackChunkwebsite=self.webpackChunkwebsite||[]).push([[8804],{23598:(e,l,a)=>{a.r(l),a.d(l,{assets:()=>d,contentTitle:()=>o,default:()=>b,frontMatter:()=>s,metadata:()=>r,toc:()=>i});const r=JSON.parse('{"id":"api/build/Label","title":"Label","description":"A label is used to represent a configured target.","source":"@site/../docs/api/build/Label.md","sourceDirName":"api/build","slug":"/api/build/Label","permalink":"/docs/api/build/Label","draft":false,"unlisted":false,"tags":[],"version":"current","frontMatter":{},"sidebar":"apiSidebar","previous":{"title":"ExternalRunnerTestInfo","permalink":"/docs/api/build/ExternalRunnerTestInfo"},"next":{"title":"LocalResourceInfo","permalink":"/docs/api/build/LocalResourceInfo"}}');var t=a(74848),n=a(28453),c=a(56289);const s={},o="Label",d={},i=[{value:"Label.cell",id:"labelcell",level:2},{value:"Label.cell_root",id:"labelcell_root",level:2},{value:"Label.configured_target",id:"labelconfigured_target",level:2},{value:"Label.name",id:"labelname",level:2},{value:"Label.package",id:"labelpackage",level:2},{value:"Label.path",id:"labelpath",level:2},{value:"Label.project_root",id:"labelproject_root",level:2},{value:"Label.raw_target",id:"labelraw_target",level:2},{value:"Label.sub_target",id:"labelsub_target",level:2}];function h(e){const l={code:"code",h1:"h1",h2:"h2",header:"header",hr:"hr",p:"p",...(0,n.R)(),...e.components};return(0,t.jsxs)(t.Fragment,{children:[(0,t.jsx)(l.header,{children:(0,t.jsx)(l.h1,{id:"label",children:"Label"})}),"\n",(0,t.jsx)(l.p,{children:"A label is used to represent a configured target."}),"\n",(0,t.jsx)(l.h2,{id:"labelcell",children:"Label.cell"}),"\n",(0,t.jsx)("pre",{class:"language-python",children:(0,t.jsxs)("code",{children:["Label.cell: ",(0,t.jsx)(c.default,{to:"/docs/api/starlark/str",children:"str"})]})}),"\n",(0,t.jsxs)(l.p,{children:["For the label ",(0,t.jsx)(l.code,{children:"fbcode//buck2/hello:world (ovr_config//platform/linux:x86_64-fbcode-46b26edb4b80a905)"})," this gives back ",(0,t.jsx)(l.code,{children:"fbcode"})]}),"\n",(0,t.jsx)(l.hr,{}),"\n",(0,t.jsx)(l.h2,{id:"labelcell_root",children:"Label.cell_root"}),"\n",(0,t.jsx)("pre",{class:"language-python",children:(0,t.jsxs)("code",{children:["Label.cell_root: ",(0,t.jsx)(c.default,{to:"/docs/api/build/CellRoot",children:"CellRoot"})]})}),"\n",(0,t.jsxs)(l.p,{children:["Obtain a reference to this target label's cell root. This can be used as if it were an artifact in places that expect one, such as ",(0,t.jsx)(l.code,{children:"cmd_args().relative_to"}),"."]}),"\n",(0,t.jsx)(l.hr,{}),"\n",(0,t.jsx)(l.h2,{id:"labelconfigured_target",children:"Label.configured_target"}),"\n",(0,t.jsx)("pre",{class:"language-python",children:(0,t.jsxs)("code",{children:["def Label.configured_target(\n) -> ",(0,t.jsx)(c.default,{to:"/docs/api/build/ConfiguredTargetLabel",children:"ConfiguredTargetLabel"})]})}),"\n",(0,t.jsx)(l.p,{children:"Returns the underlying configured target label, dropping the sub target"}),"\n",(0,t.jsx)(l.hr,{}),"\n",(0,t.jsx)(l.h2,{id:"labelname",children:"Label.name"}),"\n",(0,t.jsx)("pre",{class:"language-python",children:(0,t.jsxs)("code",{children:["Label.name: ",(0,t.jsx)(c.default,{to:"/docs/api/starlark/str",children:"str"})]})}),"\n",(0,t.jsxs)(l.p,{children:["For the label ",(0,t.jsx)(l.code,{children:"fbcode//buck2/hello:world (ovr_config//platform/linux:x86_64-fbcode-46b26edb4b80a905)"})," this gives back ",(0,t.jsx)(l.code,{children:"world"})]}),"\n",(0,t.jsx)(l.hr,{}),"\n",(0,t.jsx)(l.h2,{id:"labelpackage",children:"Label.package"}),"\n",(0,t.jsx)("pre",{class:"language-python",children:(0,t.jsxs)("code",{children:["Label.package: ",(0,t.jsx)(c.default,{to:"/docs/api/starlark/str",children:"str"})]})}),"\n",(0,t.jsxs)(l.p,{children:["For the label ",(0,t.jsx)(l.code,{children:"fbcode//buck2/hello:world (ovr_config//platform/linux:x86_64-fbcode-46b26edb4b80a905)"})," this gives back ",(0,t.jsx)(l.code,{children:"buck2/hello"})]}),"\n",(0,t.jsx)(l.hr,{}),"\n",(0,t.jsx)(l.h2,{id:"labelpath",children:"Label.path"}),"\n",(0,t.jsx)("pre",{class:"language-python",children:(0,t.jsxs)("code",{children:["Label.path: ",(0,t.jsx)(c.default,{to:"/docs/api/build/CellPath",children:"CellPath"})]})}),"\n",(0,t.jsxs)(l.p,{children:["For the label ",(0,t.jsx)(l.code,{children:"fbcode//buck2/hello:world (ovr_config//platform/linux:x86_64-fbcode-46b26edb4b80a905)"})," this gives back ",(0,t.jsx)(l.code,{children:"fbcode/buck2/hello"})]}),"\n",(0,t.jsx)(l.hr,{}),"\n",(0,t.jsx)(l.h2,{id:"labelproject_root",children:"Label.project_root"}),"\n",(0,t.jsx)("pre",{class:"language-python",children:(0,t.jsxs)("code",{children:["Label.project_root: ",(0,t.jsx)(c.default,{to:"/docs/api/build/ProjectRoot",children:"ProjectRoot"})]})}),"\n",(0,t.jsxs)(l.p,{children:["Obtain a reference to the project's root. This can be used as if it were an artifact in places that expect one, such as ",(0,t.jsx)(l.code,{children:"cmd_args().relative_to"}),"."]}),"\n",(0,t.jsx)(l.hr,{}),"\n",(0,t.jsx)(l.h2,{id:"labelraw_target",children:"Label.raw_target"}),"\n",(0,t.jsx)("pre",{class:"language-python",children:(0,t.jsxs)("code",{children:["def Label.raw_target(\n) -> ",(0,t.jsx)(c.default,{to:"/docs/api/build/TargetLabel",children:"TargetLabel"})]})}),"\n",(0,t.jsxs)(l.p,{children:["For the label ",(0,t.jsx)(l.code,{children:"fbcode//buck2/hello:world (ovr_config//platform/linux:x86_64-fbcode-46b26edb4b80a905)"})," this returns the unconfigured underlying target label (",(0,t.jsx)(l.code,{children:"fbcode//buck2/hello:world"}),")"]}),"\n",(0,t.jsx)(l.hr,{}),"\n",(0,t.jsx)(l.h2,{id:"labelsub_target",children:"Label.sub_target"}),"\n",(0,t.jsx)("pre",{class:"language-python",children:(0,t.jsxs)("code",{children:["Label.sub_target: None | list[",(0,t.jsx)(c.default,{to:"/docs/api/starlark/str",children:"str"}),"]"]})})]})}function b(e={}){const{wrapper:l}={...(0,n.R)(),...e.components};return l?(0,t.jsx)(l,{...e,children:(0,t.jsx)(h,{...e})}):h(e)}},28453:(e,l,a)=>{a.d(l,{R:()=>c,x:()=>s});var r=a(96540);const t={},n=r.createContext(t);function c(e){const l=r.useContext(n);return r.useMemo((function(){return"function"==typeof e?e(l):{...l,...e}}),[l,e])}function s(e){let l;return l=e.disableParentContext?"function"==typeof e.components?e.components(t):e.components||t:c(e.components),r.createElement(n.Provider,{value:l},e.children)}}}]);