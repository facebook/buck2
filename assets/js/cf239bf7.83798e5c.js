"use strict";(self.webpackChunkwebsite=self.webpackChunkwebsite||[]).push([[4529],{28453:(e,n,r)=>{r.d(n,{R:()=>c,x:()=>s});var o=r(96540);const t={},i=o.createContext(t);function c(e){const n=o.useContext(i);return o.useMemo((function(){return"function"==typeof e?e(n):{...n,...e}}),[n,e])}function s(e){let n;return n=e.disableParentContext?"function"==typeof e.components?e.components(t):e.components||t:c(e.components),o.createElement(i.Provider,{value:n},e.children)}},75604:(e,n,r)=>{r.r(n),r.d(n,{assets:()=>d,contentTitle:()=>s,default:()=>h,frontMatter:()=>c,metadata:()=>o,toc:()=>l});const o=JSON.parse('{"id":"rfcs/implemented/provider-collection-at","title":"Return error in ProviderCollection[] on undeclared provider","description":"Currently, ctx.attrs.foo[UnknownInfo] returns None if foo is a provider","source":"@site/../docs/rfcs/implemented/provider-collection-at.md","sourceDirName":"rfcs/implemented","slug":"/rfcs/implemented/provider-collection-at","permalink":"/docs/rfcs/implemented/provider-collection-at","draft":false,"unlisted":false,"tags":[],"version":"current","frontMatter":{}}');var t=r(74848),i=r(28453);const c={},s="Return error in ProviderCollection[] on undeclared provider",d={},l=[{value:"Why",id:"why",level:2},{value:"Bazel",id:"bazel",level:2}];function a(e){const n={code:"code",h1:"h1",h2:"h2",header:"header",li:"li",p:"p",pre:"pre",ul:"ul",...(0,i.R)(),...e.components};return(0,t.jsxs)(t.Fragment,{children:[(0,t.jsx)(n.header,{children:(0,t.jsxs)(n.h1,{id:"return-error-in-providercollection-on-undeclared-provider",children:["Return error in ",(0,t.jsx)(n.code,{children:"ProviderCollection[]"})," on undeclared provider"]})}),"\n",(0,t.jsxs)(n.p,{children:["Currently, ",(0,t.jsx)(n.code,{children:"ctx.attrs.foo[UnknownInfo]"})," returns ",(0,t.jsx)(n.code,{children:"None"})," if ",(0,t.jsx)(n.code,{children:"foo"})," is a provider\ncollection."]}),"\n",(0,t.jsx)(n.p,{children:"This RFC proposes these changes:"}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsxs)(n.li,{children:[(0,t.jsx)(n.code,{children:"ctx.attrs.foo[UnknownInfo]"})," is an error"]}),"\n",(0,t.jsxs)(n.li,{children:[(0,t.jsx)(n.code,{children:"UnknownInfo in ctx.attrs.foo"})," is ",(0,t.jsx)(n.code,{children:"False"})]}),"\n",(0,t.jsxs)(n.li,{children:[(0,t.jsx)(n.code,{children:"ctx.attrs.foo.get(UnknownInfo)"})," returns ",(0,t.jsx)(n.code,{children:"None"})]}),"\n"]}),"\n",(0,t.jsx)(n.h2,{id:"why",children:"Why"}),"\n",(0,t.jsx)(n.p,{children:"Better diagnostics when accessing unknown provider. E. g. when writing:"}),"\n",(0,t.jsx)(n.pre,{children:(0,t.jsx)(n.code,{className:"language-python",children:"ctx.attrs.foo[UnknownInfo].bar\n"})}),"\n",(0,t.jsx)(n.p,{children:"Currently, the error is:"}),"\n",(0,t.jsx)(n.pre,{children:(0,t.jsx)(n.code,{children:"Object of type `NoneType` has no attribute `bar`\n"})}),"\n",(0,t.jsx)(n.p,{children:"Instead, the error will be something like:"}),"\n",(0,t.jsx)(n.pre,{children:(0,t.jsx)(n.code,{children:"provider collection does not contain `UnknownInfo`,\n    defined providers are `FooInfo`, `BarInfo`.\n"})}),"\n",(0,t.jsx)(n.h2,{id:"bazel",children:"Bazel"}),"\n",(0,t.jsxs)(n.p,{children:["In bazel, ",(0,t.jsx)(n.code,{children:"[]"})," on unknown provider is an error, like this:"]}),"\n",(0,t.jsx)(n.pre,{children:(0,t.jsx)(n.code,{children:"Error: <target //optional_provider:n2> (rule '_sum')\n    doesn't contain declared provider 'UnknownInfo'\n"})})]})}function h(e={}){const{wrapper:n}={...(0,i.R)(),...e.components};return n?(0,t.jsx)(n,{...e,children:(0,t.jsx)(a,{...e})}):a(e)}}}]);