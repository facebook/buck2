"use strict";(self.webpackChunkwebsite=self.webpackChunkwebsite||[]).push([[8224],{28453:(t,e,o)=>{o.d(e,{R:()=>r,x:()=>u});var n=o(96540);const c={},s=n.createContext(c);function r(t){const e=n.useContext(s);return n.useMemo((function(){return"function"==typeof t?t(e):{...e,...t}}),[e,t])}function u(t){let e;return e=t.disableParentContext?"function"==typeof t.components?t.components(c):t.components||c:r(t.components),n.createElement(s.Provider,{value:e},t.children)}},51756:(t,e,o)=>{o.r(e),o.d(e,{assets:()=>i,contentTitle:()=>u,default:()=>l,frontMatter:()=>r,metadata:()=>n,toc:()=>a});const n=JSON.parse('{"id":"concepts/buck_out","title":"buck-out","description":"Buck2 stores build artifacts in a directory named buck-out in the root of your","source":"@site/../docs/concepts/buck_out.md","sourceDirName":"concepts","slug":"/concepts/buck_out","permalink":"/docs/concepts/buck_out","draft":false,"unlisted":false,"tags":[],"version":"current","frontMatter":{"id":"buck_out","title":"buck-out"},"sidebar":"main","previous":{"title":"Target Pattern","permalink":"/docs/concepts/target_pattern"},"next":{"title":"Visibility","permalink":"/docs/concepts/visibility"}}');var c=o(74848),s=o(28453);const r={id:"buck_out",title:"buck-out"},u="buck-out",i={},a=[];function d(t){const e={a:"a",code:"code",h1:"h1",header:"header",p:"p",pre:"pre",...(0,s.R)(),...t.components};return(0,c.jsxs)(c.Fragment,{children:[(0,c.jsx)(e.header,{children:(0,c.jsx)(e.h1,{id:"buck-out",children:"buck-out"})}),"\n",(0,c.jsxs)(e.p,{children:["Buck2 stores build artifacts in a directory named ",(0,c.jsx)(e.code,{children:"buck-out"})," in the root of your\n",(0,c.jsx)(e.a,{href:"/docs/concepts/glossary#project",children:"project"}),". You should not make assumptions about where\nBuck2 places your build artifacts within the directory structure beneath\n",(0,c.jsx)(e.code,{children:"buck-out"})," as these locations depend on Buck2's implementation and could\npotentially change over time. Instead, to obtain the location of the build\nartifact for a particular target, you can use one of the ",(0,c.jsx)(e.code,{children:"--show-*-output"}),"\noptions with the ",(0,c.jsx)(e.a,{href:"../../users/commands/build",children:(0,c.jsx)(e.code,{children:"buck2 build"})})," or\n",(0,c.jsx)(e.a,{href:"../../users/commands/targets",children:(0,c.jsx)(e.code,{children:"buck2 targets"})})," commands, most commonly\n",(0,c.jsx)(e.code,{children:"--show-output"}),". For the full list of ways to show the output location, you can\nrun ",(0,c.jsx)(e.code,{children:"buck2 build --help"})," or ",(0,c.jsx)(e.code,{children:"buck2 targets --help"}),"."]}),"\n",(0,c.jsx)(e.pre,{children:(0,c.jsx)(e.code,{className:"language-sh",children:"buck2 targets --show-output <target>\nbuck2 build --show-output <target>\n"})})]})}function l(t={}){const{wrapper:e}={...(0,s.R)(),...t.components};return e?(0,c.jsx)(e,{...t,children:(0,c.jsx)(d,{...t})}):d(t)}}}]);