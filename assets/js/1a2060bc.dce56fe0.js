"use strict";(self.webpackChunkwebsite=self.webpackChunkwebsite||[]).push([[5296],{28453:(t,e,a)=>{a.d(e,{R:()=>s,x:()=>l});var n=a(96540);const r={},o=n.createContext(r);function s(t){const e=n.useContext(o);return n.useMemo((function(){return"function"==typeof t?t(e):{...e,...t}}),[e,t])}function l(t){let e;return e=t.disableParentContext?"function"==typeof t.components?t.components(r):t.components||r:s(t.components),n.createElement(o.Provider,{value:e},t.children)}},58503:(t,e,a)=>{a.r(e),a.d(e,{assets:()=>c,contentTitle:()=>i,default:()=>p,frontMatter:()=>l,metadata:()=>n,toc:()=>d});const n=JSON.parse('{"id":"api/starlark/float","title":"float","description":"def float(","source":"@site/../docs/api/starlark/float.md","sourceDirName":"api/starlark","slug":"/api/starlark/float","permalink":"/docs/api/starlark/float","draft":false,"unlisted":false,"tags":[],"version":"current","frontMatter":{},"sidebar":"apiSidebar","previous":{"title":"dict","permalink":"/docs/api/starlark/dict"},"next":{"title":"int","permalink":"/docs/api/starlark/int"}}');var r=a(74848),o=a(28453),s=a(56289);const l={},i="float",c={},d=[];function f(t){const e={a:"a",code:"code",h1:"h1",header:"header",p:"p",pre:"pre",...(0,o.R)(),...t.components};return(0,r.jsxs)(r.Fragment,{children:[(0,r.jsx)(e.header,{children:(0,r.jsx)(e.h1,{id:"float",children:"float"})}),"\n",(0,r.jsx)("pre",{class:"language-python",children:(0,r.jsxs)("code",{children:["def float(\na: ",(0,r.jsx)(s.default,{to:"/docs/api/starlark/bool",children:"bool"})," | ",(0,r.jsx)(s.default,{to:"/docs/api/starlark/float",children:"float"})," | ",(0,r.jsx)(s.default,{to:"/docs/api/starlark/int",children:"int"})," | ",(0,r.jsx)(s.default,{to:"/docs/api/starlark/str",children:"str"})," = ...,\n/,\n) -> ",(0,r.jsx)(s.default,{to:"/docs/api/starlark/float",children:"float"})]})}),"\n",(0,r.jsxs)(e.p,{children:[(0,r.jsx)(e.a,{href:"https://github.com/bazelbuild/starlark/blob/master/spec.md#float",children:"float"}),": interprets its argument as a floating-point number."]}),"\n",(0,r.jsxs)(e.p,{children:["If x is a ",(0,r.jsx)(e.code,{children:"float"}),", the result is x.\nif x is an ",(0,r.jsx)(e.code,{children:"int"}),", the result is the nearest floating point value to x.\nIf x is a string, the string is interpreted as a floating-point literal.\nWith no arguments, ",(0,r.jsx)(e.code,{children:"float()"})," returns ",(0,r.jsx)(e.code,{children:"0.0"}),"."]}),"\n",(0,r.jsx)(e.pre,{children:(0,r.jsx)(e.code,{children:"float() == 0.0\nfloat(1) == 1.0\nfloat('1') == 1.0\nfloat('1.0') == 1.0\nfloat('.25') == 0.25\nfloat('1e2') == 100.0\nfloat(False) == 0.0\nfloat(True) == 1.0\nfloat(\"hello\")   # error: not a valid number\nfloat([])   # error\n"})})]})}function p(t={}){const{wrapper:e}={...(0,o.R)(),...t.components};return e?(0,r.jsx)(e,{...t,children:(0,r.jsx)(f,{...t})}):f(t)}}}]);