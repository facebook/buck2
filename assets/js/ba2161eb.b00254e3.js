"use strict";(self.webpackChunkwebsite=self.webpackChunkwebsite||[]).push([[8049],{28453:(e,t,n)=>{n.d(t,{R:()=>l,x:()=>o});var r=n(96540);const a={},d=r.createContext(a);function l(e){const t=r.useContext(d);return r.useMemo((function(){return"function"==typeof e?e(t):{...t,...e}}),[t,e])}function o(e){let t;return t=e.disableParentContext?"function"==typeof e.components?e.components(a):e.components||a:l(e.components),r.createElement(d.Provider,{value:t},e.children)}},41578:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>s,contentTitle:()=>i,default:()=>g,frontMatter:()=>o,metadata:()=>r,toc:()=>c});const r=JSON.parse('{"id":"api/bxl/UnconfiguredTargetNode","title":"UnconfiguredTargetNode","description":"Methods for unconfigured target node.","source":"@site/../docs/api/bxl/UnconfiguredTargetNode.md","sourceDirName":"api/bxl","slug":"/api/bxl/UnconfiguredTargetNode","permalink":"/docs/api/bxl/UnconfiguredTargetNode","draft":false,"unlisted":false,"tags":[],"version":"current","frontMatter":{},"sidebar":"apiSidebar","previous":{"title":"TargetUniverse","permalink":"/docs/api/bxl/TargetUniverse"},"next":{"title":"UnconfiguredTargetSet","permalink":"/docs/api/bxl/UnconfiguredTargetSet"}}');var a=n(74848),d=n(28453),l=n(56289);const o={},i="UnconfiguredTargetNode",s={},c=[{value:"UnconfiguredTargetNode.attrs",id:"unconfiguredtargetnodeattrs",level:2},{value:"UnconfiguredTargetNode.buildfile_path",id:"unconfiguredtargetnodebuildfile_path",level:2},{value:"UnconfiguredTargetNode.deps",id:"unconfiguredtargetnodedeps",level:2},{value:"UnconfiguredTargetNode.get_attr",id:"unconfiguredtargetnodeget_attr",level:2},{value:"UnconfiguredTargetNode.get_attrs",id:"unconfiguredtargetnodeget_attrs",level:2},{value:"UnconfiguredTargetNode.has_attr",id:"unconfiguredtargetnodehas_attr",level:2},{value:"UnconfiguredTargetNode.inputs",id:"unconfiguredtargetnodeinputs",level:2},{value:"UnconfiguredTargetNode.label",id:"unconfiguredtargetnodelabel",level:2},{value:"UnconfiguredTargetNode.oncall",id:"unconfiguredtargetnodeoncall",level:2},{value:"UnconfiguredTargetNode.rule_kind",id:"unconfiguredtargetnoderule_kind",level:2},{value:"UnconfiguredTargetNode.rule_type",id:"unconfiguredtargetnoderule_type",level:2}];function u(e){const t={code:"code",h1:"h1",h2:"h2",header:"header",hr:"hr",p:"p",pre:"pre",...(0,d.R)(),...e.components};return(0,a.jsxs)(a.Fragment,{children:[(0,a.jsx)(t.header,{children:(0,a.jsx)(t.h1,{id:"unconfiguredtargetnode",children:"UnconfiguredTargetNode"})}),"\n",(0,a.jsx)(t.p,{children:"Methods for unconfigured target node."}),"\n",(0,a.jsx)(t.h2,{id:"unconfiguredtargetnodeattrs",children:"UnconfiguredTargetNode.attrs"}),"\n",(0,a.jsx)("pre",{class:"language-python",children:(0,a.jsx)("code",{children:"UnconfiguredTargetNode.attrs: typing.Any"})}),"\n",(0,a.jsxs)(t.p,{children:["Gets the coerced attributes from the unconfigured target node. Returns a struct. Right now, it is not recommended to use this method. Instead, use ",(0,a.jsx)(t.code,{children:"get_attr"})," and ",(0,a.jsx)(t.code,{children:"get_attrs"})," methods. We will deprecate this method in the future."]}),"\n",(0,a.jsx)(t.p,{children:"Sample usage:"}),"\n",(0,a.jsx)(t.pre,{children:(0,a.jsx)(t.code,{className:"language-python",children:'def _impl_attributes(ctx):\n    target_node = ctx.uquery().eval("//foo:bar")[0]\n    ctx.output.print(target_node.attrs.my_attr)\n'})}),"\n",(0,a.jsx)(t.hr,{}),"\n",(0,a.jsx)(t.h2,{id:"unconfiguredtargetnodebuildfile_path",children:"UnconfiguredTargetNode.buildfile_path"}),"\n",(0,a.jsx)("pre",{class:"language-python",children:(0,a.jsxs)("code",{children:["UnconfiguredTargetNode.buildfile_path: ",(0,a.jsx)(l.default,{to:"/docs/api/bxl/FileNode",children:"bxl.FileNode"})]})}),"\n",(0,a.jsx)(t.p,{children:"Gets the buildfile path from the unconfigured target node."}),"\n",(0,a.jsx)(t.p,{children:"Sample usage:"}),"\n",(0,a.jsx)(t.pre,{children:(0,a.jsx)(t.code,{className:"language-python",children:'def _impl_label(ctx):\n    target_node = ctx.uquery().eval("//foo:bar")[0]\n    ctx.output.print(target_node.buildfile_path)\n'})}),"\n",(0,a.jsx)(t.hr,{}),"\n",(0,a.jsx)(t.h2,{id:"unconfiguredtargetnodedeps",children:"UnconfiguredTargetNode.deps"}),"\n",(0,a.jsx)("pre",{class:"language-python",children:(0,a.jsxs)("code",{children:["def UnconfiguredTargetNode.deps(\n) -> list[",(0,a.jsx)(l.default,{to:"/docs/api/build/TargetLabel",children:"TargetLabel"}),"]"]})}),"\n",(0,a.jsxs)(t.p,{children:["Gets all deps for this target. The result is a list of ",(0,a.jsx)(t.code,{children:"UnconfiguredTargetLabel"}),"."]}),"\n",(0,a.jsx)(t.p,{children:"Sample usage:"}),"\n",(0,a.jsx)(t.pre,{children:(0,a.jsx)(t.code,{className:"language-python",children:'def _impl_get_deps(ctx):\n    target_node = ctx.uquery().eval("//foo:bar")[0]\n    ctx.output.print(target_node.deps())\n'})}),"\n",(0,a.jsx)(t.hr,{}),"\n",(0,a.jsx)(t.h2,{id:"unconfiguredtargetnodeget_attr",children:"UnconfiguredTargetNode.get_attr"}),"\n",(0,a.jsx)("pre",{class:"language-python",children:(0,a.jsxs)("code",{children:["def UnconfiguredTargetNode.get_attr(\nkey: ",(0,a.jsx)(l.default,{to:"/docs/api/starlark/str",children:"str"}),",\n/,\n)"]})}),"\n",(0,a.jsxs)(t.p,{children:["Gets the attribute from the unconfigured target node. If the attribute is unset, returns the default value. If the attribute is not defined by the rule, returns ",(0,a.jsx)(t.code,{children:"None"}),". It will not return special attribute (attribute that start with 'buck.' in ",(0,a.jsx)(t.code,{children:"buck2 uquery -A"})," command)."]}),"\n",(0,a.jsx)(t.p,{children:"Sample usage:"}),"\n",(0,a.jsx)(t.pre,{children:(0,a.jsx)(t.code,{className:"language-python",children:"def _impl_attributes(ctx):\n    target_node = ctx.uquery().eval(\"//foo:bar\")[0]\n    ctx.output.print(target_node.get_attr('my_attr'))\n"})}),"\n",(0,a.jsx)(t.hr,{}),"\n",(0,a.jsx)(t.h2,{id:"unconfiguredtargetnodeget_attrs",children:"UnconfiguredTargetNode.get_attrs"}),"\n",(0,a.jsx)("pre",{class:"language-python",children:(0,a.jsxs)("code",{children:["def UnconfiguredTargetNode.get_attrs(\n) -> dict[",(0,a.jsx)(l.default,{to:"/docs/api/starlark/str",children:"str"}),", typing.Any]"]})}),"\n",(0,a.jsx)(t.p,{children:"Gets the all attributes (not include speical attributes) from the unconfigured target node. For attributes that are not explicitly set, the default value is returned."}),"\n",(0,a.jsx)(t.p,{children:"Sample usage:"}),"\n",(0,a.jsx)(t.pre,{children:(0,a.jsx)(t.code,{className:"language-python",children:'def _impl_attributes(ctx):\n    target_node = ctx.uquery().eval("//foo:bar")[0]\n    ctx.output.print(target_node.get_attrs())\n'})}),"\n",(0,a.jsx)(t.hr,{}),"\n",(0,a.jsx)(t.h2,{id:"unconfiguredtargetnodehas_attr",children:"UnconfiguredTargetNode.has_attr"}),"\n",(0,a.jsx)("pre",{class:"language-python",children:(0,a.jsxs)("code",{children:["def UnconfiguredTargetNode.has_attr(\nkey: ",(0,a.jsx)(l.default,{to:"/docs/api/starlark/str",children:"str"}),",\n/,\n) -> ",(0,a.jsx)(l.default,{to:"/docs/api/starlark/bool",children:"bool"})]})}),"\n",(0,a.jsx)(t.p,{children:"Check if rule has the attribute."}),"\n",(0,a.jsxs)(t.p,{children:["Known attribute is always set explicitly or to default value\n(otherwise target would not be created)\nFor special attributes, it will return ",(0,a.jsx)(t.code,{children:"False"})]}),"\n",(0,a.jsx)(t.p,{children:"Sample usage:"}),"\n",(0,a.jsx)(t.pre,{children:(0,a.jsx)(t.code,{className:"language-python",children:"def _impl_attributes(ctx):\n    target_node = ctx.uquery().eval(\"//foo:bar\")[0]\n    ctx.output.print(target_node.has_attr('my_attr'))\n"})}),"\n",(0,a.jsx)(t.hr,{}),"\n",(0,a.jsx)(t.h2,{id:"unconfiguredtargetnodeinputs",children:"UnconfiguredTargetNode.inputs"}),"\n",(0,a.jsx)("pre",{class:"language-python",children:(0,a.jsxs)("code",{children:["def UnconfiguredTargetNode.inputs(\n) -> list[",(0,a.jsx)(l.default,{to:"/docs/api/build/CellPath",children:"CellPath"}),"]"]})}),"\n",(0,a.jsxs)(t.p,{children:["Gets all files which are an immediate input to the rule function and thus are needed to go through analysis. The result is a list of ",(0,a.jsx)(t.code,{children:"CellPath"}),"."]}),"\n",(0,a.jsx)(t.p,{children:"Sample usage:"}),"\n",(0,a.jsx)(t.pre,{children:(0,a.jsx)(t.code,{className:"language-python",children:'def _impl_get_deps(ctx):\n    target_node = ctx.uquery().eval("//foo:bar")[0]\n    ctx.output.print(target_node.inputs())\n'})}),"\n",(0,a.jsx)(t.hr,{}),"\n",(0,a.jsx)(t.h2,{id:"unconfiguredtargetnodelabel",children:"UnconfiguredTargetNode.label"}),"\n",(0,a.jsx)("pre",{class:"language-python",children:(0,a.jsxs)("code",{children:["UnconfiguredTargetNode.label: ",(0,a.jsx)(l.default,{to:"/docs/api/build/TargetLabel",children:"TargetLabel"})]})}),"\n",(0,a.jsx)(t.p,{children:"Gets the label from the unconfigured target node."}),"\n",(0,a.jsx)(t.p,{children:"Sample usage:"}),"\n",(0,a.jsx)(t.pre,{children:(0,a.jsx)(t.code,{className:"language-python",children:'def _impl_label(ctx):\n    target_node = ctx.uquery().eval("//foo:bar")[0]\n    ctx.output.print(target_node.label)\n'})}),"\n",(0,a.jsx)(t.hr,{}),"\n",(0,a.jsx)(t.h2,{id:"unconfiguredtargetnodeoncall",children:"UnconfiguredTargetNode.oncall"}),"\n",(0,a.jsx)("pre",{class:"language-python",children:(0,a.jsxs)("code",{children:["UnconfiguredTargetNode.oncall: None | ",(0,a.jsx)(l.default,{to:"/docs/api/starlark/str",children:"str"})]})}),"\n",(0,a.jsxs)(t.p,{children:["Gets the target's special attr ",(0,a.jsx)(t.code,{children:"oncall"})]}),"\n",(0,a.jsx)(t.p,{children:"Sample usage:"}),"\n",(0,a.jsx)(t.pre,{children:(0,a.jsx)(t.code,{className:"language-python",children:'def _impl_get_oncall(ctx):\n    target_node = ctx.uquery().eval("//foo:bar")[0]\n    ctx.output.print(target_node.oncall)\n'})}),"\n",(0,a.jsx)(t.hr,{}),"\n",(0,a.jsx)(t.h2,{id:"unconfiguredtargetnoderule_kind",children:"UnconfiguredTargetNode.rule_kind"}),"\n",(0,a.jsx)("pre",{class:"language-python",children:(0,a.jsxs)("code",{children:["UnconfiguredTargetNode.rule_kind: ",(0,a.jsx)(l.default,{to:"/docs/api/starlark/str",children:"str"})]})}),"\n",(0,a.jsx)(t.p,{children:"Gets the targets' corresponding rule's kind which is one of - normal (with no special properties) - configured (usable in a configuration context) - toolchain (only usable as a toolchain dep)"}),"\n",(0,a.jsx)(t.p,{children:"Sample usage:"}),"\n",(0,a.jsx)(t.pre,{children:(0,a.jsx)(t.code,{className:"language-python",children:'def _impl_rule_kind(ctx):\n    target_node = ctx.uquery().eval("//foo:bar")[0]\n    ctx.output.print(target_node.rule_kind)\n'})}),"\n",(0,a.jsx)(t.hr,{}),"\n",(0,a.jsx)(t.h2,{id:"unconfiguredtargetnoderule_type",children:"UnconfiguredTargetNode.rule_type"}),"\n",(0,a.jsx)("pre",{class:"language-python",children:(0,a.jsxs)("code",{children:["UnconfiguredTargetNode.rule_type: ",(0,a.jsx)(l.default,{to:"/docs/api/starlark/str",children:"str"})]})}),"\n",(0,a.jsx)(t.p,{children:"Gets the fully qualified name of the rule for this unconfigured target node as a string. This includes the import path as well."}),"\n",(0,a.jsx)(t.p,{children:"Sample usage:"}),"\n",(0,a.jsx)(t.pre,{children:(0,a.jsx)(t.code,{className:"language-python",children:'def _impl_rule_type(ctx):\n    target_node = ctx.uquery().eval("//foo:bar")[0]\n    ctx.output.print(target_node.rule_type)\n'})})]})}function g(e={}){const{wrapper:t}={...(0,d.R)(),...e.components};return t?(0,a.jsx)(t,{...e,children:(0,a.jsx)(u,{...e})}):u(e)}}}]);