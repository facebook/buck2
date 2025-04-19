"use strict";(self.webpackChunkwebsite=self.webpackChunkwebsite||[]).push([[3401],{28453:(e,n,t)=>{t.d(n,{R:()=>r,x:()=>l});var s=t(96540);const a={},o=s.createContext(a);function r(e){const n=s.useContext(o);return s.useMemo((function(){return"function"==typeof e?e(n):{...n,...e}}),[n,e])}function l(e){let n;return n=e.disableParentContext?"function"==typeof e.components?e.components(a):e.components||a:r(e.components),s.createElement(o.Provider,{value:n},e.children)}},73264:(e,n,t)=>{t.r(n),t.d(n,{assets:()=>i,contentTitle:()=>l,default:()=>h,frontMatter:()=>r,metadata:()=>s,toc:()=>c});const s=JSON.parse('{"id":"rfcs/implemented/bxl-analysis","title":"Bxl support for performing analysis on targets","description":"Intro","source":"@site/../docs/rfcs/implemented/bxl-analysis.md","sourceDirName":"rfcs/implemented","slug":"/rfcs/implemented/bxl-analysis","permalink":"/docs/rfcs/implemented/bxl-analysis","draft":false,"unlisted":false,"tags":[],"version":"current","frontMatter":{}}');var a=t(74848),o=t(28453);const r={},l="Bxl support for performing analysis on targets",i={},c=[{value:"Intro",id:"intro",level:2},{value:"How to implement it?",id:"how-to-implement-it",level:2}];function d(e){const n={code:"code",em:"em",h1:"h1",h2:"h2",header:"header",li:"li",ol:"ol",p:"p",pre:"pre",...(0,o.R)(),...e.components};return(0,a.jsxs)(a.Fragment,{children:[(0,a.jsx)(n.header,{children:(0,a.jsx)(n.h1,{id:"bxl-support-for-performing-analysis-on-targets",children:"Bxl support for performing analysis on targets"})}),"\n",(0,a.jsx)(n.h2,{id:"intro",children:"Intro"}),"\n",(0,a.jsxs)(n.p,{children:["As Bob and I continue to build out ",(0,a.jsx)(n.code,{children:"bxl"})," we want users to be able to inspect the\nproviders and actions for a given target label. In order to support this, we\nneed to be able to provide access to ",(0,a.jsx)(n.code,{children:"AnalysisResult"})," via ",(0,a.jsx)(n.code,{children:"starlark"}),", obtained\nvia a call to ",(0,a.jsx)(n.code,{children:"RuleAnalysisCalculation::get_analysis_result"}),"."]}),"\n",(0,a.jsx)(n.h2,{id:"how-to-implement-it",children:"How to implement it?"}),"\n",(0,a.jsx)(n.p,{children:"Our three principle options are as follows:"}),"\n",(0,a.jsxs)(n.ol,{children:["\n",(0,a.jsxs)(n.li,{children:[(0,a.jsx)(n.code,{children:"BxlContext::analyze(targetlabel: ConfiguredTargetLabelLike)"}),", where\n",(0,a.jsx)(n.code,{children:"ConfiguredTargetLabelLike"})," accepts ",(0,a.jsx)(n.code,{children:"ConfiguredTargetLabel"}),",\n",(0,a.jsx)(n.code,{children:"ConfiguredTargetNode"}),", or sets and lists of these things + acceptable\nstrings."]}),"\n"]}),"\n",(0,a.jsxs)(n.p,{children:["In this scenario, we attach the analysis method onto the bxl context itself, and\nrequire that users pass in the target label-ish thing when they want to\nconstruct an analysis result. It's a little awkward in some ways because the\nanalysis is more naturally a method on the argument being passed in and the\n",(0,a.jsx)(n.code,{children:"BxlContext"})," is a context that is needed to perform the calculation. On the\nother hand, this allows us to construct a type analogous to ",(0,a.jsx)(n.code,{children:"TargetExpr"})," which\ncan parse from a wide variety of different ",(0,a.jsx)(n.code,{children:"ConfiguredTarget"})," like things\n(strings, nodes, labels, sets, ...). It also is a bit nice from an\nimplementational standpoint since we don't have to pass the context around\neverywhere. This isn't a huge pro though, since we can stick it in the global\neval field."]}),"\n",(0,a.jsx)(n.pre,{children:(0,a.jsx)(n.code,{className:"language-python",children:'result = bxl.analyze(bxl.cquery.deps("foo"))\n'})}),"\n",(0,a.jsxs)(n.ol,{start:"2",children:["\n",(0,a.jsxs)(n.li,{children:[(0,a.jsx)(n.code,{children:"ConfiguredTargetLabel::analyze()"}),", ",(0,a.jsx)(n.code,{children:"ConfiguredTargetNode::analyze()"}),", ...\nwhere we carry around the ",(0,a.jsx)(n.code,{children:"BxlContext"})," in the ",(0,a.jsx)(n.code,{children:"eval"})," global field and\nimplement analysis on each type that is target label like."]}),"\n"]}),"\n",(0,a.jsxs)(n.p,{children:["The pro of this one is that it's quite natural - you can take a\n",(0,a.jsx)(n.code,{children:"ConfiguredStarlarkTargetLabel"})," and then just ... call ",(0,a.jsx)(n.code,{children:"analyze()"})," on it like\nyou might expect to. The two downsides are that we have to propagate the context\naround behind the scenes, and we'll have to provide an implementation of\n",(0,a.jsx)(n.code,{children:"analyze"})," on everything that we'd like to have be able to be ",(0,a.jsx)(n.code,{children:"analyzable"}),"."]}),"\n",(0,a.jsx)(n.pre,{children:(0,a.jsx)(n.code,{className:"language-python",children:'result = "root//bin:the_binary".analyze()\n# but we don\'t support\n"root//bin:the_binary".rdeps()\n\n\n# instead this looks nice\nnodes = ctx.cquery.deps("foo")\nfor n in nodes:\n  # since we can now do\n  nodes.label\n  nodes.attrs.field\n\n  # similarly access analysis\n  nodes.analysis\n'})}),"\n",(0,a.jsxs)(n.ol,{start:"3",children:["\n",(0,a.jsxs)(n.li,{children:[(0,a.jsx)(n.code,{children:"BxlContext::analysis(): AnalysisContext"})," where ",(0,a.jsx)(n.code,{children:"AnalysisContext"})," exposes\n",(0,a.jsx)(n.code,{children:"AnalysisContext::analyze(targetlabel: ConfiguredTargetLabelLike)"}),"."]}),"\n"]}),"\n",(0,a.jsxs)(n.p,{children:["There's not really any pros of this approach except that it's similar to the\nflow for ",(0,a.jsx)(n.code,{children:"cquery"})," where we return a ",(0,a.jsx)(n.code,{children:"cqueryctx"})," object to call ",(0,a.jsx)(n.code,{children:"cquery"})," methods\nthrough."]}),"\n",(0,a.jsx)(n.pre,{children:(0,a.jsx)(n.code,{className:"language-python",children:'result = ctx.analysis().analyze("//lib:file1")\n'})}),"\n",(0,a.jsxs)(n.p,{children:["We can also restrict the API to require that users go through ",(0,a.jsx)(n.code,{children:"cquery"})," to obtain\na ",(0,a.jsx)(n.code,{children:"ConfiguredTargetNode"})," prior to calling ",(0,a.jsx)(n.code,{children:"analysis"}),", although we don't ",(0,a.jsx)(n.em,{children:"have\nto"}),". I say that we don't have to because the ",(0,a.jsx)(n.code,{children:"get_analysis_result"})," method\nmentioned above is configured to accept a label anyway."]})]})}function h(e={}){const{wrapper:n}={...(0,o.R)(),...e.components};return n?(0,a.jsx)(n,{...e,children:(0,a.jsx)(d,{...e})}):d(e)}}}]);