"use strict";(self.webpackChunkwebsite=self.webpackChunkwebsite||[]).push([[2855],{1390:(e,n,t)=>{t.d(n,{A:()=>i});const i=t.p+"assets/images/graph_with_target_configurations-a8efbe06a4d10d947687d4ba4876c1f9.png"},28453:(e,n,t)=>{t.d(n,{R:()=>s,x:()=>r});var i=t(96540);const a={},o=i.createContext(a);function s(e){const n=i.useContext(o);return i.useMemo((function(){return"function"==typeof e?e(n):{...n,...e}}),[n,e])}function r(e){let n;return n=e.disableParentContext?"function"==typeof e.components?e.components(a):e.components||a:s(e.components),i.createElement(o.Provider,{value:n},e.children)}},41735:(e,n,t)=>{t.d(n,{A:()=>i});const i=t.p+"assets/images/graph_with_deps-933225f0c6abc653036be2ffa658fa5b.png"},75566:(e,n,t)=>{t.d(n,{A:()=>i});const i=t.p+"assets/images/execution_platform_resolution-8f3830e745aee32538fc2d45398468f6.png"},90982:(e,n,t)=>{t.r(n),t.d(n,{assets:()=>l,contentTitle:()=>r,default:()=>h,frontMatter:()=>s,metadata:()=>i,toc:()=>c});const i=JSON.parse('{"id":"rule_authors/configurations_by_example","title":"Configurations By Example","description":"Buck\u2019s architectural model description","source":"@site/../docs/rule_authors/configurations_by_example.md","sourceDirName":"rule_authors","slug":"/rule_authors/configurations_by_example","permalink":"/docs/rule_authors/configurations_by_example","draft":false,"unlisted":false,"tags":[],"version":"current","frontMatter":{"id":"configurations_by_example","title":"Configurations By Example"},"sidebar":"main","previous":{"title":"Configurations","permalink":"/docs/rule_authors/configurations"},"next":{"title":"Configuration Transitions","permalink":"/docs/rule_authors/configuration_transitions"}}');var a=t(74848),o=t(28453);const s={id:"configurations_by_example",title:"Configurations By Example"},r=void 0,l={},c=[{value:"Defining Configurations",id:"defining-configurations",level:2},{value:"Target Platform Resolution",id:"target-platform-resolution",level:2},{value:"Target Compatibility",id:"target-compatibility",level:2},{value:"Execution Platforms",id:"execution-platforms",level:2},{value:"Exec Deps",id:"exec-deps",level:2},{value:"Graph with deps",id:"graph-with-deps",level:2},{value:"Splitting //",id:"splitting-",level:2},{value:"Execution Platform resolution",id:"execution-platform-resolution",level:2},{value:"Target configurations",id:"target-configurations",level:2}];function d(e){const n={a:"a",blockquote:"blockquote",code:"code",h1:"h1",h2:"h2",img:"img",li:"li",ol:"ol",p:"p",pre:"pre",ul:"ul",...(0,o.R)(),...e.components};return(0,a.jsxs)(a.Fragment,{children:[(0,a.jsxs)(n.p,{children:[(0,a.jsx)(n.a,{href:"../../developers/architecture/buck2/",children:"Buck\u2019s architectural model"})," description\nis a very helpful pre-read."]}),"\n",(0,a.jsx)(n.p,{children:"The main use of configurations is changing target properties based on what the\nbuild is targeting, which may include platform properties like OS, architecture,\nruntime version (think java, python) etc and other build properties like\noptimization level"}),"\n",(0,a.jsx)(n.p,{children:"An example of how that\u2019s done:"}),"\n",(0,a.jsx)(n.pre,{children:(0,a.jsx)(n.code,{className:"language-python",children:'# //libs/BUCK\n\njava_library(\nname = "foo",\ndeps = [\n    "//libs:lib1",\n    "//libs:lib2",\n] + select({\n    "//constraints:x86": ["//libs:lib3-x86"],\n    "//constraints:mac-arm64": ["//libs:lib3-mac-arm64"],\n    "//constraints:windows-arm64": ["//libs:lib3-win-arm64"],\n    "DEFAULT": ["//libs:lib3-general"],\n})\n...\n)\n...\n'})}),"\n",(0,a.jsxs)(n.ul,{children:["\n",(0,a.jsxs)(n.li,{children:["select() can appear in almost all attributes","\n",(0,a.jsxs)(n.ul,{children:["\n",(0,a.jsx)(n.li,{children:"since example above has lists of a single element, it could\u2019ve been a select\nfor a single element in the list rather than added to the list. that\u2019s\npretty inflexible (can\u2019t have empty cases, each case must be exactly one\nelement) and so it wouldn\u2019t generally be used"}),"\n"]}),"\n"]}),"\n",(0,a.jsx)(n.li,{children:"string, list, dict can all be added to select (on either side): list + select,\nselect + list, str + select, \u2026"}),"\n",(0,a.jsx)(n.li,{children:"Each branch of select() takes a config_setting (described below), which\ndenotes a list of required constraint_values; there\u2019s also an optional\n\u201dDEFAULT\u201d branch to the select. The target platform resolution rules (below)\npick a platform, which itself gives a list of provided constraint_values. A\nbranch matches if all its required constraint_values are provided by the\nplatform. If no branch matches then the DEFAULT branch is used (or failure if\nthere\u2019s no DEFAULT branch); if one branch matches it is used, if more than one\nbranch matches then see the \u201cselect resolution ambiguity (refinement)\u201d section\nbelow."}),"\n",(0,a.jsxs)(n.li,{children:["select() is resolved during configuration. this happens after the evaluation\nof the BUCK file is completed, and so starlark code run during BUCK file\nevaluation does not have access to the resolved value. This can make it\ndifficult to have macros that do extensive modification or inspection of\nattributes (and certainly we encourage doing that in rules instead). There are\nsome functions to do some limited operations on these objects:","\n",(0,a.jsxs)(n.ul,{children:["\n",(0,a.jsxs)(n.li,{children:["select_map(obj, function): applies function to all possible resolved values\nin obj","\n",(0,a.jsxs)(n.ul,{children:["\n",(0,a.jsxs)(n.li,{children:["ex:\n",(0,a.jsx)(n.code,{children:"select_map([1] + select({x: 2, y: 3}), lambda v: v+1) == [2] + select(x: 3, y: 4)"})]}),"\n"]}),"\n"]}),"\n",(0,a.jsx)(n.li,{children:"select_test(obj, function): function should return a bool, then applies\nfunction to each resolved value and returns True if function returns True\nfor any of them"}),"\n"]}),"\n"]}),"\n"]}),"\n",(0,a.jsx)(n.h2,{id:"defining-configurations",children:"Defining Configurations"}),"\n",(0,a.jsx)(n.p,{children:"First, define constraints and config settings. Defining constraints is done with\nconstraint_setting and constraint_value. constraint_setting in some sense is the\nID of a group of constraints each defined with constraint_value. In any\nconfiguration, only one value can be present for a constraint_setting. The\nconfig_setting rule allows creating a logical AND of constraints, and also can\nrequire that buckconfig keys have certain values."}),"\n",(0,a.jsx)(n.pre,{children:(0,a.jsx)(n.code,{className:"language-python",children:'# //constraints/BUCK\n\n# constraint_setting defines a key for a logical group of constraint values. A configuration can only\n# have at most one constraint value set for each constraint_settings\nconstraint_setting(\n    name = "arch",\n)\n\nconstraint_value(\n    name = "x86",\n    constraint_setting = ":arch",\n)\n\nconstraint_value(\n    name = "arm64",\n    constraint_setting = ":arch",\n)\n\nconstraint_setting(\n    name = "os",\n)\n\nconstraint_value(\n    name = "windows",\n    constraint_setting = ":os",\n)\n\nconstraint_value(\n    name = "mac",\n    constraint_setting = ":os",\n)\n\nconstraint_setting(\n    name = "mode",\n)\n\nconstraint_value(\n    name = "dev",\n    constraint_settings = ":mode",\n)\n\nconstraint_value(\n    name = "opt",\n    constraint_settings = ":mode",\n)\n\n# can use config_setting to group constraint values into larger logical pieces\nconfig_setting(\n    name = "mac-arm64",\n    constraint_values = [\n        ":mac",\n        ":arm64",\n    ]\n)\n\nconfig_setting(\n    name = "windows-arm64",\n    constraint_values = [\n        ":windows",\n        ":arm64",\n    ]\n)\n\n# an example of checking a buckconfig value. If the buckconfig is set,\n# this config_setting is satisfied in all configurations\nconfig_setting(\n    name = "check_some_config",\n    values = {\n        "foo.fastmode_enabled": "true",\n    }\n)\n'})}),"\n",(0,a.jsx)(n.p,{children:"Next, define platforms (which, confusingly, create what we call a\nconfiguration). platforms are just a collection of constraints. A platform() can\nhave other platforms as deps and will union the constraints associated with that\nplatform. this example shows a couple techniques that can be helpful for\ndefining platforms"}),"\n",(0,a.jsx)(n.pre,{children:(0,a.jsx)(n.code,{className:"language-python",children:'#//platforms/BUCK\n\n[\nplatform(\n        name = "{}-{}".format(base, mode)\n        deps = [":{}".format(base)],\n        constraint_values = ["//constraints:{}".format(mode)\n    )\n    for base in ["mac-x86", "mac-arm64", "windows-x86", "windows-arm64"]\n    for mode in ["dev", "opt"]\n]\n\n[\n    platform(\n        name = name,\n        constraint_values = constraint_values\n    ) for name, constraint_values in [\n        "mac-x86", ["//constraints:mac", "//constraints:x86"],\n        "mac-arm64", ["//constraints:mac", "//constraints:arm64"],\n        "windows-x86", ["//constraints:windows", "//constraints:x86"],\n        "windows-arm64", ["//constraints:windows", "//constraints:arm64"],\n    ]\n]\n'})}),"\n",(0,a.jsx)(n.h2,{id:"target-platform-resolution",children:"Target Platform Resolution"}),"\n",(0,a.jsx)(n.p,{children:"The one remaining piece to put these all together is about selecting a target\nplatform for the top-level targets."}),"\n",(0,a.jsx)(n.p,{children:"In the case that targets are provided on the command line, configurations are\ndetermined by performing 'target platform resolution' on the unconfigured target\nlabels."}),"\n",(0,a.jsxs)(n.p,{children:["The target platform resolution for a target //",":foo"," works as follows:"]}),"\n",(0,a.jsxs)(n.ol,{children:["\n",(0,a.jsxs)(n.li,{children:["Look up (unconfigured) target node for //",":foo","."]}),"\n",(0,a.jsx)(n.li,{children:"If the command has a --target-platforms flag, use that."}),"\n",(0,a.jsx)(n.li,{children:"If there's a default_target_platform attribute on the node, use that."}),"\n",(0,a.jsx)(n.li,{children:"Else, use the cell's default platform spec (from buckconfig\nparser.target_platform_detector_spec)."}),"\n"]}),"\n",(0,a.jsx)(n.p,{children:"This is performed independently for any top-level targets that need a platform.\nSince this resolution is done without a configuration, it means that the\ndefault_target_platform attribute is not selectable."}),"\n",(0,a.jsx)(n.p,{children:"This target platform will form the initial configuration for the node and will\nbe passed down to all of the target dependencies of that node (exceptions, like\nexec deps, are described below)."}),"\n",(0,a.jsx)(n.p,{children:"Example:"}),"\n",(0,a.jsx)(n.pre,{children:(0,a.jsx)(n.code,{className:"language-python",children:'# //binaries/BUCK\n\njava_binary(\n    name = "cats",\n    default_target_platform = "//platforms:windows-arm64-dev",\n    deps = ["//libs:foo"],\n)\n\njava_binary(\n    name = "dogs",\n    default_target_platform = "//platforms:mac-x86-dev",\n    deps = ["//libs:foo"],\n)\n'})}),"\n",(0,a.jsxs)(n.p,{children:["If you then do ",(0,a.jsx)(n.code,{children:"buck2 build //binaries:cats //binaries:dogs"}),", the\n//binaries",":cats"," binary will be built in the //platforms",":windows-arm64-dev","\nconfiguration and the //binaries",":dogs"," binary will be built in the\n//platforms",":mac-x86-dev"," configuration. Each of those binaries depend on\n//libs",":foo",", but they will get different versions of it as the binaries\u2019\nconfigurations will each be passed down to their dependencies."]}),"\n",(0,a.jsxs)(n.p,{children:["If you look at the //libs",":foo"," defined above, for //binaries",":cats"," its resolved\ndependencies will include //libs",":lib3-win-arm64"," and for //binaries",":dogs"," it would\ncontain //libs",":lib3-x86","."]}),"\n",(0,a.jsx)(n.p,{children:"You can specify a different target platform on the command line. If you run"}),"\n",(0,a.jsxs)(n.p,{children:[(0,a.jsx)(n.code,{children:"buck2 build //binaries:cats //binaries:dogs --target-platforms //platforms:mac-x86-opt"}),",\nboth //binaries",":cats"," and //binaries",":dogs"," will be built in the\n//platforms",":mac-x86-opt"," configuration."]}),"\n",(0,a.jsx)(n.h2,{id:"target-compatibility",children:"Target Compatibility"}),"\n",(0,a.jsx)(n.p,{children:"If a target doesn\u2019t work when built targeting certain platforms or\nconfigurations, it can specify this by setting target_compatible_with. This\nattribute is a list of constraints that a configuration must have otherwise the\ntarget will be marked as incompatible with that configuration."}),"\n",(0,a.jsx)(n.pre,{children:(0,a.jsx)(n.code,{className:"language-python",children:'# //other/BUCK\n\ndefault_target_platform = "//platforms:mac-x86-dev" if host_info().os == "mac" else "//platforms:win-x86-dev"\n\n...\n\njava_binary(\n    name = "other",\n    deps = [":other_lib"],\n    default_target_platform = default_target_platform,\n)\n\njava_library(\n    name = "other_lib",\n    target_compatible_with = [\n        "//constraints:dev",\n        "//constraints:win",\n    ]\n)\n'})}),"\n",(0,a.jsxs)(n.p,{children:["Running ",(0,a.jsx)(n.code,{children:"buck2 build //other:other --target-platforms //platforms:win-x86-dev"}),"\nwould build other in that configuration. But running\n",(0,a.jsx)(n.code,{children:"buck2 build //other:other --target-platforms //platforms:mac-x86-dev"})," would\nfail, because //other",":other_lib"," would be incompatible with that configuration\nand so //other",":other"," would be as well. buck considers it an error to request to\nbuild (or run or install or test) an explicit target that is incompatible."]}),"\n",(0,a.jsxs)(n.p,{children:["If a package (ex //other:) or recursive (ex //other/...) pattern is provided, it\nis not an error for that to include incompatible targets and they will instead\nsimply be skipped (buck should print a message that it is skipping them). In\nthis example, the default_target_platform is being selected based on the host\n(you could imagine this being commonly done within some small macro layer that\nyour project uses). There may be other targets in the //other/BUCK file that are\ncompatible with mac, and so if you do ",(0,a.jsx)(n.code,{children:"buck2 build //other:"})," that could build\nall the targets in that package that are compatible with their\ndefault_target_platform and if they all used the same as //other",":other"," some of\nthem may be compatible with mac when building on a mac and those would be built\nfine (and //other",":other"," would be skipped)."]}),"\n",(0,a.jsx)(n.h1,{id:"advanced-topics",children:"Advanced topics"}),"\n",(0,a.jsx)(n.h2,{id:"execution-platforms",children:"Execution Platforms"}),"\n",(0,a.jsx)(n.p,{children:"Execution platforms are used to define the configurations and execution\nproperties for the platforms used by build tools during the build. Currently\nthere is a single list (in priority order) of all available execution platforms.\nThis list is provided by a target in the build.execution_platforms buckconfig\nconfiguration key."}),"\n",(0,a.jsxs)(n.blockquote,{children:["\n",(0,a.jsx)(n.p,{children:"To Buck, both execution platforms and the list of them are based on\nExecutionPlatformInfo and ExecutionPlatformRegistrationInfo, but we\u2019ll talk in\nterms of the execution_platform and execution_platforms rules."}),"\n"]}),"\n",(0,a.jsx)(n.p,{children:"There are three main concepts to understand about execution platforms:"}),"\n",(0,a.jsxs)(n.ol,{children:["\n",(0,a.jsx)(n.li,{children:"execution platforms"}),"\n",(0,a.jsx)(n.li,{children:"exec deps"}),"\n",(0,a.jsx)(n.li,{children:"execution platform resolution"}),"\n"]}),"\n",(0,a.jsx)(n.p,{children:"Here\u2019s an example definition of execution platforms."}),"\n",(0,a.jsx)(n.pre,{children:(0,a.jsx)(n.code,{className:"language-python",children:'# //platforms/execution/BUCK\n\nexecution_platform(\n    name = "mac-exec",\n    platform = "//platforms:mac-arm64-opt",\n    local_enabled = host_info().os.is_macos,\n    remote_enabled = True,\n    use_limited_hybrid = False,\n    remote_execution_use_case = "buck2-build",\n    remote_execution_properties = {\n        "platform": "mac-re"\n    },\n)\n\nexecution_platform(\n    name = "windows-exec",\n    platform = "//platforms:windows-arm64-opt",\n    local_enabled = host_info().os.is_windows,\n    ...\n)\n\nexecution_platform(\n    name = "linux-exec",\n    ...\n)\n\nexecution_platforms(\n    name = "exec-platforms",\n    # in practice, may want to change this order based on the host os.\n    platforms = [\n        "linux-exec",\n        "windows-exec",\n        "mac-exec",\n    ],\n    fallback = "error",\n)\n'})}),"\n",(0,a.jsx)(n.p,{children:"This sets us up with three execution platforms, one for each of windows, mac,\nand linux. We choose a more optimized configuration for that platform (i.e. opt\ninstead of dev). Generally for build tools we\u2019d recommend using an optimized\nform as most of the time the build will be executing the built tools rather than\nbuilding them."}),"\n",(0,a.jsx)(n.h2,{id:"exec-deps",children:"Exec Deps"}),"\n",(0,a.jsx)(n.p,{children:"Exec deps are the second part of the execution platform system. An exec dep\ndiffers in two ways from a normal dep:"}),"\n",(0,a.jsxs)(n.ol,{children:["\n",(0,a.jsx)(n.li,{children:"It will inherit the execution platform of its dependent instead of the target\nplatform and"}),"\n",(0,a.jsx)(n.li,{children:"A dependent\u2019s execution platform will be selected so that all exec deps are\ntarget compatible with it."}),"\n"]}),"\n",(0,a.jsx)(n.p,{children:"Exec deps should be used for build tools that will be used when executing the\nactions of a target. If information about the dep is going to be propagated out\nof the target it almost always should not be an execution dep (except for\ntoolchains, see below)."}),"\n",(0,a.jsx)(n.p,{children:"Exec deps are added primarily in two ways:"}),"\n",(0,a.jsxs)(n.ol,{children:["\n",(0,a.jsx)(n.li,{children:"By rule attributes defined with attr.exec_dep() and"}),"\n",(0,a.jsx)(n.li,{children:"By $(exe xxx) placeholders in attributes defined with attr.arg()"}),"\n"]}),"\n",(0,a.jsx)(n.pre,{children:(0,a.jsx)(n.code,{className:"language-python",children:"foo_rule = rule(\n  impl = <...>\n"})}),"\n",(0,a.jsx)(n.h1,{id:"visualizing-configuration-concepts",children:"Visualizing Configuration Concepts"}),"\n",(0,a.jsx)(n.h2,{id:"graph-with-deps",children:"Graph with deps"}),"\n",(0,a.jsx)(n.p,{children:(0,a.jsx)(n.img,{alt:"Example graph with dependencies",src:t(41735).A+"",width:"1999",height:"915"})}),"\n",(0,a.jsxs)(n.h2,{id:"splitting-",children:["Splitting //",":lib3"]}),"\n",(0,a.jsxs)(n.p,{children:["As we work out the configurations here, //",":lib3"," will end up being in two\ndifferent configurations, so gonna be easiest to split it now."]}),"\n",(0,a.jsx)(n.h2,{id:"execution-platform-resolution",children:"Execution Platform resolution"}),"\n",(0,a.jsx)(n.p,{children:(0,a.jsx)(n.img,{alt:"Example graph with dependencies",src:t(75566).A+"",width:"1999",height:"1040"})}),"\n",(0,a.jsxs)(n.p,{children:["This shows which nodes are involved in determining the exec configuration for\nthe //",":binary"," target. The exec deps of //",":binary"," and the exec deps for the\n(transitive) toolchain deps of //",":binary"," are the main things involved, that set\nof exec deps must all be target compatible with an execution platform for it to\nbe selected. In addition, the target itself and its toolchain deps must be\nexec_compatible_with. It is very rare to use exec_compatible_with, for the most\npart exec platform restrictions should be marked on the tools that require the\nrestriction."]}),"\n",(0,a.jsx)(n.h2,{id:"target-configurations",children:"Target configurations"}),"\n",(0,a.jsx)(n.p,{children:(0,a.jsx)(n.img,{alt:"Example graph with dependencies",src:t(1390).A+"",width:"1999",height:"1040"})})]})}function h(e={}){const{wrapper:n}={...(0,o.R)(),...e.components};return n?(0,a.jsx)(n,{...e,children:(0,a.jsx)(d,{...e})}):d(e)}}}]);