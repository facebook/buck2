"use strict";(self.webpackChunkwebsite=self.webpackChunkwebsite||[]).push([[2886],{28453:(e,n,i)=>{i.d(n,{R:()=>t,x:()=>r});var c=i(96540);const s={},o=c.createContext(s);function t(e){const n=c.useContext(o);return c.useMemo((function(){return"function"==typeof e?e(n):{...n,...e}}),[n,e])}function r(e){let n;return n=e.disableParentContext?"function"==typeof e.components?e.components(s):e.components||s:t(e.components),c.createElement(o.Provider,{value:n},e.children)}},90700:(e,n,i)=>{i.r(n),i.d(n,{assets:()=>l,contentTitle:()=>r,default:()=>h,frontMatter:()=>t,metadata:()=>c,toc:()=>a});const c=JSON.parse('{"id":"concepts/buckconfig","title":".buckconfig","description":"The root of your project must contain a configuration","source":"@site/../docs/concepts/buckconfig.md","sourceDirName":"concepts","slug":"/concepts/buckconfig","permalink":"/docs/concepts/buckconfig","draft":false,"unlisted":false,"tags":[],"version":"current","frontMatter":{"id":"buckconfig","title":".buckconfig"},"sidebar":"main","previous":{"title":"Isolation Directory","permalink":"/docs/concepts/isolation_dir"},"next":{"title":"Configurations","permalink":"/docs/concepts/configurations"}}');var s=i(74848),o=i(28453);const t={id:"buckconfig",title:".buckconfig"},r=void 0,l={},a=[{value:"Performance impact of Buck2 configuration changes",id:"performance-impact-of-buck2-configuration-changes",level:2},{value:"The .buckconfig file uses the INI file format",id:"the-buckconfig-file-uses-the-ini-file-format",level:2},{value:"Other INI file parsers",id:"other-ini-file-parsers",level:3},{value:"Dot character not supported in section names",id:"dot-character-not-supported-in-section-names",level:3},{value:"Character encoding",id:"character-encoding",level:2},{value:"Key values as lists",id:"key-values-as-lists",level:2},{value:"Transclusion of values from one key to another",id:"transclusion-of-values-from-one-key-to-another",level:2},{value:"Comments",id:"comments",level:2},{value:".buckconfig.local",id:"buckconfiglocal",level:2},{value:"Other initialization files",id:"other-initialization-files",level:2},{value:"Command-line control of configuration",id:"command-line-control-of-configuration",level:2},{value:"Precedence of Buck2 configuration specifications",id:"precedence-of-buck2-configuration-specifications",level:2},{value:"Configuration files can include other files",id:"configuration-files-can-include-other-files",level:2},{value:"Sections",id:"sections",level:2},{value:"[alias]",id:"alias",level:2},{value:"[cells]",id:"cells",level:2}];function d(e){const n={a:"a",code:"code",em:"em",h2:"h2",h3:"h3",li:"li",ol:"ol",p:"p",pre:"pre",strong:"strong",table:"table",tbody:"tbody",td:"td",th:"th",thead:"thead",tr:"tr",...(0,o.R)(),...e.components};return(0,s.jsxs)(s.Fragment,{children:[(0,s.jsxs)(n.p,{children:["The root of your ",(0,s.jsx)(n.a,{href:"/docs/concepts/glossary#project",children:"project"})," must contain a configuration\nfile named ",(0,s.jsx)(n.code,{children:".buckconfig"}),". Before executing, Buck2 reads this file to incorporate\nany customizations it specifies."]}),"\n",(0,s.jsx)(n.h2,{id:"performance-impact-of-buck2-configuration-changes",children:"Performance impact of Buck2 configuration changes"}),"\n",(0,s.jsx)(n.p,{children:"Because configuration settings are sometimes included in the cache keys that\nBuck2 uses in its caching system, changes to Buck's configuration can invalidate\npreviously-built artifacts in Buck's caches. If this occurs, Buck2 rebuilds\nthose artifacts, which can impact your build time."}),"\n",(0,s.jsxs)(n.p,{children:["These configuration changes can happen when modifying configuration files and\ncommand line args. ",(0,s.jsx)(n.a,{href:"#precedence-of-buck2-configuration-specifications",children:"See more"})]}),"\n",(0,s.jsx)(n.h2,{id:"the-buckconfig-file-uses-the-ini-file-format",children:"The .buckconfig file uses the INI file format"}),"\n",(0,s.jsxs)(n.p,{children:["The ",(0,s.jsx)(n.code,{children:".buckconfig"})," file uses the\n",(0,s.jsx)(n.a,{href:"http://en.wikipedia.org/wiki/INI_file",children:"INI file format"}),". That is, it is divided\ninto ",(0,s.jsx)(n.em,{children:"sections"})," where each section contains a collection of key ",(0,s.jsx)(n.em,{children:"names"})," and key\n",(0,s.jsx)(n.em,{children:"values"}),". The ",(0,s.jsx)(n.code,{children:".buckconfig"})," implementation supports some modifications to the\nINI file format; these are discussed below."]}),"\n",(0,s.jsx)(n.h3,{id:"other-ini-file-parsers",children:"Other INI file parsers"}),"\n",(0,s.jsxs)(n.p,{children:["As mentioned previously, we have extended the INI file parser that Buck2 uses to\nparse configuration files. As a result, ",(0,s.jsx)(n.em,{children:"INI file parsers provided by other\nlanguages or libraries are often not able to parse Buck's configuration files\nsuccessfully"}),"."]}),"\n",(0,s.jsx)(n.h3,{id:"dot-character-not-supported-in-section-names",children:"Dot character not supported in section names"}),"\n",(0,s.jsxs)(n.p,{children:["We do not support the use of the ",(0,s.jsx)(n.em,{children:"dot"})," character (",(0,s.jsx)(n.code,{children:"."}),") in section names within\nBuck2 configuration files. For example, the following is ",(0,s.jsx)(n.strong,{children:"not"}),"\nsupported\u2014",(0,s.jsx)(n.em,{children:"although Buck2 does not issue a warning or error"}),"."]}),"\n",(0,s.jsx)(n.pre,{children:(0,s.jsx)(n.code,{className:"language-ini",children:"[foo.bar]\n  baz=1\n"})}),"\n",(0,s.jsxs)(n.p,{children:["Note that sometimes you might need to define your own custom sections, such as\nfor platform flavors for C++ or Python. These scenarios are examples of when you\nshould be careful not to introduce the dot character in section names. This\nconstraint is because Buck2 uses the dot character to delimit section names and\nkey names in other contexts such as the ",(0,s.jsx)(n.code,{children:"--config"})," command-line parameter."]}),"\n",(0,s.jsx)(n.h2,{id:"character-encoding",children:"Character encoding"}),"\n",(0,s.jsxs)(n.p,{children:["To ensure that any character can be encoded in a ",(0,s.jsx)(n.code,{children:".buckconfig"})," key value, you\ncan use escape sequences to encode characters that would otherwise be\nproblematic. The following escape sequences are supported."]}),"\n",(0,s.jsxs)(n.table,{children:[(0,s.jsx)(n.thead,{children:(0,s.jsxs)(n.tr,{children:[(0,s.jsx)(n.th,{children:(0,s.jsx)(n.code,{children:"\\\\"})}),(0,s.jsx)(n.th,{children:"backslash"})]})}),(0,s.jsxs)(n.tbody,{children:[(0,s.jsxs)(n.tr,{children:[(0,s.jsx)(n.td,{children:(0,s.jsx)(n.code,{children:'\\"'})}),(0,s.jsx)(n.td,{children:"double quote"})]}),(0,s.jsxs)(n.tr,{children:[(0,s.jsx)(n.td,{children:(0,s.jsx)(n.code,{children:"\\n"})}),(0,s.jsx)(n.td,{children:"newline"})]}),(0,s.jsxs)(n.tr,{children:[(0,s.jsx)(n.td,{children:(0,s.jsx)(n.code,{children:"\\r"})}),(0,s.jsx)(n.td,{children:"carriage return"})]}),(0,s.jsxs)(n.tr,{children:[(0,s.jsx)(n.td,{children:(0,s.jsx)(n.code,{children:"\\t"})}),(0,s.jsx)(n.td,{children:"tab"})]}),(0,s.jsxs)(n.tr,{children:[(0,s.jsx)(n.td,{children:(0,s.jsx)(n.code,{children:"\\x##"})}),(0,s.jsx)(n.td,{children:"Unicode character with code point ## (in hex)"})]}),(0,s.jsxs)(n.tr,{children:[(0,s.jsx)(n.td,{children:(0,s.jsx)(n.code,{children:"\\u####"})}),(0,s.jsx)(n.td,{children:"Unicode character with code point #### (in hex)"})]}),(0,s.jsxs)(n.tr,{children:[(0,s.jsx)(n.td,{children:(0,s.jsx)(n.code,{children:"\\U########"})}),(0,s.jsx)(n.td,{children:"Unicode character with code point ######## (in hex)"})]})]})]}),"\n",(0,s.jsx)(n.h2,{id:"key-values-as-lists",children:"Key values as lists"}),"\n",(0,s.jsxs)(n.p,{children:["Although the standard INI format supports only key values that represent a\nsingle item, Buck2 supports key values that represent a list of items. The\nsyntax is to separate the items in the list using the space (",(0,s.jsx)(n.code,{children:"0x20"}),") character.\nFor example, a key value for the list of command-line flags to be passed to a\ncompiler could be represented as a list of the flags separated by spaces:"]}),"\n",(0,s.jsx)(n.pre,{children:(0,s.jsx)(n.code,{className:"language-ini",children:"flags = -foo -bar -baz -qux\n"})}),"\n",(0,s.jsxs)(n.p,{children:["When a key value is parsed as a list instead of a single item, the separator\ncharacter is interpreted as a separator only when it occurs ",(0,s.jsx)(n.em,{children:"outside of double\nquotes"}),". For example, if ",(0,s.jsx)(n.code,{children:"flags"})," is a key value interpreted as a list of items\nseparated by spaces, then"]}),"\n",(0,s.jsx)(n.pre,{children:(0,s.jsx)(n.code,{className:"language-ini",children:'flags = -foo "-bar \\u0429"\n'})}),"\n",(0,s.jsxs)(n.p,{children:["results in the two strings: ",(0,s.jsx)(n.code,{children:"foo"})," and ",(0,s.jsx)(n.code,{children:"-bar \u0429"}),"; the space character between\n",(0,s.jsx)(n.code,{children:"-bar"})," and ",(0,s.jsx)(n.code,{children:"\\u0429"})," is not interpreted as a separator."]}),"\n",(0,s.jsx)(n.h2,{id:"transclusion-of-values-from-one-key-to-another",children:"Transclusion of values from one key to another"}),"\n",(0,s.jsx)(n.p,{children:"Values from other keys can be transcluded into the current key using the\nfollowing syntax inside the current key value."}),"\n",(0,s.jsx)(n.pre,{children:(0,s.jsx)(n.code,{children:"$(config <section>.<field>)\n"})}),"\n",(0,s.jsxs)(n.p,{children:["For example, to use the ",(0,s.jsx)(n.code,{children:"[go].vendor_path"})," in a custom setting:"]}),"\n",(0,s.jsx)(n.pre,{children:(0,s.jsx)(n.code,{className:"language-ini",children:"[custom_section]custom_value = $(config go.vendor_path)\n"})}),"\n",(0,s.jsx)(n.h2,{id:"comments",children:"Comments"}),"\n",(0,s.jsxs)(n.p,{children:["In addition to the semicolon (",(0,s.jsx)(n.code,{children:";"}),"), you can use the pound sign (",(0,s.jsx)(n.code,{children:"#"}),"), as a\ncomment character in ",(0,s.jsx)(n.code,{children:".buckconfig"}),"."]}),"\n",(0,s.jsx)(n.h2,{id:"buckconfiglocal",children:".buckconfig.local"}),"\n",(0,s.jsxs)(n.p,{children:["The root of your ",(0,s.jsx)(n.a,{href:"/docs/concepts/glossary#project",children:"project"})," may contain a second\nconfiguration file named ",(0,s.jsx)(n.code,{children:".buckconfig.local"}),". Its format is the same as that of\n",(0,s.jsx)(n.code,{children:".buckconfig"}),", but settings in ",(0,s.jsx)(n.code,{children:".buckconfig.local"})," override those in\n",(0,s.jsx)(n.code,{children:".buckconfig"}),". In practice, ",(0,s.jsx)(n.code,{children:".buckconfig"})," is a version-controlled file that\ncontains settings that are applicable to all team members, whereas\n",(0,s.jsx)(n.code,{children:".buckconfig.local"})," is excluded from version control to allow users to define\npersonal settings, such as personal aliases."]}),"\n",(0,s.jsx)(n.h2,{id:"other-initialization-files",children:"Other initialization files"}),"\n",(0,s.jsxs)(n.p,{children:["In addition to the ",(0,s.jsx)(n.code,{children:".buckconfig"})," and ",(0,s.jsx)(n.code,{children:".buckconfig.local"})," files in the project\nroot, Buck2 reads configuration settings from the following additional\nlocations, some of which are actually directories:"]}),"\n",(0,s.jsxs)(n.ol,{children:["\n",(0,s.jsxs)(n.li,{children:["Directory ",(0,s.jsx)(n.code,{children:".buckconfig.d"})," located in the project root directory."]}),"\n",(0,s.jsxs)(n.li,{children:["File ",(0,s.jsx)(n.code,{children:".buckconfig"})," and directory ",(0,s.jsx)(n.code,{children:".buckconfig.d"})," located in the current\nuser's home directory which, on Unix-like systems, is available from the\n",(0,s.jsx)(n.code,{children:"HOME"})," environment variable or through the ",(0,s.jsx)(n.code,{children:"~"})," symbol."]}),"\n",(0,s.jsxs)(n.li,{children:["File ",(0,s.jsx)(n.code,{children:"buckconfig"})," and directory ",(0,s.jsx)(n.code,{children:"buckconfig.d"})," located in system directory\n",(0,s.jsx)(n.code,{children:"/etc/"}),"."]}),"\n"]}),"\n",(0,s.jsxs)(n.p,{children:["Buck2 treats ",(0,s.jsx)(n.em,{children:"any"})," file\u2014irrespective of name\u2014in a\n",(0,s.jsx)(n.code,{children:".buckconfig.d"}),"(",(0,s.jsx)(n.code,{children:"buckconfig.d"}),") directory (excluding files found in\nsubdirectories) as a Buck2 configuration file, provided that it adheres to\n",(0,s.jsx)(n.code,{children:".buckconfig"})," syntax. Note that a ",(0,s.jsx)(n.code,{children:".buckconfig.d"})," directory is distinct from the\nsimilarly-named ",(0,s.jsx)(n.code,{children:".buckd"})," directory which is used by the\n",(0,s.jsxs)(n.a,{href:"/docs/concepts/daemon",children:["Buck2 Daemon (",(0,s.jsx)(n.code,{children:"buckd"}),")"]})," . For a description of how Buck2 resolves\ncollisions between settings in these configuration files, see the section\n",(0,s.jsx)(n.a,{href:"#precedence-of-buck2-configuration-specifications",children:(0,s.jsx)(n.strong,{children:"Precedence of Buck2 configuration specifications"})}),"\nbelow."]}),"\n",(0,s.jsx)(n.h2,{id:"command-line-control-of-configuration",children:"Command-line control of configuration"}),"\n",(0,s.jsxs)(n.p,{children:["In addition to the above configuration files, Buck2 supports specifying\nadditional configuration files from the Buck2 command line using the\n",(0,s.jsx)(n.code,{children:"--config-file"})," parameter. You can also specify configuration settings\n",(0,s.jsx)(n.em,{children:"individually"})," on the Buck2 command line using the ",(0,s.jsx)(n.code,{children:"--config"})," (",(0,s.jsx)(n.code,{children:"-c"}),") parameter.\nFurthermore, you can aggregate these settings into ",(0,s.jsx)(n.em,{children:"flag files"})," using the\n",(0,s.jsx)(n.code,{children:"--flagfile"})," parameter. A flag file provides similar functionality to a\nconfiguration file but uses a different syntax. Flag files are sometimes called\n",(0,s.jsx)(n.em,{children:"mode files"})," or ",(0,s.jsx)(n.em,{children:"at"})," (",(0,s.jsx)(n.code,{children:"@"}),") files."]}),"\n",(0,s.jsx)(n.h2,{id:"precedence-of-buck2-configuration-specifications",children:"Precedence of Buck2 configuration specifications"}),"\n",(0,s.jsxs)(n.p,{children:["The following list shows the order of precedence for how Buck2 interprets its\nconfiguration specifications. Settings specified using a method closer to the\ntop of the list have higher precedence and will override those lower on the\nlist. For example, the ",(0,s.jsx)(n.code,{children:".buckconfig"})," file in the repo overrides a ",(0,s.jsx)(n.code,{children:".buckconfig"}),"\nfile in the user's ",(0,s.jsx)(n.code,{children:"HOME"})," directory."]}),"\n",(0,s.jsxs)(n.ol,{children:["\n",(0,s.jsxs)(n.li,{children:["Configuration specified on the command line using ",(0,s.jsx)(n.code,{children:"--config"})," (",(0,s.jsx)(n.code,{children:"-c"}),"),\n",(0,s.jsx)(n.code,{children:"--config-file"})," and ",(0,s.jsx)(n.code,{children:"--flagfile"}),". Configuration specified later on the\ncommand line overrides configuration specified earlier."]}),"\n",(0,s.jsxs)(n.li,{children:[(0,s.jsx)(n.code,{children:".buckconfig.local"})," in the repo."]}),"\n",(0,s.jsxs)(n.li,{children:[(0,s.jsx)(n.code,{children:".buckconfig"})," in the repo."]}),"\n",(0,s.jsxs)(n.li,{children:["Files in a ",(0,s.jsx)(n.code,{children:".buckconfig.d"})," folder of the repo."]}),"\n",(0,s.jsxs)(n.li,{children:[(0,s.jsx)(n.code,{children:".buckconfig.local"})," in user's ",(0,s.jsx)(n.code,{children:"HOME"})," directory."]}),"\n",(0,s.jsxs)(n.li,{children:["Files in a ",(0,s.jsx)(n.code,{children:".buckconfig.d"})," folder in user's ",(0,s.jsx)(n.code,{children:"HOME"})," directory."]}),"\n",(0,s.jsxs)(n.li,{children:["The global file ",(0,s.jsx)(n.code,{children:"/etc/buckconfig"})]}),"\n",(0,s.jsxs)(n.li,{children:["Files in the global directory ",(0,s.jsx)(n.code,{children:"/etc/buckconfig.d"})]}),"\n"]}),"\n",(0,s.jsxs)(n.p,{children:["Files in a ",(0,s.jsx)(n.code,{children:".buckconfig.d"})," (",(0,s.jsx)(n.code,{children:"buckconfig.d"}),") directory have precedence according\nto the lexicographical order of their file names. Files ",(0,s.jsx)(n.em,{children:"later"})," in the\nlexicographical order have precedence over files earlier in that order."]}),"\n",(0,s.jsx)(n.h2,{id:"configuration-files-can-include-other-files",children:"Configuration files can include other files"}),"\n",(0,s.jsxs)(n.p,{children:["Any of the configuration files that we've discussed so far can also include by\nreference other files that contain configuration information. These included\nfiles can contain complete ",(0,s.jsx)(n.code,{children:".buckconfig"})," sections or they can contain a group of\nkey name/value pairs that constitute part of a section. In this second use case,\nyou'll need to ensure that the ",(0,s.jsx)(n.em,{children:"included"})," file is referenced beneath the\nappropriate section in the ",(0,s.jsx)(n.em,{children:"including"})," file. Because of this additional\ncomplexity, we recommend that you include only files that contain complete\nsections. ",(0,s.jsx)(n.strong,{children:"Note:"})," Inclusion of files is a Buck-specific extension to the INI\nfile parser that Buck2 uses. Therefore, if you use this feature, your Buck2\nconfiguration files will probably not be parsable by other more-generic INI file\nparsers. The syntax to include a file is"]}),"\n",(0,s.jsx)(n.pre,{children:(0,s.jsx)(n.code,{children:"<file:*path-to-included-file*>\n"})}),"\n",(0,s.jsxs)(n.p,{children:["where ",(0,s.jsx)(n.em,{children:"path-to-included-file"})," is either a relative path from the including file\n(recommended) or an absolute path from the root of the file system. You can also\nspecify that the file should be included only if it exists by prefixing with a\nquestion mark (",(0,s.jsx)(n.code,{children:"?"}),")."]}),"\n",(0,s.jsx)(n.pre,{children:(0,s.jsx)(n.code,{children:"<?file:*path-to-included-file*>\n"})}),"\n",(0,s.jsxs)(n.p,{children:["If you use this prefix, it is not an error condition if the file does not exist;\nBuck2 just silently continues to process the rest of the configuration file. In\nthe following example, the ",(0,s.jsx)(n.code,{children:".buckconfig"})," file includes the file\n",(0,s.jsx)(n.code,{children:"cxx-other-platform.include"})," which exists in the subdirectory\n",(0,s.jsx)(n.code,{children:"cxx-other-platform"}),". The ",(0,s.jsx)(n.code,{children:".buckconfig"})," file will also include the file\n",(0,s.jsx)(n.code,{children:"future-platform"})," from the directory ",(0,s.jsx)(n.code,{children:"future-platform.include"})," if that file\nexists."]}),"\n",(0,s.jsx)(n.pre,{children:(0,s.jsx)(n.code,{className:"language-ini",children:'#\n# .buckconfig\n#\n[cxx]\n  cxxppflags="-D MYMACRO=\\"Buck\\""\n\n<file:cxx-other-platform/cxx-other-platform.include>\n\n<?file:future-platform/future-platform.include>\n#\n# cxx-other-platform.include\n#\n[cxx#other_platform]\n  cxxppflags="-D MYMACRO=\\"Watchman\\""\n'})}),"\n",(0,s.jsx)(n.h2,{id:"sections",children:"Sections"}),"\n",(0,s.jsx)(n.p,{children:"Below is an incomplete list of supported buckconfigs."}),"\n",(0,s.jsx)(n.h2,{id:"alias",children:"[alias]"}),"\n",(0,s.jsxs)(n.p,{children:["This section contains definitions of ",(0,s.jsx)(n.a,{href:"/docs/concepts/build_target",children:"build target"})," aliases."]}),"\n",(0,s.jsx)(n.pre,{children:(0,s.jsx)(n.code,{className:"language-ini",children:"[alias]\n  app = //apps/myapp:app\n  apptest = //apps/myapp:test\n"})}),"\n",(0,s.jsx)(n.p,{children:"These aliases can then be used from the command line:"}),"\n",(0,s.jsx)(n.pre,{children:(0,s.jsx)(n.code,{className:"language-sh",children:"$ buck2 build app\n$ buck2 test apptest\n"})}),"\n",(0,s.jsx)(n.h2,{id:"cells",children:"[cells]"}),"\n",(0,s.jsxs)(n.p,{children:["Lists the cells that constitute the Buck2 project. Buck2 builds that are part of\nthis project\u2014that is, which use this ",(0,s.jsx)(n.code,{children:".buckconfig"}),"\u2014can access the cells\nspecified in this section."]}),"\n",(0,s.jsx)(n.pre,{children:(0,s.jsx)(n.code,{className:"language-ini",children:"[cells]\n    buck = .\n    bazel_skylib = ./third-party/skylark/bazel-skylib\n"})}),"\n",(0,s.jsxs)(n.p,{children:["The string on the left-hand side of the equals sign is the ",(0,s.jsx)(n.em,{children:"alias"})," for the cell.\nThe string on the right-hand side of the equals sign is the path to the cell\nfrom the directory that contains this ",(0,s.jsx)(n.code,{children:".buckconfig"})," file. It is not necessary to\ninclude the current cell in this section, but we consider it a best practice to\ndo so:"]}),"\n",(0,s.jsx)(n.pre,{children:(0,s.jsx)(n.code,{className:"language-ini",children:"buck = .\n"})}),"\n",(0,s.jsxs)(n.p,{children:["You can view the contents of this section using the ",(0,s.jsx)(n.code,{children:"buck2 audit cell"})," command."]}),"\n",(0,s.jsxs)(n.p,{children:[(0,s.jsx)(n.code,{children:"[repositories]"})," is additionally supported as a deprecated alternative name for\nthis section."]})]})}function h(e={}){const{wrapper:n}={...(0,o.R)(),...e.components};return n?(0,s.jsx)(n,{...e,children:(0,s.jsx)(d,{...e})}):d(e)}}}]);