"use strict";(self.webpackChunkwebsite=self.webpackChunkwebsite||[]).push([[7556],{28453:(e,t,n)=>{n.d(t,{R:()=>r,x:()=>i});var a=n(96540);const o={},s=a.createContext(o);function r(e){const t=a.useContext(s);return a.useMemo((function(){return"function"==typeof e?e(t):{...t,...e}}),[t,e])}function i(e){let t;return t=e.disableParentContext?"function"==typeof e.components?e.components(o):e.components||o:r(e.components),a.createElement(s.Provider,{value:t},e.children)}},42153:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>u,contentTitle:()=>i,default:()=>d,frontMatter:()=>r,metadata:()=>a,toc:()=>c});const a=JSON.parse('{"id":"users/commands/aquery","title":"aquery","description":"These are the flags/commands under buck2 aquery and their --help output:","source":"@site/../docs/users/commands/aquery.generated.md","sourceDirName":"users/commands","slug":"/users/commands/aquery","permalink":"/docs/users/commands/aquery","draft":false,"unlisted":false,"tags":[],"version":"current","frontMatter":{"id":"aquery","title":"aquery"},"sidebar":"main","previous":{"title":"Glossary of Terms","permalink":"/docs/concepts/glossary"},"next":{"title":"audit","permalink":"/docs/users/commands/audit"}}');var o=n(74848),s=n(28453);const r={id:"aquery",title:"aquery"},i=void 0,u={},c=[];function l(e){const t={code:"code",p:"p",pre:"pre",...(0,s.R)(),...e.components};return(0,o.jsxs)(o.Fragment,{children:[(0,o.jsxs)(t.p,{children:["These are the flags/commands under ",(0,o.jsx)(t.code,{children:"buck2 aquery"})," and their ",(0,o.jsx)(t.code,{children:"--help"})," output:"]}),"\n",(0,o.jsx)(t.pre,{children:(0,o.jsx)(t.code,{className:"language-text",children:"Perform queries on the action graph (experimental)\n\nThe action graph consists of all the declared actions for a build,\nwith dependencies when one action consumes the outputs of another\naction.\n\nRun `buck2 docs aquery` or\nhttps://buck2.build/docs/users/query/aquery/\nfor more documentation about the functions available in aquery\nexpressions.\n\nExamples:\n\nPrint the action producing a target's default output\n\n`buck2 aquery //java/com/example/app:amazing`\n\nList all the commands for run actions for building a target\n\n`buck2 aquery 'kind(run, deps(\"//java/com/example/app:amazing+more\"))' --output-attribute=cmd`\n\nDynamic outputs (`ctx.actions.dynamic_output`):\n\nCurrently, aquery interacts poorly with dynamic outputs. It may\nreturn incorrect results or otherwise behave unexpectedly.\n\nUsage: buck2-release aquery [OPTIONS] <QUERY> [QUERY_ARGS]...\n\nArguments:\n  <QUERY>\n          the query to evaluate\n\n  [QUERY_ARGS]...\n          list of literals for a multi-query (one containing `%s` or `%Ss`)\n\nOptions:\n  -A, --output-all-attributes\n          Output all attributes, equivalent of --output-attribute ''.\n          \n          Avoid using this flag in automation because it may be expensive to produce certain\n          attributes, and because it makes harder to track which special attributes are used.\n\n  -B, --output-basic-attributes\n          Output basic attributes, namely those the user can supply, plus rule type and package name\n\n  -a, --output-attribute <ATTRIBUTE>\n          Regular expressions to match attributes. Regular expressions are used in \"search\" mode, so\n          for example empty string matches all attributes including special attributes.\n          \n          When using in automation, please specify the regular expression to match the attribute\n          precisely, for example `--output-attribute '^headers$'` to make it easier to track which\n          special attributes are used.\n\n      --output-attributes <ATTRIBUTE>...\n          Deprecated: Use `--output-attribute` instead.\n          \n          List of space-separated attributes to output, --output-attributes attr1 attr2.\n\n      --json\n          Output in JSON format\n\n      --dot\n          Output in Graphviz Dot format\n\n      --dot-compact\n          Output in a more compact format than Graphviz Dot\n\n      --output-format <dot|dot_compact|json|starlark|html>\n          Output format (default: list). \n          \n                     dot -  dot graph format. \n          \n                     dot_compact - compact alternative to dot format. \n          \n                     json - JSON format. \n          \n                     starlark - targets are printed like starlark code that would produce them.\n                     html - html file containing interactive target graph.\n                   \n          \n          [possible values: dot, json, dot_compact, starlark, html]\n\n  -h, --help\n          Print help (see a summary with '-h')\n\nTarget Configuration Options:\n      --target-platforms <PLATFORM>\n          Configuration target (one) to use to configure targets\n\n  -m, --modifier <VALUE>\n          A configuration modifier to configure all targets on the command line. This may be a\n          constraint value target.\n\nBuckconfig Options:\n  -c, --config <SECTION.OPTION=VALUE>\n          List of config options\n\n      --config-file <PATH>\n          List of config file paths\n\n      --fake-host <HOST>\n          [possible values: default, linux, macos, windows]\n\n      --fake-arch <ARCH>\n          [possible values: default, aarch64, x8664]\n\n      --fake-xcode-version <VERSION-BUILD>\n          Value must be formatted as: version-build (e.g., 14.3.0-14C18 or 14.1-14B47b)\n\n      --reuse-current-config\n          Re-uses any `--config` values (inline or via modefiles) if there's a previous command,\n          otherwise the flag is ignored.\n          \n          If there is a previous command and `--reuse-current-config` is set, then the old config is\n          used, ignoring any overrides.\n          \n          If there is no previous command but the flag was set, then the flag is ignored, the\n          command behaves as if the flag was not set at all.\n\n      --exit-when-different-state\n          Used for exiting a concurrent command when a different state is detected\n\n      --preemptible <PREEMPTIBLE>\n          Used to configure when this command could be preempted by another command for the same\n          isolation dir.\n          \n          Normally, when you run two commands - from different terminals, say - buck2 will attempt\n          to run them in parallel. However, if the two commands are based on different state, that\n          is they either have different configs or different filesystem states, buck2 cannot run\n          them in parallel. The default behavior in this case is to block the second command until\n          the first completes.\n\n          Possible values:\n          - never:            (default) When another command starts that cannot run in parallel with\n            this one, block that command\n          - always:           When another command starts, interrupt this command, *even if they\n            could run in parallel*. There is no good reason to use this other than that it provides\n            slightly nicer superconsole output\n          - ondifferentstate: When another command starts that cannot run in parallel with this one,\n            interrupt this command\n\nStarlark Options:\n      --disable-starlark-types\n          Disable runtime type checking in Starlark interpreter.\n          \n          This option is not stable, and can be used only locally to diagnose evaluation performance\n          problems.\n\n      --stack\n          Record or show target call stacks.\n          \n          Starlark call stacks will be included in duplicate targets error.\n          \n          If a command outputs targets (like `targets` command), starlark call stacks will be\n          printed after the targets.\n\nConsole Options:\n      --console <super|simple|...>\n          Which console to use for this command\n          \n          [env: BUCK_CONSOLE=]\n          [default: auto]\n          [possible values: auto, none, simple, simplenotty, simpletty, super]\n\n      --ui <UI>...\n          Configure additional superconsole ui components.\n          \n          Accepts a comma-separated list of superconsole components to add. Possible values are:\n          \n          dice - shows information about evaluated dice nodes debugevents - shows information about\n          the flow of events from buckd\n          \n          These components can be turned on/off interactively. Press 'h' for help when superconsole\n          is active.\n\n          Possible values:\n          - dice\n          - debugevents\n          - io:          I/O panel\n          - re:          RE panel\n\n      --no-interactive-console\n          Disable console interactions\n          \n          [env: BUCK_NO_INTERACTIVE_CONSOLE=]\n\nEvent Log Options:\n      --event-log <PATH>\n          Write events to this log file\n\n      --write-build-id <PATH>\n          Write command invocation id into this file\n\n      --unstable-write-invocation-record <PATH>\n          Write the invocation record (as JSON) to this path. No guarantees whatsoever are made\n          regarding the stability of the format\n\n      --command-report-path <PATH>\n          Write the command report to this path. A command report is always written to\n          `buck-out/v2/<uuid>/command_report` even without this flag\n\nUniversal Options:\n  -v, --verbose <VERBOSITY>\n          How verbose buck should be while logging.\n          \n          Values: 0 = Quiet, errors only; 1 = Show status. Default; 2 = more info about errors; 3 =\n          more info about everything; 4 = more info about everything + stderr;\n          \n          It can be combined with specific log items (stderr, full_failed_command, commands,\n          actions, status, stats, success) to fine-tune the verbosity of the log. Example usage\n          \"-v=1,stderr\"\n          \n          [default: 1]\n\n      --oncall <ONCALL>\n          The oncall executing this command\n\n      --client-metadata <CLIENT_METADATA>\n          Metadata key-value pairs to inject into Buck2's logging. Client metadata must be of the\n          form `key=value`, where `key` is a snake_case identifier, and will be sent to backend\n          datasets\n\n"})})]})}function d(e={}){const{wrapper:t}={...(0,s.R)(),...e.components};return t?(0,o.jsx)(t,{...e,children:(0,o.jsx)(l,{...e})}):l(e)}}}]);