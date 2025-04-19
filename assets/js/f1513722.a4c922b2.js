"use strict";(self.webpackChunkwebsite=self.webpackChunkwebsite||[]).push([[8664],{26889:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>u,contentTitle:()=>r,default:()=>c,frontMatter:()=>i,metadata:()=>a,toc:()=>l});const a=JSON.parse('{"id":"users/commands/targets","title":"targets","description":"These are the flags/commands under buck2 targets and their --help output:","source":"@site/../docs/users/commands/targets.generated.md","sourceDirName":"users/commands","slug":"/users/commands/targets","permalink":"/docs/users/commands/targets","draft":false,"unlisted":false,"tags":[],"version":"current","frontMatter":{"id":"targets","title":"targets"},"sidebar":"main","previous":{"title":"subscribe","permalink":"/docs/users/commands/subscribe"},"next":{"title":"test","permalink":"/docs/users/commands/test"}}');var s=n(74848),o=n(28453);const i={id:"targets",title:"targets"},r=void 0,u={},l=[];function h(e){const t={code:"code",p:"p",pre:"pre",...(0,o.R)(),...e.components};return(0,s.jsxs)(s.Fragment,{children:[(0,s.jsxs)(t.p,{children:["These are the flags/commands under ",(0,s.jsx)(t.code,{children:"buck2 targets"})," and their ",(0,s.jsx)(t.code,{children:"--help"})," output:"]}),"\n",(0,s.jsx)(t.pre,{children:(0,s.jsx)(t.code,{className:"language-text",children:"Alias for `utargets`\n\nUsage: buck2-release targets [OPTIONS] [TARGET_PATTERNS]...\n\nArguments:\n  [TARGET_PATTERNS]...\n          Patterns to interpret\n\nOptions:\n      --json\n          Print targets as JSON\n\n      --json-lines\n          Print targets as JSON-lines\n\n      --stats\n          Print statistics of how many entries were processed\n\n      --resolve-alias\n          Print the fully-qualified build target for the specified aliases\n\n      --show-target-hash\n          Print a stable hash of each target after the target name\n\n      --show-unconfigured-target-hash\n          Print a stable unconfigured hash of each target after the target name\n\n      --target-hash-file-mode <TARGET_HASH_FILE_MODE>\n          Modifies computation of target hashes. If set to `PATHS_AND_CONTENTS` (the default), the\n          contents of all files referenced from the targets will be used to compute the target hash.\n          If set to `PATHS_ONLY`, only files' paths contribute to the hash. If set to `NONE` no\n          files will be used. See also --target-hash-modified-paths\n          \n          [default: paths_and_contents]\n          [possible values: paths_only, paths_and_contents, none]\n\n      --target-hash-modified-paths <TARGET_HASH_MODIFIED_PATHS>...\n          Modifies computation of target hashes. Only effective when --target-hash-file-mode is set\n          to `PATHS_ONLY`. If a target or its dependencies reference a file from this set, the\n          target's hash will be different than if this option was omitted. Otherwise, the target's\n          hash will be the same as if this option was omitted\n\n      --target-hash-function <TARGET_HASH_FUNCTION>\n          Selects either the \"fast\" or the \"strong\" target hash function to be used for computing\n          target hashes. While we don't specify the exact algorithm, the \"strong\" algorithm should\n          be a reasonable cryptographic hash (ex. blake3) while the \"fast\" function will likely be a\n          non-crypto hash. Both functions are guaranteed to be deterministic and to have the same\n          value across different platforms/architectures\n          \n          [default: fast]\n          [possible values: sha1, sha256, murmur-hash3, fast, strong]\n\n      --target-hash-recursive <TARGET_HASH_RECURSIVE>\n          When true, emit the hash or target node and all dependencies recursively. When false, hash\n          only the target node\n          \n          [default: true]\n          [possible values: true, false]\n\n  -A, --output-all-attributes\n          Output all attributes, equivalent of --output-attribute ''.\n          \n          Avoid using this flag in automation because it may be expensive to produce certain\n          attributes, and because it makes harder to track which special attributes are used.\n\n  -B, --output-basic-attributes\n          Output basic attributes, namely those the user can supply, plus rule type and package name\n\n  -a, --output-attribute <ATTRIBUTE>\n          Regular expressions to match attributes. Regular expressions are used in \"search\" mode, so\n          for example empty string matches all attributes including special attributes.\n          \n          When using in automation, please specify the regular expression to match the attribute\n          precisely, for example `--output-attribute '^headers$'` to make it easier to track which\n          special attributes are used.\n\n      --output-attributes <ATTRIBUTE>...\n          Deprecated: Use `--output-attribute` instead.\n          \n          List of space-separated attributes to output, --output-attributes attr1 attr2.\n\n      --include-defaults\n          Enables printing of default attributes. This would be attributes in a target that aren't\n          explicitly set in the target but instead use the default set in the rule declaration\n\n      --show-output\n          Print the path to the output for each of the rules relative to the project root\n\n      --show-full-output\n          Print the absolute path to the output for each of the rules\n\n      --show-simple-output\n          Print only the path to the output for each of the rules relative to the project root\n\n      --show-full-simple-output\n          Print only the absolute path to the output for each of the rules\n\n      --show-json-output\n          Print the output paths relative to the project root, in JSON format\n\n      --show-full-json-output\n          Print the output absolute paths, in JSON format\n\n      --keep-going\n          On loading errors, put buck.error in the output stream and continue\n\n      --streaming\n          Write output as soon as it is available. The order of the output items is\n          non-deterministic and if multiple patterns cover the same target, may have duplicates\n\n      --no-cache\n          Don't cache the target information on the build graph\n\n      --imports\n          Show the imports of each package/import. Shows an additional output per package/import\n          (not per target), including implicit dependencies (e.g. the prelude) but only direct\n          dependencies (not the transitive closure)\n\n      --package-values\n          Show the package values. Produces an additional attribute representing all the package\n          values for the package containing the target\n\n      --package-values-regex <VALUES>\n          Regular expressions to match package values. Produces an additional attribute representing\n          package values for the package containing the target. Regular expressions are used in\n          \"search\" mode so, for example, empty string matches all package values\n\n  -o, --output <PATH>\n          File to put the output in, rather than sending to stdout.\n          \n          File will be created if it does not exist, and overwritten if it does.\n\n      --compression <SCHEME>\n          Compress the output\n          \n          [default: none]\n          [possible values: none, gzip, zstd]\n\n  -j, --num-threads <THREADS>\n          Number of threads to use during execution (default is # cores)\n\n  -h, --help\n          Print help (see a summary with '-h')\n\nTarget Configuration Options:\n      --target-platforms <PLATFORM>\n          Configuration target (one) to use to configure targets\n\n  -m, --modifier <VALUE>\n          A configuration modifier to configure all targets on the command line. This may be a\n          constraint value target.\n\nBuckconfig Options:\n  -c, --config <SECTION.OPTION=VALUE>\n          List of config options\n\n      --config-file <PATH>\n          List of config file paths\n\n      --fake-host <HOST>\n          [possible values: default, linux, macos, windows]\n\n      --fake-arch <ARCH>\n          [possible values: default, aarch64, x8664]\n\n      --fake-xcode-version <VERSION-BUILD>\n          Value must be formatted as: version-build (e.g., 14.3.0-14C18 or 14.1-14B47b)\n\n      --reuse-current-config\n          Re-uses any `--config` values (inline or via modefiles) if there's a previous command,\n          otherwise the flag is ignored.\n          \n          If there is a previous command and `--reuse-current-config` is set, then the old config is\n          used, ignoring any overrides.\n          \n          If there is no previous command but the flag was set, then the flag is ignored, the\n          command behaves as if the flag was not set at all.\n\n      --exit-when-different-state\n          Used for exiting a concurrent command when a different state is detected\n\n      --preemptible <PREEMPTIBLE>\n          Used to configure when this command could be preempted by another command for the same\n          isolation dir.\n          \n          Normally, when you run two commands - from different terminals, say - buck2 will attempt\n          to run them in parallel. However, if the two commands are based on different state, that\n          is they either have different configs or different filesystem states, buck2 cannot run\n          them in parallel. The default behavior in this case is to block the second command until\n          the first completes.\n\n          Possible values:\n          - never:            (default) When another command starts that cannot run in parallel with\n            this one, block that command\n          - always:           When another command starts, interrupt this command, *even if they\n            could run in parallel*. There is no good reason to use this other than that it provides\n            slightly nicer superconsole output\n          - ondifferentstate: When another command starts that cannot run in parallel with this one,\n            interrupt this command\n\nStarlark Options:\n      --disable-starlark-types\n          Disable runtime type checking in Starlark interpreter.\n          \n          This option is not stable, and can be used only locally to diagnose evaluation performance\n          problems.\n\n      --stack\n          Record or show target call stacks.\n          \n          Starlark call stacks will be included in duplicate targets error.\n          \n          If a command outputs targets (like `targets` command), starlark call stacks will be\n          printed after the targets.\n\nConsole Options:\n      --console <super|simple|...>\n          Which console to use for this command\n          \n          [env: BUCK_CONSOLE=]\n          [default: auto]\n          [possible values: auto, none, simple, simplenotty, simpletty, super]\n\n      --ui <UI>...\n          Configure additional superconsole ui components.\n          \n          Accepts a comma-separated list of superconsole components to add. Possible values are:\n          \n          dice - shows information about evaluated dice nodes debugevents - shows information about\n          the flow of events from buckd\n          \n          These components can be turned on/off interactively. Press 'h' for help when superconsole\n          is active.\n\n          Possible values:\n          - dice\n          - debugevents\n          - io:          I/O panel\n          - re:          RE panel\n\n      --no-interactive-console\n          Disable console interactions\n          \n          [env: BUCK_NO_INTERACTIVE_CONSOLE=]\n\nEvent Log Options:\n      --event-log <PATH>\n          Write events to this log file\n\n      --write-build-id <PATH>\n          Write command invocation id into this file\n\n      --unstable-write-invocation-record <PATH>\n          Write the invocation record (as JSON) to this path. No guarantees whatsoever are made\n          regarding the stability of the format\n\n      --command-report-path <PATH>\n          Write the command report to this path. A command report is always written to\n          `buck-out/v2/<uuid>/command_report` even without this flag\n\nUniversal Options:\n  -v, --verbose <VERBOSITY>\n          How verbose buck should be while logging.\n          \n          Values: 0 = Quiet, errors only; 1 = Show status. Default; 2 = more info about errors; 3 =\n          more info about everything; 4 = more info about everything + stderr;\n          \n          It can be combined with specific log items (stderr, full_failed_command, commands,\n          actions, status, stats, success) to fine-tune the verbosity of the log. Example usage\n          \"-v=1,stderr\"\n          \n          [default: 1]\n\n      --oncall <ONCALL>\n          The oncall executing this command\n\n      --client-metadata <CLIENT_METADATA>\n          Metadata key-value pairs to inject into Buck2's logging. Client metadata must be of the\n          form `key=value`, where `key` is a snake_case identifier, and will be sent to backend\n          datasets\n\n"})})]})}function c(e={}){const{wrapper:t}={...(0,o.R)(),...e.components};return t?(0,s.jsx)(t,{...e,children:(0,s.jsx)(h,{...e})}):h(e)}},28453:(e,t,n)=>{n.d(t,{R:()=>i,x:()=>r});var a=n(96540);const s={},o=a.createContext(s);function i(e){const t=a.useContext(o);return a.useMemo((function(){return"function"==typeof e?e(t):{...t,...e}}),[t,e])}function r(e){let t;return t=e.disableParentContext?"function"==typeof e.components?e.components(s):e.components||s:i(e.components),a.createElement(o.Provider,{value:t},e.children)}}}]);