"use strict";(self.webpackChunkwebsite=self.webpackChunkwebsite||[]).push([[1641],{28453:(e,t,n)=>{n.d(t,{R:()=>a,x:()=>s});var r=n(96540);const i={},o=r.createContext(i);function a(e){const t=r.useContext(o);return r.useMemo((function(){return"function"==typeof e?e(t):{...t,...e}}),[t,e])}function s(e){let t;return t=e.disableParentContext?"function"==typeof e.components?e.components(i):e.components||i:a(e.components),r.createElement(o.Provider,{value:t},e.children)}},49565:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>l,contentTitle:()=>s,default:()=>d,frontMatter:()=>a,metadata:()=>r,toc:()=>h});const r=JSON.parse('{"id":"users/build_observability/build_report","title":"Build Report","description":"The build report is a JSON file that you can ask buck to output which contains","source":"@site/../docs/users/build_observability/build_report.md","sourceDirName":"users/build_observability","slug":"/users/build_observability/build_report","permalink":"/docs/users/build_observability/build_report","draft":false,"unlisted":false,"tags":[],"version":"current","frontMatter":{"id":"build_report","title":"Build Report"},"sidebar":"main","previous":{"title":"Logging","permalink":"/docs/users/build_observability/logging"},"next":{"title":"Remote Execution","permalink":"/docs/users/remote_execution"}}');var i=n(74848),o=n(28453);const a={id:"build_report",title:"Build Report"},s=void 0,l={},h=[{value:"Schema",id:"schema",level:2},{value:"On Compatibility",id:"on-compatibility",level:3},{value:"Limitations",id:"limitations",level:3}];function c(e){const t={code:"code",em:"em",h2:"h2",h3:"h3",li:"li",ol:"ol",p:"p",pre:"pre",strong:"strong",...(0,o.R)(),...e.components};return(0,i.jsxs)(i.Fragment,{children:[(0,i.jsxs)(t.p,{children:["The build report is a JSON file that you can ask buck to output which contains\nstructured information about the result of your build. It is particularly\nvaluable for its reporting of ",(0,i.jsx)(t.em,{children:"unsuccessful"})," outcomes in addition to\n",(0,i.jsx)(t.em,{children:"successful"})," ones; usually, most use cases that only need to care about\nsuccessful outcomes are well served by direct usage of the CLI."]}),"\n",(0,i.jsxs)(t.p,{children:["To request a build report, pass ",(0,i.jsx)(t.code,{children:"--build-report <path>"})," to ",(0,i.jsx)(t.code,{children:"buck build"})," on the\nCLI."]}),"\n",(0,i.jsxs)(t.p,{children:["At a high level, the build report outputs information for each of the targets\nthat you requested to have built on the CLI. As a result, it may report\ninformation for more than one configuration or subtarget of a target. For\nexample, this can happen if you passed ",(0,i.jsx)(t.code,{children:"--target-platforms"})," or built ",(0,i.jsx)(t.code,{children:":target"}),"\nand ",(0,i.jsx)(t.code,{children:":target[sub]"}),"."]}),"\n",(0,i.jsx)(t.h2,{id:"schema",children:"Schema"}),"\n",(0,i.jsx)(t.pre,{children:(0,i.jsx)(t.code,{className:"language-python",children:'BuildReport {\n    # A unique ID identifying this buck invocation. Currently a UUID, however\n    # that may change in the future.\n    trace_id: str,\n\n    # True if all requested targets built successfully\n    success: bool,\n\n    # The absolute path to the project root\n    project_root: Path,\n\n    # The results of the build, categorized by unconfigured target\n    results: dict[TargetLabel, BuildReportEntry],\n\n    # A cache for error message lookup. This is meant for deduplicating strings\n    # that might otherwise appear many times in the build report and cause an\n    # unnecessary size increase. They keys are used in other fields in the build\n    # report in reference to these strings.\n    strings: dict[str, str],\n\n    # BUCK1 BACKCOMPAT ONLY!\n    #\n    # Currently always empty. Will be filled in if a flag is passed in the future.\n    #\n    # A map from targets that failed to build to error messages describing the\n    # failure.\n    failures: dict[TargetLabel, str],\n}\n\nBuildReportEntry {\n    # The results of building the target in the given configurations\n    configured: dict[Configuration, ConfiguredBuildReportEntry],\n\n    # Errors encountered while building this target.\n    #\n    # Note that this does not include the errors that are found within the\n    # `ConfiguredBuildReportEntry`s. Instead, it includes additional errors\n    # which could not be associated with a specific configuration of the\n    # target, typically because they occurred before the target could be\n    # configured.\n    errors: list[Error],\n\n    # BUCK1 BACKCOMPAT ONLY!\n    #\n    # The two fields below are included for buck1 backwards compatibility only.\n    # They are both computed by aggregating across all the configured targets in\n    # the way you might expect.\n    success: "FAIL" | "SUCCESS,\n    outputs: dict[str, list[Path]],\n\n    # The path to the package containing this target, relative to the project\n    # root. This is the source code location for this target.\n    package_project_relative_path: Optional[str]\n}\n\nConfiguredBuildReportEntry {\n    # Did this target build successfully or not?\n    success: "FAIL" | "SUCCESS,\n\n    # A map of subtargets that were built to a list of the successfully built\n    # outputs for that subtarget.\n    #\n    # The keys are generated by joining the subtargets with a `|`. For example,\n    # if you request to have `:target` and `:target[foo][bar]` built on the CLI,\n    # this list will contain one entry for `""` and one for `"foo|bar"`.\n    outputs: dict[str, list[Path]],\n\n    # The number of targets in the configured dependency graph of this target.\n    #\n    # This is only included if `-c buck2.log_configured_graph_size=true` is set.\n    # Otherwise, it is left as None.\n    configured_graph_size: Optional[uint],\n\n    # Information about this particular artifact. Includes things like its hash, whether it is\n    # executable, etc.\n    artifact_info: dict[str, ArtifactInfoFile | ArtifactInfoSymlink | ArtifactInfoExternalSymlink],\n}\n\nError {\n    # The stringified hash of the same stringified error message that is shown to the user on the\n    # console. The hash is stored as the key in the `strings` cache of the `BuildReport`\n    message_content: str,\n\n    # Structured action error. Present only if the error was actually an action error\n    action_error: Optional[ActionError],\n\n    # An index that can be used to detect duplicate errors. Two errors with the\n    # same cause index have the same cause. Note that that does not mean that\n    # they have the same error message.\n    cause_index: uint,\n\n    # List of error tags associated with the error. The error tags provide hints to the error category\n    # that the error is associated to as determined by Buck2 internally. This is meant to classify errors\n    # more precisely, helping developers better understand the nature of the error.\n    error_tags: list[str],\n}\n\nActionError {\n    # The action key\n    key: ActionKey,\n\n    # The action name\n    name: ActionName,\n\n    # Digest of the action\n    digest: str,\n\n    # Stringified hash of the stderr of the action\n    stderr: str,\n\n    # Stringified hash of the stdout of the action\n    stdout: str,\n\n    # Stringified hash of the same stringified error message that is provided by the action\n    error: str,\n\n    # Optional list of error categorizations provided by an error handler which is invoked\n    # in the event of a failed action, or an error message if the error handler failed.\n    error_diagnostics: Optional[ActionErrorDiagnostics],\n}\n\nActionKey {\n    # The configured target, anon target, or bxl function which owns this action\n    owner: str,\n}\n\nActionName {\n    # The category of the action\n    category: str,\n\n    # The optional identifier of the action\n    identifier: Optional[str],\n}\n\nenum ActionErrorDiagnostics {\n    # The list of sub errors if the error handler succeeded\n    sub_errors: list[ActionSubError],\n\n    # The stringified hash of the error message if the error handler failed\n    handler_invocation_error: String,\n}\n\nActionSubError {\n    # Name of the error category. The category should be finer grain error categorizations\n    # provided by the rule authors, and tend to be language specific. These should not be\n    # any kind of shared concepts among all errors for all languages/rules. For example,\n    # timeouts and infra errors should not go here - buck2 tries to categorize these types\n    # of errors automatically. An example of a finer grain error category may be the error\n    # code for rustc outputs.\n    category: str,\n\n    # The stringified hash of the extra message provided for the specific sub-error category.\n    message_content: str,\n\n    # List of error locations, if any\n    locations: Optional[list[ActionErrorLocation]],\n}\n\nActionErrorLocation {\n    # File path where the error appeared, preferrably either project-relative or absolute.\n    file: str,\n\n    # Optional line number\n    line: Optional[u64]\n}\n\nArtifactInfoFile {\n    # The type of this artifact info. This will always be "file".\n    kind: str,\n\n    # CAS digest for this file, includes hash and file size.\n    digest: str,\n    # Whether this file is executable.\n    is_exec: bool,\n}\n\nArtifactInfoSymlink {\n    # The type of this artifact info. This will always be "symlink".\n    kind: str,\n\n    # Symlink target path relative to the root directory.\n    symlink_rel_path: str\n}\n\nArtifactInfoExternalSymlink {\n    # The type of this artifact info. This will always be "external_symlink".\n    kind: str,\n\n    # Symlink target path.\n    target: str,\n    # An optional trailing path component. Join this with the `target`\n    field to get the full symlink file path.\n    remaining_path: Optional[str],\n}\n'})}),"\n",(0,i.jsx)(t.h3,{id:"on-compatibility",children:"On Compatibility"}),"\n",(0,i.jsx)(t.p,{children:"The format of the build report is generally stable. However, note that new\nfields may be added at any time, and you should ensure this does not cause your\nparsing to fail."}),"\n",(0,i.jsx)(t.p,{children:"A number of fields above are marked as being for buck1 backwards compatibility\nonly. These fields all have superior alternatives available in the build report\nalready. We would strongly prefer that new code neither use nor parse them, as\nthis increases the likelyhood that they can be removed one day."}),"\n",(0,i.jsxs)(t.p,{children:["The build report additionally outputs a few fields that are intentionally not\ndocumented here. Those fields are even less useful than ones documented as being\nfor backwards compatibility only, and even closer to removal. ",(0,i.jsx)(t.strong,{children:"Please"})," avoid\nusing or parsing these if at all possible."]}),"\n",(0,i.jsx)(t.h3,{id:"limitations",children:"Limitations"}),"\n",(0,i.jsx)(t.p,{children:"The build report currently has at least the following limitations:"}),"\n",(0,i.jsxs)(t.ol,{children:["\n",(0,i.jsxs)(t.li,{children:["It includes only one action error per failed target. This is the expected\nbehavior when ",(0,i.jsx)(t.code,{children:"--keep-going"})," is not passed, but when ",(0,i.jsx)(t.code,{children:"--keep-going"})," is\npassed, this is a bug."]}),"\n",(0,i.jsx)(t.li,{children:"It is currently not generated when a non-existant package is specified on\nthe command line. This is also a bug."}),"\n",(0,i.jsxs)(t.li,{children:["It cannot be requested for any buck2 command other than ",(0,i.jsx)(t.code,{children:"build"})]}),"\n",(0,i.jsx)(t.li,{children:"Errors do not contain any additional metadata outside of the error message.\nThis will be made available as such metadata is available in buck2."}),"\n",(0,i.jsx)(t.li,{children:'The "failures" field is always empty. This will be changed under a\nbackcompat opt-in flag in the future.'}),"\n"]}),"\n",(0,i.jsx)(t.p,{children:"Finally, it's worth raising that the concept of error deduplication has some\nfundamental limitations; if two targets both refer to the same non-existant\ndependency, do those errors have the same cause (the dependency doesn't exist)\nor different causes (each target is individually broken)? As a result, the exact\ndetails of when two errors are considered to have the same cause are not\ngenerally stable, and may not always be what you expect."})]})}function d(e={}){const{wrapper:t}={...(0,o.R)(),...e.components};return t?(0,i.jsx)(t,{...e,children:(0,i.jsx)(c,{...e})}):c(e)}}}]);