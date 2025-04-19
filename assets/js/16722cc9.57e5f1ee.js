"use strict";(self.webpackChunkwebsite=self.webpackChunkwebsite||[]).push([[4440],{6557:(e,n,t)=>{t.r(n),t.d(n,{assets:()=>c,contentTitle:()=>r,default:()=>u,frontMatter:()=>l,metadata:()=>s,toc:()=>d});const s=JSON.parse('{"id":"users/build_observability/logging","title":"Logging","description":"Buck2 produces detailed event logs for each invocation, which follow a schema","source":"@site/../docs/users/build_observability/logging.md","sourceDirName":"users/build_observability","slug":"/users/build_observability/logging","permalink":"/docs/users/build_observability/logging","draft":false,"unlisted":false,"tags":[],"version":"current","frontMatter":{"id":"logging","title":"Logging"},"sidebar":"main","previous":{"title":"Buck2 Consoles","permalink":"/docs/users/build_observability/interactive_console"},"next":{"title":"Build Report","permalink":"/docs/users/build_observability/build_report"}}');var o=t(74848),a=t(28453),i=t(78191);const l={id:"logging",title:"Logging"},r=void 0,c={},d=[{value:"Event log format",id:"event-log-format",level:2},{value:"Invocation header",id:"invocation-header",level:3},{value:"Command result footer",id:"command-result-footer",level:3},{value:"Buck events",id:"buck-events",level:3},{value:"Span starts",id:"span-starts",level:4},{value:"Span ends",id:"span-ends",level:4},{value:"Instant events",id:"instant-events",level:4},{value:"Viewing the event log",id:"viewing-the-event-log",level:2}];function h(e){const n={a:"a",code:"code",h2:"h2",h3:"h3",h4:"h4",li:"li",p:"p",pre:"pre",ul:"ul",...(0,a.R)(),...e.components};return(0,o.jsxs)(o.Fragment,{children:[(0,o.jsxs)(n.p,{children:["Buck2 produces detailed event logs for each invocation, which follow a schema\noutlined in ",(0,o.jsx)(n.code,{children:"app/buck2_data/data.proto"})," in the buck2 parent directory. The event\nlogs that Buck2 produces automatically are always in protobuf zstd-compressed\nformat (see ",(0,o.jsx)(n.a,{href:"#viewing-the-event-log",children:"Viewing the event log"})," for more details)."]}),"\n",(0,o.jsx)(n.h2,{id:"event-log-format",children:"Event log format"}),"\n",(0,o.jsxs)(n.p,{children:["Warning: the schemas are all subject to change, so we do not recommend relying\non the format. For the source of truth, take a look at ",(0,o.jsx)(n.code,{children:"data.proto"}),"."]}),"\n",(0,o.jsx)(n.h3,{id:"invocation-header",children:"Invocation header"}),"\n",(0,o.jsxs)(n.p,{children:["The first line of the event log is always the ",(0,o.jsx)(n.code,{children:"Invocation"})," header:"]}),"\n",(0,o.jsx)(n.pre,{children:(0,o.jsx)(n.code,{className:"language-python",children:"Invocation {\n    # CLI args split into a list of strings\n    command_line_args: List[str],\n    # Expanded CLI args, which expand any argsfiles\n    expanded_command_line_args: List[str],\n    # Absolute path of the current working directory of the Buck2 command\n    working_dir: str,\n    # UUID of the Buck2 command\n    trace_id: str,\n}\n"})}),"\n",(0,o.jsx)(n.h3,{id:"command-result-footer",children:"Command result footer"}),"\n",(0,o.jsxs)(n.p,{children:["The last line is always the ",(0,o.jsx)(n.code,{children:"CommandResult"}),":"]}),"\n",(0,o.jsx)(n.pre,{children:(0,o.jsx)(n.code,{className:"language-python",children:"Result {\n    # One of the result types of CommandResult protobuf type in data.proto\n    result: BuildResponse | CqueryResponse | BxlResponse | ...,\n}\n"})}),"\n",(0,o.jsx)(n.h3,{id:"buck-events",children:"Buck events"}),"\n",(0,o.jsxs)(n.p,{children:["The rest of the event log contain ",(0,o.jsx)(n.code,{children:"BuckEvent"}),"s, which are either\n",(0,o.jsx)(n.code,{children:"SpanStartEvent"}),"s, ",(0,o.jsx)(n.code,{children:"SpanEndEvent"}),"s, or ",(0,o.jsx)(n.code,{children:"InstantEvent"}),"s."]}),"\n",(0,o.jsxs)(n.p,{children:["The ",(0,o.jsx)(n.code,{children:"BuckEvent"})," format is roughly as follows:"]}),"\n",(0,o.jsx)(n.pre,{children:(0,o.jsx)(n.code,{className:"language-python",children:"Event {\n    # When the event was fired. This is always a 2-item list, where the first\n    # value is millis, second value is micros\n    timestamp: List[u64],\n    # UUID of the Buck2 command, same one as the invocation header\n    trace_id: str,\n    # A trace-unique 64-bit integer identifying this event's span ID,\n    # if this event begins a new span or belongs to one.\n    span_id: u64,\n    # A trace-unique 64-bit identifying the span that this event is logically\n    # parented to.\n    parent_id: u64,\n    # See sections below for more details\n    data: SpanStart | SpanEnd | Instant,\n}\n"})}),"\n",(0,o.jsx)(n.h4,{id:"span-starts",children:"Span starts"}),"\n",(0,o.jsxs)(n.p,{children:["The ",(0,o.jsx)(n.code,{children:"SpanStartEvent"})," indicates that a span of work starting:"]}),"\n",(0,o.jsx)(n.pre,{children:(0,o.jsx)(n.code,{className:"language-python",children:"SpanStart {\n    # One of the data types of SpanStartEvent protobuf type in data.proto\n    data: AnalysisStart | ActionExecutionStart | ...,\n}\n"})}),"\n",(0,o.jsx)(n.h4,{id:"span-ends",children:"Span ends"}),"\n",(0,o.jsxs)(n.p,{children:["The ",(0,o.jsx)(n.code,{children:"SpanEndEvent"})," indicates that a span of work has finished:"]}),"\n",(0,o.jsx)(n.pre,{children:(0,o.jsx)(n.code,{className:"language-python",children:"SpanEnd {\n    # Duration of the span\n    duration_us: u64,\n    # CPU poll times for this span\n    stats: SpanStats,\n    # One of the data types of SpanEndEvent protobuf type in data.proto\n    data: AnalysisEnd | ActionExecutionEnd | ...,\n}\n\n# CPU poll times for this span\nSpanStats {\n  max_poll_time_us: u64,\n  total_poll_time_us: u64,\n}\n"})}),"\n",(0,o.jsx)(n.h4,{id:"instant-events",children:"Instant events"}),"\n",(0,o.jsxs)(n.p,{children:["The ",(0,o.jsx)(n.code,{children:"InstantEvent"})," represents a single point in time:"]}),"\n",(0,o.jsx)(n.pre,{children:(0,o.jsx)(n.code,{className:"language-python",children:"InstantEvent {\n    # One of the data types of InstantEvent protobuf type in data.proto\n    data: ConsoleMessage | ActionError | ...,\n}\n"})}),"\n",(0,o.jsxs)(n.p,{children:["One specific instant event type that may be of interest is the ",(0,o.jsx)(n.code,{children:"SnapShot"})," event,\nwhich includes some interesting details like RSS, CPU, I/O, remote execution,\nand DICE metrics."]}),"\n",(0,o.jsx)(n.h2,{id:"viewing-the-event-log",children:"Viewing the event log"}),"\n",(0,o.jsxs)(n.p,{children:["Event logs can be accessed using commands under ",(0,o.jsx)(n.code,{children:"buck2 log show"}),", which outputs\nthe event logs in JSONL format. You can run ",(0,o.jsx)(n.code,{children:"buck2 log show --help"})," to see all\navailable options. Some useful commands:"]}),"\n",(0,o.jsxs)(n.ul,{children:["\n",(0,o.jsx)(n.li,{children:"Show the logs for the most recent Buck2 command:"}),"\n"]}),"\n",(0,o.jsx)(n.pre,{children:(0,o.jsx)(n.code,{className:"language-sh",children:"buck2 log show\n"})}),"\n",(0,o.jsxs)(n.ul,{children:["\n",(0,o.jsx)(n.li,{children:"Show the logs for a specific Buck2 command, given the command's UUID:"}),"\n"]}),"\n",(0,o.jsx)(n.pre,{children:(0,o.jsx)(n.code,{className:"language-sh",children:"buck2 log show --trace-id <UUID>\n"})}),"\n",(0,o.jsxs)(n.ul,{children:["\n",(0,o.jsx)(n.li,{children:"Show the logs for a recent Buck2 command:"}),"\n"]}),"\n",(0,o.jsx)(n.pre,{children:(0,o.jsx)(n.code,{className:"language-sh",children:"buck2 log show --recent <NUMBER>\n"})}),"\n",(0,o.jsx)(i.FbInternalOnly,{children:(0,o.jsxs)(n.p,{children:["You can also download the logs locally from Buck2 UI. The logs will be\ndownloaded from Manifold in protobuf zstd-compressed format, and you can view\nthem in JSONL format by passing the path into ",(0,o.jsx)(n.code,{children:"buck2 log show"}),"."]})}),"\n",(0,o.jsxs)(n.p,{children:["The JSON schema is derived from the protobuf types, and the log itself could be\nquite large. ",(0,o.jsx)(n.a,{href:"https://jqlang.github.io/jq/",children:"jq"})," can be useful to find specific\nthings. For example, this jq script shows the max event delay between a snapshot\nevent creation on the daemon side, and when the client receives it."]}),"\n",(0,o.jsx)(n.pre,{children:(0,o.jsx)(n.code,{className:"language-sh",children:"buck2 log show | jq -s '\n  map(\n    .Event.data.Instant.data.Snapshot.this_event_client_delay_ms\n      | select(. != null)\n  ) | max'\n"})})]})}function u(e={}){const{wrapper:n}={...(0,a.R)(),...e.components};return n?(0,o.jsx)(n,{...e,children:(0,o.jsx)(h,{...e})}):h(e)}},28453:(e,n,t)=>{t.d(n,{R:()=>i,x:()=>l});var s=t(96540);const o={},a=s.createContext(o);function i(e){const n=s.useContext(a);return s.useMemo((function(){return"function"==typeof e?e(n):{...n,...e}}),[n,e])}function l(e){let n;return n=e.disableParentContext?"function"==typeof e.components?e.components(o):e.components||o:i(e.components),s.createElement(a.Provider,{value:n},e.children)}}}]);