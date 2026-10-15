/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Best-effort detection of Buck calls whose output is consumed directly by an
//! agent, walking client ancestry through command shells and stopping at tools
//! or scripts. Inherited metadata cannot establish this boundary; executable
//! names and `exec` can also mislead detection. The inference is logged to improve
//! the classifier; only explicit `direct_call` controls advice.

use std::ffi::OsString;
use std::path::Path;
use std::time::Duration;

use buck2_hash::BuckMutSet;
use buck2_wrapper_common::kill::process_creation_time;
use sysinfo::Pid;
use sysinfo::ProcessRefreshKind;
use sysinfo::ProcessesToUpdate;
use sysinfo::System;
use sysinfo::UpdateKind;

// Bound work even for corrupt snapshots or unexpectedly deep shell nesting.
const MAX_PROCESS_DEPTH: usize = 64;

const AGENT_EXE_NAMES: &[&str] = &["codex", "claude", "muse", "metacode", "pi"];
const UNIX_SHELLS: &[&str] = &["sh", "bash", "dash", "zsh", "fish", "csh", "tcsh", "ksh"];
const POWERSHELLS: &[&str] = &["powershell", "pwsh", "powershell_ise"];

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Caller {
    /// Executable name matches `AGENT_EXE_NAMES`, e.g. `codex`; infer a direct call.
    Agent,
    /// Shell running a command, e.g. `bash -c 'buck2 ...'`; check its parent.
    CommandShell,
    /// Tool boundary, e.g. `python tool.py` or `bash script.sh`, or unsupported
    /// shell arguments; stop with a non-match.
    Other,
    /// Insufficient information, e.g. missing executable path or shell argv;
    /// stop without a classification.
    Unknown,
}

struct ProcessInfo {
    parent_pid: Option<Pid>,
    creation_time: Duration,
    caller: Caller,
}

pub(crate) fn infer_direct_agent_call() -> Option<bool> {
    let mut system = System::new();
    let refresh = ProcessRefreshKind::nothing()
        .with_exe(UpdateKind::OnlyIfNotSet)
        .with_cmd(UpdateKind::OnlyIfNotSet);
    walk_ancestors(Pid::from_u32(std::process::id()), |pid| {
        system.refresh_processes_specifics(ProcessesToUpdate::Some(&[pid]), true, refresh);
        let process = system.process(pid)?;
        Some(ProcessInfo {
            parent_pid: process.parent(),
            creation_time: process_creation_time(process)?,
            caller: classify(process.exe(), process.cmd()),
        })
    })
}

fn walk_ancestors(
    client_pid: Pid,
    mut read_process: impl FnMut(Pid) -> Option<ProcessInfo>,
) -> Option<bool> {
    let mut child = read_process(client_pid)?;
    let mut seen = BuckMutSet::default();
    seen.insert(client_pid);
    for _ in 0..MAX_PROCESS_DEPTH {
        let Some(parent_pid) = child.parent_pid else {
            return Some(false);
        };
        if !seen.insert(parent_pid) {
            return None;
        }
        let parent = read_process(parent_pid)?;
        // A newer process cannot be our parent. Equal timestamps are allowed:
        // Unix creation times have one-second resolution.
        if parent.creation_time > child.creation_time {
            return None;
        }
        match parent.caller {
            Caller::Agent => return Some(true),
            Caller::CommandShell => child = parent,
            Caller::Other => return Some(false),
            Caller::Unknown => return None,
        }
    }
    None
}

fn classify(exe: Option<&Path>, cmd: &[OsString]) -> Caller {
    let Some(name) = exe.and_then(Path::file_name).and_then(|s| s.to_str()) else {
        return Caller::Unknown;
    };
    let name = name.to_ascii_lowercase();
    let name = name.strip_suffix(".exe").unwrap_or(&name);
    // Executable basenames are a convention, not authentication. Do not match
    // arbitrary arguments: tool commands and prompts can contain agent names.
    if AGENT_EXE_NAMES.contains(&name) {
        return Caller::Agent;
    }
    // A recognized shell with missing argv is Unknown.
    if cmd.is_empty()
        && (UNIX_SHELLS.contains(&name) || POWERSHELLS.contains(&name) || name == "cmd")
    {
        return Caller::Unknown;
    }
    // Recognized command flags with a nonempty command yield CommandShell.
    // Script operands, unsupported options, or no command flag yield Other.
    let mut args = cmd.iter().skip(1).map(|arg| arg.to_str());
    while let Some(arg) = args.next().flatten() {
        let has_command_flag = match name {
            name if UNIX_SHELLS.contains(&name) => {
                if matches!(arg, "--login" | "--noprofile" | "--norc") {
                    continue;
                }
                let Some(flags) = arg.strip_prefix('-').filter(|flags| !flags.is_empty()) else {
                    return Caller::Other;
                };
                if !flags.chars().all(|c| "cleufx".contains(c)) {
                    return Caller::Other;
                }
                flags.contains('c')
            }
            "cmd" => match arg.to_ascii_lowercase().as_str() {
                "/d" | "/s" => continue,
                "/c" => true,
                _ => return Caller::Other,
            },
            // ISE opens script files in a GUI; it has no command-mode switch.
            "powershell_ise" => return Caller::Other,
            name if POWERSHELLS.contains(&name) => {
                let arg = arg.to_ascii_lowercase();
                let flag = arg.strip_prefix("--").or_else(|| arg.strip_prefix('-'));
                match flag {
                    Some("nologo" | "noprofile" | "noninteractive") => continue,
                    Some("executionpolicy") => {
                        if !matches!(args.next().flatten(), Some(value) if !value.is_empty() && !value.starts_with('-'))
                        {
                            return Caller::Other;
                        }
                        continue;
                    }
                    Some("command" | "c") => true,
                    _ => return Caller::Other,
                }
            }
            _ => return Caller::Other,
        };
        if has_command_flag {
            return if matches!(args.next().flatten(), Some(command) if !command.is_empty()) {
                Caller::CommandShell
            } else {
                Caller::Other
            };
        }
    }
    // File/stdin scripts and interactive shells are tool boundaries. A shell
    // script that execs Buck erases itself; ancestry cannot recover that history.
    Caller::Other
}

#[cfg(test)]
mod tests {
    use super::*;

    fn caller(exe: &str, args: &[&str]) -> Caller {
        let cmd = std::iter::once(exe)
            .chain(args.iter().copied())
            .map(OsString::from)
            .collect::<Vec<_>>();
        classify(Some(Path::new(exe)), &cmd)
    }

    #[test]
    fn agents_and_command_shells() {
        for exe in [
            "codex",
            "claude",
            "muse",
            "metacode",
            "pi",
            "codex.exe",
            "claude.exe",
        ] {
            assert_eq!(caller(exe, &[]), Caller::Agent);
        }
        for (exe, args) in [
            (
                "bash",
                vec!["--noprofile", "--norc", "-c", "buck2 build //:x"],
            ),
            ("zsh", vec!["-lc", "buck2 build //:x"]),
            ("sh", vec!["-e", "-c", "buck2 build //:x"]),
            ("fish", vec!["-c", "buck2 build //:x"]),
            ("dash", vec!["-c", "buck2 build //:x"]),
            ("csh", vec!["-fc", "buck2 build //:x"]),
            ("tcsh", vec!["-c", "buck2 build //:x"]),
            ("ksh", vec!["-c", "buck2 build //:x"]),
            ("cmd.exe", vec!["/D", "/S", "/C", "buck2 build //:x"]),
            (
                "powershell.exe",
                vec![
                    "-NoProfile",
                    "-NonInteractive",
                    "-Command",
                    "buck2 build //:x",
                ],
            ),
            ("pwsh", vec!["-c", "buck2 build //:x"]),
            (
                "pwsh",
                vec!["-ExecutionPolicy", "Bypass", "-c", "buck2 build //:x"],
            ),
            (
                "pwsh",
                vec![
                    "--NoProfile",
                    "--ExecutionPolicy",
                    "RemoteSigned",
                    "--Command",
                    "buck2 build //:x",
                ],
            ),
        ] {
            assert_eq!(caller(exe, &args), Caller::CommandShell, "{exe} {args:?}");
        }
    }

    #[test]
    fn scripts_and_tools_are_boundaries() {
        for (exe, args) in [
            ("bash", vec!["script.sh", "-c", "claude"]),
            ("bash", vec!["--", "-c", "codex"]),
            ("bash", vec!["-s"]),
            ("bash", vec!["-c"]),
            ("bash", vec!["-c", ""]),
            ("zsh", vec!["-l"]),
            ("python3", vec!["-c", "codex"]),
            ("node", vec!["tool.js", "claude"]),
            ("powershell.exe", vec!["-File", "script.ps1"]),
            ("pwsh", vec!["-ExecutionPolicy"]),
            ("pwsh", vec!["-ExecutionPolicy", "-Command", "buck2"]),
            ("pwsh", vec!["-ExecutionPolicy", "", "-c", "buck2"]),
            (
                "pwsh",
                vec!["-ExecutionPolicy", "Bypass", "-File", "script.ps1"],
            ),
            ("pwsh", vec!["--NoProfile", "--Command", ""]),
            ("powershell_ise.exe", vec!["-File", "script.ps1"]),
            ("powershell_ise.exe", vec!["-Command", "buck2 build //:x"]),
            ("csh", vec!["script.csh"]),
            ("tcsh", vec!["-s"]),
            ("ksh", vec!["script.ksh"]),
            ("cmd.exe", vec!["/K", "buck2"]),
            ("buck2", vec!["build"]),
            ("not-codex", vec![]),
        ] {
            assert_eq!(caller(exe, &args), Caller::Other, "{exe} {args:?}");
        }
        assert_eq!(classify(None, &[OsString::from("codex")]), Caller::Unknown);
        assert_eq!(classify(Some(Path::new("bash")), &[]), Caller::Unknown);
        assert_eq!(
            classify(Some(Path::new("powershell_ise.exe")), &[]),
            Caller::Unknown
        );
    }

    fn walk(rows: &[(u32, Option<u32>, u64, Caller)]) -> Option<bool> {
        walk_ancestors(Pid::from_u32(1), |pid| {
            let (_, parent_pid, creation_time, caller) =
                rows.iter().find(|row| row.0 == pid.as_u32())?;
            Some(ProcessInfo {
                parent_pid: parent_pid.map(Pid::from_u32),
                creation_time: Duration::from_secs(*creation_time),
                caller: *caller,
            })
        })
    }

    #[test]
    fn direct_and_tool_ancestry() {
        use Caller::*;
        assert_eq!(
            walk(&[(1, Some(2), 10, Other), (2, None, 10, Agent)]),
            Some(true)
        );
        assert_eq!(
            walk(&[
                (1, Some(2), 10, Other),
                (2, Some(3), 9, CommandShell),
                (3, None, 8, Agent)
            ]),
            Some(true)
        );
        assert_eq!(
            walk(&[
                (1, Some(2), 10, Other),
                (2, Some(3), 9, Other),
                (3, None, 8, Agent)
            ]),
            Some(false)
        );
    }

    #[test]
    fn broken_recycled_and_cyclic_ancestry() {
        use Caller::*;
        assert_eq!(walk(&[]), None);
        assert_eq!(walk(&[(1, None, 10, Other)]), Some(false));
        assert_eq!(walk(&[(1, Some(2), 10, Other)]), None);
        assert_eq!(walk(&[(1, Some(2), 10, Other), (2, None, 11, Agent)]), None);
        assert_eq!(
            walk(&[(1, Some(2), 10, Other), (2, None, 9, Unknown)]),
            None
        );
        assert_eq!(
            walk(&[
                (1, Some(2), 10, Other),
                (2, Some(3), 5, CommandShell),
                (3, None, 6, Agent)
            ]),
            None
        );
        assert_eq!(
            walk(&[(1, Some(2), 10, Other), (2, Some(2), 10, CommandShell)]),
            None
        );
        assert_eq!(
            walk(&[(1, Some(2), 10, Other), (2, Some(1), 10, CommandShell)]),
            None
        );
    }
}
