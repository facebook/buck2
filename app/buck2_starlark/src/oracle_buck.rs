/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use starlark::environment::Globals;
use starlark::environment::LibraryExtension;
use starlark::typing::*;

pub(crate) struct OracleBuck;

impl OracleBuck {
    #[allow(clippy::new_ret_no_self)]
    pub(crate) fn new(globals: Globals) -> Arc<dyn TypingOracle + Send + Sync> {
        let _unused = globals;
        Arc::new(vec![
            Box::new(OracleStandard::new(LibraryExtension::all()))
                as Box<dyn TypingOracle + Send + Sync>,
            Box::new(OracleBuck),
        ])
    }
}

impl TypingOracle for OracleBuck {
    fn attribute(&self, ty: &Ty, attr: &str) -> Option<Result<Ty, ()>> {
        fn output() -> Ty {
            Ty::unions(vec![
                Ty::string(),
                Ty::name("artifact"),
                Ty::name("output_artifact"),
            ])
        }
        let err = Some(Err(()));

        Some(Ok(match ty {
            Ty::Name(name) => match name.as_str() {
                "artifact" => match attr {
                    "basename" => Ty::string(),
                    "extension" => Ty::string(),
                    "is_source" => Ty::bool(),
                    "owner" => Ty::name("label"),
                    "short_path" => Ty::string(),
                    "as_output" => Ty::function(vec![], Ty::name("output_artifact")),
                    "project" => Ty::function(
                        vec![
                            Param::pos_only(Ty::string()),
                            Param::name_only("hide_prefix", Ty::bool()).optional(),
                        ],
                        Ty::name("artifact"),
                    ),
                    _ => return err,
                },
                "dependency" => match attr {
                    "__index__" | "get" => Ty::function(vec![Param::pos_only(Ty::Any)], Ty::Any),
                    "__in__" => Ty::function(vec![Param::pos_only(Ty::Any)], Ty::bool()),
                    "label" => Ty::name("label"),
                    "providers" => Ty::list(Ty::Any),
                    "sub_target" => {
                        Ty::function(vec![Param::pos_only(Ty::string())], Ty::name("dependency"))
                    }
                    _ => return err,
                },
                "context" => match attr {
                    "attrs" => Ty::name("struct"),
                    "actions" => Ty::name("actions"),
                    "label" => Ty::name("label"),
                    "artifacts" => Ty::Any,
                    "outputs" => Ty::Any,
                    _ => return err,
                },
                "actions" => match attr {
                    "declare_output" => Ty::function(
                        vec![
                            Param::pos_only(Ty::string()),
                            Param::pos_only(Ty::string()).optional(),
                            Param::name_only("dir", Ty::bool()).optional(),
                        ],
                        Ty::name("artifact"),
                    ),
                    "write_json" => Ty::function(
                        vec![
                            Param::pos_only(output()),
                            Param::pos_only(Ty::Any),
                            Param::name_only("with_inputs", Ty::bool()).optional(),
                        ],
                        Ty::name("artifact"),
                    ),
                    "write" => Ty::function(
                        vec![
                            Param::pos_only(output()),
                            Param::pos_only(Ty::Any),
                            Param::name_only("is_executable", Ty::bool()).optional(),
                            Param::name_only("allow_args", Ty::bool()).optional(),
                        ],
                        Ty::union2(
                            Ty::name("artifact"),
                            Ty::tuple2(Ty::name("artifact"), Ty::list(Ty::name("output_artifact"))),
                        ),
                    ),
                    "copy" | "symlink" => Ty::function(
                        vec![
                            Param::pos_only(Ty::name("artifact")),
                            Param::pos_only(output()),
                        ],
                        Ty::name("artifact"),
                    ),
                    "copy_file" | "symlink_file" => Ty::function(
                        vec![
                            Param::pos_only(output()),
                            Param::pos_only(Ty::name("artifact")),
                        ],
                        Ty::name("artifact"),
                    ),
                    "symlinked_dir" | "copied_dir" => Ty::function(
                        vec![
                            Param::pos_only(output()),
                            Param::pos_only(Ty::dict(Ty::string(), Ty::name("artifact"))),
                        ],
                        Ty::name("artifact"),
                    ),
                    "run" => Ty::function(
                        vec![
                            Param::pos_only(Ty::Any),
                            Param::name_only("category", Ty::string()),
                            Param::name_only("identifier", Ty::string()).optional(),
                            Param::name_only("env", Ty::dict(Ty::string(), Ty::Any)).optional(),
                            Param::name_only("local_only", Ty::bool()).optional(),
                            Param::name_only("prefer_local", Ty::bool()).optional(),
                            Param::name_only("prefer_remote", Ty::bool()).optional(),
                            Param::name_only("always_print_stderr", Ty::bool()).optional(),
                            Param::name_only("weight", Ty::int()).optional(),
                            Param::name_only("weight_percentage", Ty::int()).optional(),
                            Param::name_only("dep_files", Ty::dict(Ty::string(), Ty::Any))
                                .optional(),
                            Param::name_only("metadata_env_var", Ty::string()).optional(),
                            Param::name_only("metadata_path", Ty::string()).optional(),
                            Param::name_only("no_outputs_cleanup", Ty::bool()).optional(),
                            Param::name_only("allow_cache_upload", Ty::bool()).optional(),
                            Param::name_only("force_full_hybrid_if_capable", Ty::bool()).optional(),
                            Param::name_only(
                                "exe",
                                Ty::union2(Ty::name("WorkerRunInfo"), Ty::name("RunInfo")),
                            )
                            .optional(),
                        ],
                        Ty::None,
                    ),
                    "download_file" => Ty::function(
                        vec![
                            Param::pos_only(output()),
                            Param::pos_only(Ty::string()),
                            Param::name_only("vpnless_url", Ty::string()).optional(),
                            Param::name_only("sha1", Ty::string()).optional(),
                            Param::name_only("sha256", Ty::string()).optional(),
                            Param::name_only("is_executable", Ty::bool()).optional(),
                            Param::name_only("is_deferrable", Ty::bool()).optional(),
                        ],
                        Ty::name("artifact"),
                    ),
                    "tset" => Ty::Any,
                    "dynamic_output" => Ty::Any,
                    "artifact_tag" => Ty::function(vec![], Ty::name("artifact_tag")),
                    "anon_targets" => Ty::function(
                        vec![Param::pos_only(Ty::list(Ty::tuple2(
                            Ty::Any,
                            Ty::dict(Ty::string(), Ty::Any),
                        )))],
                        Ty::name("promise"),
                    ),
                    _ => return err,
                },
                "label" => match attr {
                    "package" => Ty::string(),
                    "name" => Ty::string(),
                    "path" => Ty::name("label_relative_path"),
                    "cell" => Ty::string(),
                    "cell_root" => Ty::name("cell_root"),
                    "raw_target" => Ty::function(vec![], Ty::name("target_label")),
                    "sub_target" => Ty::list(Ty::string()),
                    _ => return err,
                },
                "label_relative_path" => match attr {
                    "add" => Ty::function(
                        vec![Param::pos_only(Ty::string())],
                        Ty::name("label_relative_path"),
                    ),
                    _ => return err,
                },
                "cmd_args" => match attr {
                    "add" => Ty::function(vec![Param::args(Ty::Any)], Ty::name("cmd_args")),
                    "hidden" => Ty::function(vec![Param::args(Ty::Any)], Ty::name("cmd_args")),
                    "ignore_artifacts" => Ty::function(vec![], Ty::name("cmd_args")),
                    "replace_regex" => Ty::function(
                        vec![
                            Param::pos_or_name("pattern", Ty::string()),
                            Param::pos_or_name("replacement", Ty::string()),
                        ],
                        Ty::name("cmd_args"),
                    ),

                    "relative_to" => Ty::function(
                        vec![
                            Param::pos_or_name("directory", Ty::Any),
                            Param::pos_or_name("parent", Ty::int()).optional(),
                        ],
                        Ty::name("cmd_args"),
                    ),
                    "absolute_prefix" => Ty::function(
                        vec![Param::pos_or_name("prefix", Ty::string())],
                        Ty::name("cmd_args"),
                    ),
                    "absolute_suffix" => Ty::function(
                        vec![Param::pos_or_name("suffix", Ty::string())],
                        Ty::name("cmd_args"),
                    ),
                    "parent" => Ty::function(
                        vec![Param::pos_only(Ty::int()).optional()],
                        Ty::name("cmd_args"),
                    ),
                    "copy" => Ty::function(vec![], Ty::name("cmd_args")),
                    "inputs" => Ty::function(vec![], Ty::name("command_line_inputs")),
                    "outputs" => Ty::function(vec![], Ty::list(Ty::name("output_artifact"))),
                    _ => return err,
                },
                _ => return None,
            },
            _ => return None,
        }))
    }

    fn builtin(&self, name: &str) -> Option<Result<Ty, ()>> {
        Some(Ok(match name {
            "attrs" => Ty::todo(),
            "__internal__" => Ty::todo(),
            "cmd_args" => Ty::ctor_function(
                "cmd_args",
                vec![
                    Param::args(Ty::Any),
                    Param::name_only("delimiter", Ty::string()).optional(),
                    Param::name_only("format", Ty::string()).optional(),
                    Param::name_only("prepend", Ty::string()).optional(),
                    Param::name_only("quote", Ty::string()).optional(),
                ],
                Ty::name("cmd_args"),
            ),
            "CommandExecutorConfig" => Ty::todo(),
            "ConfigurationInfo" => Ty::todo(),
            "ConstraintSettingInfo" => Ty::todo(),
            "ConstraintValueInfo" => Ty::todo(),
            "DefaultInfo" => Ty::todo(),
            "InstallInfo" => Ty::todo(),
            "ExternalRunnerTestInfo" => Ty::todo(),
            "LocalResourceInfo" => Ty::todo(),
            "WorkerRunInfo" => Ty::todo(),
            "WorkerInfo" => Ty::todo(),
            "host_info" => Ty::todo(),
            "load_symbols" => Ty::todo(),
            "PlatformInfo" => Ty::todo(),
            "provider" => Ty::todo(),
            "read_config" => Ty::todo(),
            "regex_match" => Ty::todo(),
            "rule" => Ty::todo(),
            "dedupe" => Ty::function(vec![Param::pos_only(Ty::iter(Ty::Any))], Ty::list(Ty::Any)),
            "soft_error" => Ty::todo(),
            "warning" => Ty::todo(),
            "RunInfo" => Ty::todo(),
            "select" => Ty::todo(),
            "glob" => Ty::todo(),
            "package_name" => Ty::todo(),
            "rule_exists" => Ty::todo(),
            "TemplatePlaceholderInfo" => Ty::todo(),
            "transition" => Ty::todo(),
            "transitive_set" => Ty::todo(),
            _ => return None,
        }))
    }

    fn builtin_call(&self, _name: &str, _args: &[Arg]) -> Option<Result<Ty, String>> {
        None
    }

    fn subtype(&self, require: &TyName, got: &TyName) -> bool {
        match require.as_str() {
            "provider" => got.as_str().ends_with("Info"),
            _ => false,
        }
    }
}
