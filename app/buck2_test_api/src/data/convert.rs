/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::time::Duration;
use std::time::SystemTime;

use anyhow::Context as _;
use buck2_core::cells::name::CellName;
use buck2_core::execution_types::executor_config::RemoteExecutorUseCase;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePathBuf;
use buck2_error::BuckErrorContext;
use gazebo::prelude::*;

use super::LocalExecutionCommand;
use super::LocalResourceType;
use super::PrepareForLocalExecutionResult;
use super::RemoteStorageConfig;
use super::RequiredLocalResources;
use super::TtlConfig;
use crate::convert;
use crate::data::ArgHandle;
use crate::data::ArgValue;
use crate::data::ArgValueContent;
use crate::data::ConfiguredTarget;
use crate::data::ConfiguredTargetHandle;
use crate::data::DeclaredOutput;
use crate::data::EnvHandle;
use crate::data::ExecuteRequest2;
use crate::data::ExecutionResult2;
use crate::data::ExecutionStatus;
use crate::data::ExecutionStream;
use crate::data::ExecutorConfigOverride;
use crate::data::ExternalRunnerSpec;
use crate::data::ExternalRunnerSpecValue;
use crate::data::Output;
use crate::data::OutputName;
use crate::data::RemoteDir;
use crate::data::RemoteFile;
use crate::data::RemoteObject;
use crate::data::TestExecutable;
use crate::data::TestResult;
use crate::data::TestStage;
use crate::data::TestStatus;
use crate::protocol::convert::host_sharing_requirements_from_grpc;
use crate::protocol::convert::host_sharing_requirements_to_grpc;

impl TryFrom<buck2_test_proto::TestStage> for TestStage {
    type Error = anyhow::Error;

    fn try_from(s: buck2_test_proto::TestStage) -> Result<Self, Self::Error> {
        use buck2_test_proto::test_stage::*;
        use buck2_test_proto::Testing;

        let res = match s.item.context("Missing `item`")? {
            Item::Listing(Listing { suite, cacheable }) => Self::Listing { suite, cacheable },
            Item::Testing(Testing { suite, testcases }) => Self::Testing { suite, testcases },
        };

        Ok(res)
    }
}

impl TryInto<buck2_test_proto::TestStage> for TestStage {
    type Error = anyhow::Error;

    fn try_into(self) -> Result<buck2_test_proto::TestStage, Self::Error> {
        use buck2_test_proto::test_stage::*;
        use buck2_test_proto::Testing;

        let item = match self {
            Self::Listing { suite, cacheable } => Item::Listing(Listing { suite, cacheable }),
            Self::Testing { suite, testcases } => Item::Testing(Testing { suite, testcases }),
        };

        Ok(buck2_test_proto::TestStage { item: Some(item) })
    }
}

impl TryFrom<buck2_test_proto::ExecutionStream> for ExecutionStream {
    type Error = anyhow::Error;

    fn try_from(s: buck2_test_proto::ExecutionStream) -> Result<Self, Self::Error> {
        use buck2_test_proto::execution_stream::*;

        Ok(match s.item.context("Missing `item`")? {
            Item::Inline(bytes) => Self::Inline(bytes),
        })
    }
}

impl TryInto<buck2_test_proto::ExecutionStream> for ExecutionStream {
    type Error = anyhow::Error;

    fn try_into(self) -> Result<buck2_test_proto::ExecutionStream, Self::Error> {
        use buck2_test_proto::execution_stream::*;

        let item = match self {
            Self::Inline(bytes) => Item::Inline(bytes),
        };

        Ok(buck2_test_proto::ExecutionStream { item: Some(item) })
    }
}

impl TryFrom<buck2_test_proto::ExecutionStatus> for ExecutionStatus {
    type Error = anyhow::Error;

    fn try_from(s: buck2_test_proto::ExecutionStatus) -> Result<Self, Self::Error> {
        use buck2_test_proto::execution_status::*;

        Ok(match s.status.context("Missing `status`")? {
            Status::Finished(exitcode) => Self::Finished { exitcode },
            Status::TimedOut(duration) => Self::TimedOut {
                duration: convert::to_std_duration(duration)?,
            },
        })
    }
}

impl TryInto<buck2_test_proto::ExecutionStatus> for ExecutionStatus {
    type Error = anyhow::Error;

    fn try_into(self) -> Result<buck2_test_proto::ExecutionStatus, Self::Error> {
        use buck2_test_proto::execution_status::*;

        let status = match self {
            Self::Finished { exitcode } => Status::Finished(exitcode),
            Self::TimedOut { duration } => Status::TimedOut(duration.try_into()?),
        };

        Ok(buck2_test_proto::ExecutionStatus {
            status: Some(status),
        })
    }
}

impl TryFrom<buck2_test_proto::ConfiguredTargetHandle> for ConfiguredTargetHandle {
    type Error = anyhow::Error;

    fn try_from(s: buck2_test_proto::ConfiguredTargetHandle) -> Result<Self, Self::Error> {
        let handle = s.id.try_into().context("Invalid `id`")?;
        Ok(Self(handle))
    }
}

impl TryInto<buck2_test_proto::ConfiguredTargetHandle> for ConfiguredTargetHandle {
    type Error = anyhow::Error;

    fn try_into(self) -> Result<buck2_test_proto::ConfiguredTargetHandle, Self::Error> {
        Ok(buck2_test_proto::ConfiguredTargetHandle {
            id: self.0.try_into().context("Invalid `handle`")?,
        })
    }
}

impl TryFrom<buck2_test_proto::ConfiguredTarget> for ConfiguredTarget {
    type Error = anyhow::Error;

    fn try_from(s: buck2_test_proto::ConfiguredTarget) -> Result<Self, Self::Error> {
        let buck2_test_proto::ConfiguredTarget {
            handle,
            cell,
            package,
            target,
            configuration,
            package_project_relative_path,
        } = s;

        Ok(Self {
            handle: handle
                .context("Missing `handle`")?
                .try_into()
                .context("Invalid `handle`")?,
            cell,
            package,
            target,
            configuration,
            package_project_relative_path: ForwardRelativePathBuf::try_from(
                package_project_relative_path,
            )?,
        })
    }
}

impl TryInto<buck2_test_proto::ConfiguredTarget> for ConfiguredTarget {
    type Error = anyhow::Error;

    fn try_into(self) -> Result<buck2_test_proto::ConfiguredTarget, Self::Error> {
        Ok(buck2_test_proto::ConfiguredTarget {
            handle: Some(self.handle.try_into().context("Invalid `handle`")?),
            cell: self.cell,
            package: self.package,
            target: self.target,
            configuration: self.configuration,
            package_project_relative_path: self.package_project_relative_path.as_str().to_owned(),
        })
    }
}

impl TryFrom<i32> for TestStatus {
    type Error = anyhow::Error;

    fn try_from(s: i32) -> Result<Self, Self::Error> {
        let s = buck2_test_proto::TestStatus::try_from(s).context("Invalid `status`")?;

        Ok(match s {
            buck2_test_proto::TestStatus::NotSet => {
                return Err(anyhow::Error::msg("Missing `status`"));
            }
            buck2_test_proto::TestStatus::Pass => TestStatus::PASS,
            buck2_test_proto::TestStatus::Fail => TestStatus::FAIL,
            buck2_test_proto::TestStatus::Skip => TestStatus::SKIP,
            buck2_test_proto::TestStatus::Omitted => TestStatus::OMITTED,
            buck2_test_proto::TestStatus::Fatal => TestStatus::FATAL,
            buck2_test_proto::TestStatus::Timeout => TestStatus::TIMEOUT,
            buck2_test_proto::TestStatus::Unknown => TestStatus::UNKNOWN,
            buck2_test_proto::TestStatus::Rerun => TestStatus::RERUN,
            buck2_test_proto::TestStatus::ListingSuccess => TestStatus::LISTING_SUCCESS,
            buck2_test_proto::TestStatus::ListingFailed => TestStatus::LISTING_FAILED,
        })
    }
}

impl TryInto<i32> for TestStatus {
    type Error = anyhow::Error;

    fn try_into(self) -> Result<i32, Self::Error> {
        Ok(match self {
            TestStatus::PASS => buck2_test_proto::TestStatus::Pass,
            TestStatus::FAIL => buck2_test_proto::TestStatus::Fail,
            TestStatus::SKIP => buck2_test_proto::TestStatus::Skip,
            TestStatus::OMITTED => buck2_test_proto::TestStatus::Omitted,
            TestStatus::FATAL => buck2_test_proto::TestStatus::Fatal,
            TestStatus::TIMEOUT => buck2_test_proto::TestStatus::Timeout,
            TestStatus::UNKNOWN => buck2_test_proto::TestStatus::Unknown,
            TestStatus::RERUN => buck2_test_proto::TestStatus::Rerun,
            TestStatus::LISTING_SUCCESS => buck2_test_proto::TestStatus::ListingSuccess,
            TestStatus::LISTING_FAILED => buck2_test_proto::TestStatus::ListingFailed,
        } as i32)
    }
}

impl TryFrom<buck2_test_proto::TestResult> for TestResult {
    type Error = anyhow::Error;

    fn try_from(s: buck2_test_proto::TestResult) -> Result<Self, Self::Error> {
        let buck2_test_proto::TestResult {
            target,
            name,
            status,
            msg,
            duration,
            details,
            max_memory_used_bytes,
        } = s;

        let duration = duration
            .map(convert::to_std_duration)
            .transpose()
            .context("For `duration`")?;

        Ok(Self {
            target: target
                .context("Missing `target`")?
                .try_into()
                .context("Invalid `target`")?,
            name,
            status: status.try_into().context("Invalid `status`")?,
            msg: msg.map(|m| m.msg),
            duration,
            max_memory_used_bytes,
            details,
        })
    }
}

impl TryInto<buck2_test_proto::TestResult> for TestResult {
    type Error = anyhow::Error;

    fn try_into(self) -> Result<buck2_test_proto::TestResult, Self::Error> {
        use buck2_test_proto::test_result::*;

        Ok(buck2_test_proto::TestResult {
            target: Some(self.target.try_into().context("Invalid `target`")?),
            name: self.name,
            status: self.status.try_into().context("Invalid `status`")?,
            details: self.details,
            msg: self.msg.map(|msg| OptionalMsg { msg }),
            duration: self.duration.try_map(|d| d.try_into())?,
            max_memory_used_bytes: self.max_memory_used_bytes,
        })
    }
}

impl TryFrom<buck2_test_proto::ExternalRunnerSpec> for ExternalRunnerSpec {
    type Error = anyhow::Error;

    fn try_from(s: buck2_test_proto::ExternalRunnerSpec) -> Result<Self, Self::Error> {
        let buck2_test_proto::ExternalRunnerSpec {
            target,
            test_type,
            command,
            env,
            labels,
            contacts,
            oncall,
            working_dir_cell,
        } = s;

        Ok(Self {
            target: target
                .context("Missing `target`")?
                .try_into()
                .context("Invalid `target`")?,
            test_type,
            command: command
                .into_try_map(|x| x.try_into())
                .context("Invalid `command`")?,
            env: env
                .into_iter()
                .map(|(k, v)| Ok((k, v.try_into().unwrap())))
                .collect::<Result<_, Self::Error>>()?,
            labels,
            contacts,
            oncall,
            working_dir_cell: CellName::unchecked_new(&working_dir_cell)?,
        })
    }
}

impl TryInto<buck2_test_proto::ExternalRunnerSpec> for ExternalRunnerSpec {
    type Error = anyhow::Error;

    fn try_into(self) -> Result<buck2_test_proto::ExternalRunnerSpec, Self::Error> {
        let ExternalRunnerSpec {
            target,
            test_type,
            command,
            env,
            labels,
            contacts,
            oncall,
            working_dir_cell,
        } = self;
        Ok(buck2_test_proto::ExternalRunnerSpec {
            target: Some(target.try_into().context("Invalid `target`")?),
            test_type,
            command: command
                .into_try_map(|x| x.try_into())
                .context("Invalid `command`")?,
            env: env
                .into_iter()
                .map(|(k, v)| Ok((k, v.try_into().unwrap())))
                .collect::<Result<_, Self::Error>>()?,
            labels,
            contacts,
            oncall,
            working_dir_cell: working_dir_cell.as_str().to_owned(),
        })
    }
}

impl TryFrom<buck2_test_proto::ExternalRunnerSpecValue> for ExternalRunnerSpecValue {
    type Error = anyhow::Error;

    fn try_from(s: buck2_test_proto::ExternalRunnerSpecValue) -> Result<Self, Self::Error> {
        use buck2_test_proto::external_runner_spec_value::*;
        Ok(match s.value.context("Missing `value`")? {
            Value::Verbatim(val) => ExternalRunnerSpecValue::Verbatim(val),
            Value::ArgHandle(val) => {
                ExternalRunnerSpecValue::ArgHandle(val.try_into().context("Invalid `arg_handle`")?)
            }
            Value::EnvHandle(val) => ExternalRunnerSpecValue::EnvHandle(val.into()),
        })
    }
}

impl TryInto<buck2_test_proto::ExternalRunnerSpecValue> for ExternalRunnerSpecValue {
    type Error = anyhow::Error;

    fn try_into(self) -> Result<buck2_test_proto::ExternalRunnerSpecValue, Self::Error> {
        use buck2_test_proto::external_runner_spec_value::*;

        let value = match self {
            Self::Verbatim(val) => Value::Verbatim(val),
            Self::ArgHandle(ArgHandle(val)) => {
                Value::ArgHandle(val.try_into().context("Invalid `arg_handle`")?)
            }
            Self::EnvHandle(EnvHandle(val)) => Value::EnvHandle(val),
        };

        Ok(buck2_test_proto::ExternalRunnerSpecValue { value: Some(value) })
    }
}

impl From<OutputName> for buck2_test_proto::OutputName {
    fn from(o: OutputName) -> Self {
        Self {
            name: o.name.as_str().to_owned(),
        }
    }
}

impl TryFrom<buck2_test_proto::OutputName> for OutputName {
    type Error = anyhow::Error;

    fn try_from(o: buck2_test_proto::OutputName) -> Result<Self, Self::Error> {
        let name = ForwardRelativePathBuf::try_from(o.name)?;
        Ok(Self { name })
    }
}

impl From<TtlConfig> for buck2_test_proto::TtlConfig {
    fn from(o: TtlConfig) -> Self {
        Self {
            ttl_seconds: o.ttl.as_secs() as i64,
            use_case: o.use_case.to_string(),
        }
    }
}

impl From<buck2_test_proto::TtlConfig> for TtlConfig {
    fn from(o: buck2_test_proto::TtlConfig) -> Self {
        let ttl = Duration::from_secs(o.ttl_seconds as u64);
        let use_case = RemoteExecutorUseCase::new(o.use_case);
        Self { ttl, use_case }
    }
}

impl From<DeclaredOutput> for buck2_test_proto::DeclaredOutput {
    fn from(o: DeclaredOutput) -> Self {
        Self {
            name: o.name.as_str().to_owned(),
            supports_remote: o.remote_storage_config.supports_remote,
            ttl_config: o.remote_storage_config.ttl_config.map(Into::into),
        }
    }
}

impl TryFrom<buck2_test_proto::DeclaredOutput> for DeclaredOutput {
    type Error = anyhow::Error;

    fn try_from(o: buck2_test_proto::DeclaredOutput) -> Result<Self, Self::Error> {
        let name = ForwardRelativePathBuf::try_from(o.name)?.into();
        let remote_storage_config = RemoteStorageConfig {
            supports_remote: o.supports_remote,
            ttl_config: o.ttl_config.map(Into::into),
        };
        Ok(Self {
            name,
            remote_storage_config,
        })
    }
}

impl From<ExecutorConfigOverride> for buck2_test_proto::ExecutorConfigOverride {
    fn from(o: ExecutorConfigOverride) -> Self {
        Self {
            name: o.name.as_str().to_owned(),
        }
    }
}

impl From<buck2_test_proto::ExecutorConfigOverride> for ExecutorConfigOverride {
    fn from(o: buck2_test_proto::ExecutorConfigOverride) -> Self {
        Self { name: o.name }
    }
}

impl From<LocalResourceType> for buck2_test_proto::LocalResourceType {
    fn from(r: LocalResourceType) -> Self {
        Self {
            name: r.name.as_str().to_owned(),
        }
    }
}

impl From<buck2_test_proto::LocalResourceType> for LocalResourceType {
    fn from(o: buck2_test_proto::LocalResourceType) -> Self {
        Self { name: o.name }
    }
}

impl TryInto<buck2_test_proto::ArgValue> for ArgValue {
    type Error = anyhow::Error;

    fn try_into(self) -> Result<buck2_test_proto::ArgValue, Self::Error> {
        Ok(buck2_test_proto::ArgValue {
            content: Some(self.content.try_into().context("Invalid `content`")?),
            format: self
                .format
                .map(|f| buck2_test_proto::ArgFormat { format: f }),
        })
    }
}

impl TryFrom<buck2_test_proto::ArgValue> for ArgValue {
    type Error = anyhow::Error;

    fn try_from(s: buck2_test_proto::ArgValue) -> Result<Self, Self::Error> {
        let content = s
            .content
            .context("Missing `content`")?
            .try_into()
            .context("Invalid `content`")?;
        let format = s.format.map(|f| f.format);

        Ok(Self { content, format })
    }
}

impl TryInto<buck2_test_proto::ArgValueContent> for ArgValueContent {
    type Error = anyhow::Error;

    fn try_into(self) -> Result<buck2_test_proto::ArgValueContent, Self::Error> {
        use buck2_test_proto::arg_value_content::*;

        let value = match self {
            Self::ExternalRunnerSpecValue(value) => Value::SpecValue(
                value
                    .try_into()
                    .context("Invalid external runner spec value")?,
            ),
            Self::DeclaredOutput(value) => Value::DeclaredOutput(value.into()),
        };

        Ok(buck2_test_proto::ArgValueContent { value: Some(value) })
    }
}

impl TryFrom<buck2_test_proto::ArgValueContent> for ArgValueContent {
    type Error = anyhow::Error;

    fn try_from(s: buck2_test_proto::ArgValueContent) -> Result<Self, Self::Error> {
        use buck2_test_proto::arg_value_content::*;

        Ok(match s.value.context("Missing `value`")? {
            Value::SpecValue(value) => Self::ExternalRunnerSpecValue(
                value
                    .try_into()
                    .context("Invalid external runner spec value")?,
            ),
            Value::DeclaredOutput(value) => {
                Self::DeclaredOutput(value.try_into().context("Invalid `value`")?)
            }
        })
    }
}

impl TryFrom<buck2_test_proto::ExecuteRequest2> for ExecuteRequest2 {
    type Error = anyhow::Error;

    fn try_from(s: buck2_test_proto::ExecuteRequest2) -> Result<Self, Self::Error> {
        let buck2_test_proto::ExecuteRequest2 {
            test_executable,
            timeout,
            host_sharing_requirements,
            executor_override,
            required_local_resources,
        } = s;

        let test_executable = test_executable
            .context("Missing `test_executable`")?
            .try_into()
            .context("Invalid `test_executable`")?;

        let timeout = convert::to_std_duration(timeout.context("Missing `timeout`")?)
            .context("Invalid `timeout`")?;

        let host_sharing_requirements =
            host_sharing_requirements.context("Missing `host_sharing_requirements`")?;
        let host_sharing_requirements =
            host_sharing_requirements_from_grpc(host_sharing_requirements)
                .context("Invalid `host_sharing_requirements`")?;

        let executor_override = executor_override.map(|o| o.into());

        let required_local_resources = RequiredLocalResources {
            resources: required_local_resources.into_map(|r| r.into()),
        };

        Ok(ExecuteRequest2 {
            test_executable,
            timeout,
            host_sharing_requirements,
            executor_override,
            required_local_resources,
        })
    }
}

impl TryInto<buck2_test_proto::ExecuteRequest2> for ExecuteRequest2 {
    type Error = anyhow::Error;

    fn try_into(self) -> Result<buck2_test_proto::ExecuteRequest2, Self::Error> {
        let test_executable = Some(
            self.test_executable
                .try_into()
                .context("Invalid `test_executable`")?,
        );

        Ok(buck2_test_proto::ExecuteRequest2 {
            test_executable,
            timeout: Some(self.timeout.try_into()?),
            host_sharing_requirements: Some(
                host_sharing_requirements_to_grpc(self.host_sharing_requirements)
                    .context("Invalid `host_sharing_requirements`")?,
            ),
            executor_override: self.executor_override.map(|o| o.into()),
            required_local_resources: self
                .required_local_resources
                .resources
                .into_map(|r| r.into()),
        })
    }
}

impl TryInto<buck2_test_proto::RemoteObject> for RemoteObject {
    type Error = anyhow::Error;

    fn try_into(self) -> Result<buck2_test_proto::RemoteObject, Self::Error> {
        match self {
            RemoteObject::File(RemoteFile { name, digest }) => {
                let node = buck2_test_proto::RemoteFileNode { name };
                Ok(buck2_test_proto::RemoteObject {
                    digest: Some(digest),
                    node: Some(buck2_test_proto::remote_object::Node::File(node)),
                })
            }
            RemoteObject::Dir(RemoteDir {
                name,
                digest,
                children,
            }) => {
                let children = children
                    .into_iter()
                    .map(|child| child.try_into())
                    .collect::<Result<Vec<_>, _>>()?;
                let node = buck2_test_proto::RemoteDirNode { name, children };
                Ok(buck2_test_proto::RemoteObject {
                    digest: Some(digest),
                    node: Some(buck2_test_proto::remote_object::Node::Dir(node)),
                })
            }
        }
    }
}

impl TryFrom<buck2_test_proto::RemoteObject> for RemoteObject {
    type Error = anyhow::Error;

    fn try_from(value: buck2_test_proto::RemoteObject) -> Result<Self, Self::Error> {
        let digest = value.digest.context("missing digest")?;
        match value.node.context("missing node")? {
            buck2_test_proto::remote_object::Node::File(file) => {
                Ok(RemoteObject::file(file.name, digest))
            }
            buck2_test_proto::remote_object::Node::Dir(dir) => {
                let children = dir
                    .children
                    .into_iter()
                    .map(|child| child.try_into())
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(RemoteObject::dir(dir.name, digest, children))
            }
        }
    }
}

impl TryInto<buck2_test_proto::Output> for Output {
    type Error = anyhow::Error;

    fn try_into(self) -> Result<buck2_test_proto::Output, Self::Error> {
        use buck2_test_proto::output::*;

        let value = match self {
            Self::LocalPath(value) => Value::LocalPath(
                value
                    .to_str()
                    .buck_error_context_anyhow("Invalid local path")?
                    .to_owned(),
            ),
            Self::RemoteObject(value) => Value::RemoteObject(value.try_into()?),
        };

        Ok(buck2_test_proto::Output { value: Some(value) })
    }
}

impl TryFrom<buck2_test_proto::Output> for Output {
    type Error = anyhow::Error;

    fn try_from(s: buck2_test_proto::Output) -> Result<Self, Self::Error> {
        use buck2_test_proto::output::*;

        Ok(match s.value.context("Missing `value`")? {
            Value::LocalPath(value) => Self::LocalPath(
                value
                    .try_into()
                    .buck_error_context_anyhow("Invalid local path value.")?,
            ),
            Value::RemoteObject(value) => Self::RemoteObject(value.try_into()?),
        })
    }
}

impl TryInto<buck2_test_proto::ExecutionResult2> for ExecutionResult2 {
    type Error = anyhow::Error;

    fn try_into(self) -> Result<buck2_test_proto::ExecutionResult2, Self::Error> {
        Ok(buck2_test_proto::ExecutionResult2 {
            status: Some(self.status.try_into().context("Invalid `status`")?),
            stdout: Some(self.stdout.try_into().context("Invalid `stdout`")?),
            stderr: Some(self.stderr.try_into().context("Invalid `stderr`")?),
            outputs: self
                .outputs
                .into_iter()
                .map(|(k, v)| {
                    Ok(buck2_test_proto::OutputEntry {
                        declared_output: Some(k.into()),
                        output: Some(v.try_into().context("Invalid `output`")?),
                    })
                })
                .collect::<Result<_, Self::Error>>()?,
            start_time: Some(
                self.start_time
                    .duration_since(SystemTime::UNIX_EPOCH)
                    .unwrap_or_default()
                    .try_into()?,
            ),
            execution_time: Some(self.execution_time.try_into()?),
            execution_details: Some(self.execution_details),
            max_memory_used_bytes: self.max_memory_used_bytes,
        })
    }
}

impl TryFrom<buck2_test_proto::ExecutionResult2> for ExecutionResult2 {
    type Error = anyhow::Error;

    fn try_from(s: buck2_test_proto::ExecutionResult2) -> Result<Self, Self::Error> {
        let buck2_test_proto::ExecutionResult2 {
            status,
            stdout,
            stderr,
            outputs,
            start_time,
            execution_time,
            execution_details,
            max_memory_used_bytes,
        } = s;
        let status = status
            .context("Missing `status`")?
            .try_into()
            .context("Invalid `status`")?;
        let stdout = stdout
            .context("Missing `stdout`")?
            .try_into()
            .context("Invalid `stdout`")?;
        let stderr = stderr
            .context("Missing `stderr`")?
            .try_into()
            .context("Invalid `stderr`")?;

        let outputs = outputs
            .into_iter()
            .map(|entry| {
                let buck2_test_proto::OutputEntry {
                    declared_output,
                    output,
                } = entry;
                let declared_output = declared_output
                    .context("Missing `declared_output`")?
                    .try_into()
                    .context("Invalid `declared_output`")?;
                let output = output
                    .context("Missing `output`")?
                    .try_into()
                    .context("Invalid `output`")?;
                Ok((declared_output, output))
            })
            .collect::<Result<_, Self::Error>>()?;

        let start_time = SystemTime::UNIX_EPOCH
            + convert::to_std_duration(start_time.context("Missing `start_time`")?)
                .context("Invalid `start_time`")?;

        let execution_time =
            convert::to_std_duration(execution_time.context("Missing `execution_time`")?)
                .context("Invalid `execution_time`")?;

        let execution_details = execution_details.context("Missing `execution_details`")?;

        Ok(ExecutionResult2 {
            status,
            stdout,
            stderr,
            outputs,
            start_time,
            execution_time,
            execution_details,
            max_memory_used_bytes,
        })
    }
}

impl TryFrom<buck2_test_proto::TestExecutable> for TestExecutable {
    type Error = anyhow::Error;

    fn try_from(s: buck2_test_proto::TestExecutable) -> Result<Self, Self::Error> {
        let buck2_test_proto::TestExecutable {
            stage,
            target,
            cmd,
            pre_create_dirs,
            env,
        } = s;
        let ui_prints = stage
            .context("Missing `ui_prints`")?
            .try_into()
            .context("Invalid `ui_prints`")?;

        let target = target
            .context("Missing `target`")?
            .try_into()
            .context("Invalid `target`")?;

        let cmd = cmd
            .into_try_map(|c| c.try_into())
            .context("Invalid `cmd`")?;

        let env = env
            .into_iter()
            .map(|env_var| {
                let buck2_test_proto::EnvironmentVariable { key, value } = env_var;
                value
                    .context("Missing `value`")?
                    .try_into()
                    .context("Invalid `env`")
                    .map(|v: ArgValue| (key, v))
            })
            .collect::<anyhow::Result<_>>()?;

        let pre_create_dirs = pre_create_dirs
            .into_try_map(|c| c.try_into())
            .context("Invalid `pre_create_dirs`")?;

        Ok(TestExecutable {
            stage: ui_prints,
            target,
            cmd,
            env,
            pre_create_dirs,
        })
    }
}

impl TryInto<buck2_test_proto::TestExecutable> for TestExecutable {
    type Error = anyhow::Error;

    fn try_into(self) -> Result<buck2_test_proto::TestExecutable, Self::Error> {
        let stage = Some(self.stage.try_into().context("Invalid `ui_prints`")?);
        let target = Some(self.target.try_into().context("Invalid `target`")?);
        let cmd = self
            .cmd
            .into_try_map(|i| i.try_into())
            .context("Invalid `cmd`")?;

        let env = self
            .env
            .into_iter()
            .map(|(k, v)| {
                v.try_into()
                    .context("Invalid `env`")
                    .map(
                        |v: buck2_test_proto::ArgValue| buck2_test_proto::EnvironmentVariable {
                            key: k,
                            value: Some(v),
                        },
                    )
            })
            .collect::<anyhow::Result<_>>()?;

        let pre_create_dirs = self.pre_create_dirs.into_map(|i| i.into());

        Ok(buck2_test_proto::TestExecutable {
            stage,
            target,
            cmd,
            pre_create_dirs,
            env,
        })
    }
}

impl TryInto<buck2_test_proto::PrepareForLocalExecutionResponse>
    for PrepareForLocalExecutionResult
{
    type Error = anyhow::Error;

    fn try_into(self) -> Result<buck2_test_proto::PrepareForLocalExecutionResponse, Self::Error> {
        let cwd = self
            .command
            .cwd
            .to_str()
            .buck_error_context_anyhow("Invalid cwd path")?
            .to_owned();

        Ok(buck2_test_proto::PrepareForLocalExecutionResponse {
            result: Some(buck2_test_proto::PrepareForLocalExecutionResult {
                cmd: self.command.cmd,
                cwd,
                env: self
                    .command
                    .env
                    .into_iter()
                    .map(
                        |(key, value)| buck2_test_proto::VerbatimEnvironmentVariable { key, value },
                    )
                    .collect(),
            }),
            setup_local_resource_commands: self
                .local_resource_setup_commands
                .into_iter()
                .map(|c| {
                    <LocalExecutionCommand as TryInto<
                        buck2_test_proto::SetupLocalResourceLocalExecutionCommand,
                    >>::try_into(c)
                })
                .collect::<Result<Vec<_>, anyhow::Error>>()?,
        })
    }
}

impl TryInto<buck2_test_proto::SetupLocalResourceLocalExecutionCommand> for LocalExecutionCommand {
    type Error = anyhow::Error;

    fn try_into(
        self,
    ) -> Result<buck2_test_proto::SetupLocalResourceLocalExecutionCommand, Self::Error> {
        Ok(buck2_test_proto::SetupLocalResourceLocalExecutionCommand {
            cmd: self.cmd,
            cwd: self
                .cwd
                .to_str()
                .buck_error_context_anyhow("Invalid cwd path for local resource")?
                .to_owned(),
            env: self
                .env
                .into_iter()
                .map(|(k, v)| buck2_test_proto::VerbatimEnvironmentVariable { key: k, value: v })
                .collect(),
        })
    }
}

impl TryFrom<buck2_test_proto::PrepareForLocalExecutionResult> for LocalExecutionCommand {
    type Error = anyhow::Error;

    fn try_from(s: buck2_test_proto::PrepareForLocalExecutionResult) -> Result<Self, Self::Error> {
        Ok(Self {
            cmd: s.cmd,
            cwd: s
                .cwd
                .try_into()
                .buck_error_context_anyhow("Invalid cwd value.")?,
            env: s
                .env
                .into_iter()
                .map(|env_var| (env_var.key, env_var.value))
                .collect(),
        })
    }
}

impl TryFrom<buck2_test_proto::SetupLocalResourceLocalExecutionCommand> for LocalExecutionCommand {
    type Error = anyhow::Error;

    fn try_from(
        s: buck2_test_proto::SetupLocalResourceLocalExecutionCommand,
    ) -> Result<Self, Self::Error> {
        Ok(Self {
            cmd: s.cmd,
            cwd: s
                .cwd
                .try_into()
                .buck_error_context_anyhow("Invalid cwd value.")?,
            env: s
                .env
                .into_iter()
                .map(|env_var| (env_var.key, env_var.value))
                .collect(),
        })
    }
}

impl TryFrom<buck2_test_proto::PrepareForLocalExecutionResponse>
    for PrepareForLocalExecutionResult
{
    type Error = anyhow::Error;

    fn try_from(
        s: buck2_test_proto::PrepareForLocalExecutionResponse,
    ) -> Result<Self, Self::Error> {
        let result = s.result.context("Missing `result`")?;
        Ok(Self {
            command: LocalExecutionCommand::try_from(result)?,
            local_resource_setup_commands: s
                .setup_local_resource_commands
                .into_iter()
                .map(LocalExecutionCommand::try_from)
                .collect::<Result<Vec<_>, anyhow::Error>>()?,
        })
    }
}

#[cfg(test)]
mod tests {
    use std::fmt::Debug;

    use host_sharing::HostSharingRequirements;
    use sorted_vector_map::sorted_vector_map;

    use super::*;

    fn assert_roundtrips<P, S>(s: &S)
    where
        S: Clone + PartialEq + Debug,
        P: TryInto<S, Error = anyhow::Error>,
        S: TryInto<P, Error = anyhow::Error>,
    {
        let proto: P = s.clone().try_into().unwrap();
        let roundtrip: S = proto.try_into().unwrap();
        assert_eq!(*s, roundtrip);
    }

    #[test]
    fn external_runner_spec_roundtrip() {
        let test_spec = ExternalRunnerSpec {
            target: ConfiguredTarget {
                handle: ConfiguredTargetHandle(1),
                cell: "qux".into(),
                package: "foo".into(),
                target: "bar".into(),
                configuration: "xxx".into(),
                package_project_relative_path: ForwardRelativePathBuf::unchecked_new(
                    "qux/foo".to_owned(),
                ),
            },
            test_type: "some_type".to_owned(),
            command: vec![
                ExternalRunnerSpecValue::Verbatim("arg".to_owned()),
                ExternalRunnerSpecValue::ArgHandle(ArgHandle(42)),
            ],
            env: [
                (
                    "FOO".to_owned(),
                    ExternalRunnerSpecValue::EnvHandle(EnvHandle("FOO".to_owned())),
                ),
                (
                    "BAR".to_owned(),
                    ExternalRunnerSpecValue::Verbatim("BAR".to_owned()),
                ),
            ]
            .into_iter()
            .collect(),
            labels: vec!["label1".to_owned(), "label2".to_owned()],
            contacts: vec!["contact1".to_owned(), "contact2".to_owned()],
            oncall: Some("contact1".to_owned()),
            working_dir_cell: CellName::testing_new("qux"),
        };
        assert_roundtrips::<buck2_test_proto::ExternalRunnerSpec, ExternalRunnerSpec>(&test_spec);
    }

    #[test]
    fn execute_request2_roundtrip() {
        let declared_output = DeclaredOutput {
            name: OutputName::unchecked_new("name".to_owned()),
            remote_storage_config: RemoteStorageConfig::new(true),
        };

        let test_executable = TestExecutable {
            stage: TestStage::Listing {
                suite: "name".to_owned(),
                cacheable: true,
            },
            target: ConfiguredTargetHandle(42),
            cmd: vec![
                ArgValue {
                    content: ArgValueContent::ExternalRunnerSpecValue(
                        ExternalRunnerSpecValue::Verbatim("arg".to_owned()),
                    ),
                    format: None,
                },
                ArgValue {
                    content: ArgValueContent::DeclaredOutput(declared_output.name.clone()),
                    format: Some("--output={}".to_owned()),
                },
            ],
            env: [(
                "FOO".to_owned(),
                ArgValue {
                    content: ArgValueContent::ExternalRunnerSpecValue(
                        ExternalRunnerSpecValue::EnvHandle(EnvHandle("FOO".to_owned())),
                    ),
                    format: None,
                },
            )]
            .into_iter()
            .collect(),
            pre_create_dirs: vec![declared_output],
        };
        let request = ExecuteRequest2 {
            test_executable,
            timeout: Duration::from_millis(42),
            host_sharing_requirements: HostSharingRequirements::ExclusiveAccess,
            executor_override: Some(ExecutorConfigOverride {
                name: "foo".to_owned(),
            }),
            required_local_resources: RequiredLocalResources { resources: vec![] },
        };
        assert_roundtrips::<buck2_test_proto::ExecuteRequest2, ExecuteRequest2>(&request);
    }

    #[test]
    fn execution_result2_roundtrips() {
        let local_path = if cfg!(not(windows)) {
            "/some/path"
        } else {
            "c:/some/path"
        };

        let result = ExecutionResult2 {
            status: ExecutionStatus::Finished { exitcode: 42 },
            stdout: ExecutionStream::Inline(vec![97, 115, 109]),
            stderr: ExecutionStream::Inline(vec![118, 105, 109]),
            outputs: [(
                OutputName::unchecked_new("name".to_owned()),
                Output::LocalPath(String::from(local_path).try_into().expect("valid abs path")),
            )]
            .into_iter()
            .collect(),
            start_time: SystemTime::UNIX_EPOCH + Duration::from_secs(123),
            execution_time: Duration::from_secs(456),
            execution_details: Default::default(),
            max_memory_used_bytes: None,
        };
        assert_roundtrips::<buck2_test_proto::ExecutionResult2, ExecutionResult2>(&result);
    }

    fn dummy_local_execution_command() -> LocalExecutionCommand {
        let cmd = vec![
            "my_cmd".to_owned(),
            "--some-arg".to_owned(),
            "some_value".to_owned(),
        ];
        let local_path = if cfg!(not(windows)) {
            "/some/path"
        } else {
            "c:/some/path"
        };
        let cwd = String::from(local_path).try_into().expect("valid abs path");
        let env = sorted_vector_map! { "some_env".to_owned() => "some_env_val".to_owned() };

        LocalExecutionCommand { cmd, env, cwd }
    }

    #[test]
    fn prepare_for_local_execution_result_roundtrip() {
        let result = PrepareForLocalExecutionResult {
            command: dummy_local_execution_command(),
            local_resource_setup_commands: vec![dummy_local_execution_command()],
        };

        assert_roundtrips::<
            buck2_test_proto::PrepareForLocalExecutionResponse,
            PrepareForLocalExecutionResult,
        >(&result);
    }

    #[test]
    fn test_executable_roundtrip() {
        let declared_output = DeclaredOutput {
            name: OutputName::unchecked_new("name".to_owned()),
            remote_storage_config: RemoteStorageConfig::new(false),
        };

        let test_executable = TestExecutable {
            stage: TestStage::Listing {
                suite: "name".to_owned(),
                cacheable: true,
            },
            target: ConfiguredTargetHandle(42),
            cmd: vec![
                ArgValue {
                    content: ArgValueContent::ExternalRunnerSpecValue(
                        ExternalRunnerSpecValue::Verbatim("arg".to_owned()),
                    ),
                    format: None,
                },
                ArgValue {
                    content: ArgValueContent::DeclaredOutput(declared_output.name.clone()),
                    format: Some("--output={}".to_owned()),
                },
            ],
            env: [(
                "FOO".to_owned(),
                ArgValue {
                    content: ArgValueContent::ExternalRunnerSpecValue(
                        ExternalRunnerSpecValue::EnvHandle(EnvHandle("FOO".to_owned())),
                    ),
                    format: None,
                },
            )]
            .into_iter()
            .collect(),
            pre_create_dirs: vec![declared_output],
        };

        assert_roundtrips::<buck2_test_proto::TestExecutable, TestExecutable>(&test_executable);
    }
}
