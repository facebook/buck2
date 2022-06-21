/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![feature(min_specialization)]
#![allow(clippy::large_enum_variant)]

use std::convert::TryFrom;

use thiserror::Error;

use crate::BuckDaemonProtoError::MissingClientContext;

tonic::include_proto!("buck.daemon");

#[derive(Debug, Error)]
enum BuckDaemonProtoError {
    #[error("daemon request was missing client context")]
    MissingClientContext,
}

pub trait HasClientContext {
    fn client_context(&self) -> anyhow::Result<&ClientContext>;
}

impl HasClientContext for StreamingRequest {
    fn client_context(&self) -> anyhow::Result<&ClientContext> {
        match self.request.as_ref() {
            Some(streaming_request::Request::Context(ctx)) => Ok(ctx),
            _ => Err(MissingClientContext.into()),
        }
    }
}

impl HasBuildOptions for StreamingRequest {
    fn build_options(&self) -> Option<&CommonBuildOptions> {
        None
    }
}

impl TryFrom<StreamingRequest> for LspRequest {
    type Error = tonic::Status;

    fn try_from(value: StreamingRequest) -> Result<Self, Self::Error> {
        match value.request {
            Some(streaming_request::Request::Lsp(req)) => Ok(req),
            _ => Err(tonic::Status::invalid_argument(
                "messages sent by client must be of type `LspRequest`",
            )),
        }
    }
}

impl From<LspRequest> for StreamingRequest {
    fn from(request: LspRequest) -> Self {
        Self {
            request: Some(streaming_request::Request::Lsp(request)),
        }
    }
}

/// Trait for requests that have CommonBuildOptions.
pub trait HasBuildOptions {
    fn build_options(&self) -> Option<&CommonBuildOptions>;
}

pub trait HasRecordTargetCallStacks {
    /// If true, rule functions record call stacks.
    fn record_target_call_stacks(&self) -> bool;
}

impl<T> HasRecordTargetCallStacks for T {
    default fn record_target_call_stacks(&self) -> bool {
        false
    }
}

impl HasRecordTargetCallStacks for CqueryRequest {
    fn record_target_call_stacks(&self) -> bool {
        self.target_call_stacks
    }
}

impl HasRecordTargetCallStacks for UqueryRequest {
    fn record_target_call_stacks(&self) -> bool {
        self.target_call_stacks
    }
}

impl HasRecordTargetCallStacks for TargetsRequest {
    fn record_target_call_stacks(&self) -> bool {
        self.target_call_stacks
    }
}

macro_rules! result_convert {
    ( $name:ident ) => {
        impl From<$name> for command_result::Result {
            fn from(v: $name) -> Self {
                Self::$name(v)
            }
        }

        impl TryFrom<command_result::Result> for $name {
            type Error = command_result::Result;
            fn try_from(r: command_result::Result) -> Result<Self, Self::Error> {
                if let command_result::Result::$name(res) = r {
                    Ok(res)
                } else {
                    Err(r)
                }
            }
        }
    };
}

/// Helper macro for defining a request and implementing common traits for said request. See the bottom of this file
/// for usage examples.
macro_rules! define_request {
    // define_request! expects an invocation of the form `define_request!(MyRequest, has(foo, bar, baz))`. The bulk of
    // this macro comes from parsing the contents of the `has` token tree. This is done by parsing the comma-delimited
    // list of identifiers tail-recursively while passing down parsing state as parameters to rules prefixed by `@has`.
    //
    // There are three state parameters in each of the non-public rules:
    //  1) @has, which marks the rule as non-public. The public rules pass @has as the first tokens when recursively
    //     invoking this rule.
    //  2) $name:ident, the name of the request.
    //  3) $has_buildopts:ident, a `true` or `false` token indicating whether or not we've emitted an impl of
    //     HasBuildOptions yet.
    //
    // If we reach the end of the token stream without emitting a HasBuildOptions impl, we'll emit a trivial one that
    // returns None.

    ( @has $name:ident $has_buildopts:ident context $($tail:ident)* ) => {
        impl HasClientContext for $name {
            fn client_context(&self) -> anyhow::Result<&ClientContext> {
                // A request that has a client context field should always set the context.
                match &self.context {
                    Some(v) => Ok(v),
                    None => Err(BuckDaemonProtoError::MissingClientContext.into()),
                }
            }
        }

        define_request!(@has $name $has_buildopts $($tail)*);
    };

    ( @has $name:ident $has_buildopts:ident build_options $($tail:ident)* ) => {
        impl HasBuildOptions for $name {
            fn build_options(&self) -> Option<&CommonBuildOptions> {
                self.build_opts.as_ref()
            }
        }

        define_request!(@has $name true $($tail)*);
    };

    ( @has $name:ident false) => {
        impl HasBuildOptions for $name {
            fn build_options(&self) -> Option<&CommonBuildOptions> {
                None
            }
        }
    };

    ( @has $name:ident true) => {};

    // ------ Public API begins here.

    ( $name:ident, has ($($tail:ident),*)) => {
        define_request!(@has $name false $($tail)*);
    };

    ( $name:ident ) => {};
}

mod serialize_timestamp {
    use serde::Deserialize;
    use serde::Deserializer;
    use serde::Serialize;
    use serde::Serializer;

    pub fn serialize<S>(
        value: &Option<::prost_types::Timestamp>,
        serializer: S,
    ) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let d = value.as_ref().map(|v| (v.seconds, v.nanos));
        d.serialize(serializer)
    }

    pub fn deserialize<'de, D>(
        deserializer: D,
    ) -> Result<Option<::prost_types::Timestamp>, D::Error>
    where
        D: Deserializer<'de>,
    {
        let d = Option::<(i64, i32)>::deserialize(deserializer)?;
        let d = d.map(|(seconds, nanos)| ::prost_types::Timestamp { seconds, nanos });
        Ok(d)
    }
}

mod serialize_duration {
    use serde::Deserialize;
    use serde::Deserializer;
    use serde::Serialize;
    use serde::Serializer;

    pub fn serialize<S>(
        value: &Option<::prost_types::Duration>,
        serializer: S,
    ) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let d = value.as_ref().map(|v| (v.seconds, v.nanos));
        d.serialize(serializer)
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<Option<::prost_types::Duration>, D::Error>
    where
        D: Deserializer<'de>,
    {
        let d = Option::<(i64, i32)>::deserialize(deserializer)?;
        let d = d.map(|(seconds, nanos)| ::prost_types::Duration { seconds, nanos });
        Ok(d)
    }
}

result_convert!(AqueryResponse);
result_convert!(KillResponse);
result_convert!(PingResponse);
result_convert!(StatusResponse);
result_convert!(BuildResponse);
result_convert!(BxlResponse);
result_convert!(TestResponse);
result_convert!(UqueryResponse);
result_convert!(CqueryResponse);
result_convert!(TargetsResponse);
result_convert!(TargetsShowOutputsResponse);
result_convert!(GenericResponse);
result_convert!(UnstableDocsResponse);
result_convert!(ProfileResponse);
result_convert!(CleanResponse);
result_convert!(InstallResponse);
result_convert!(MaterializeResponse);
result_convert!(LspResponse);

define_request!(KillRequest);
define_request!(StatusRequest);
define_request!(PingRequest);

define_request!(BuildRequest, has(context, build_options));
define_request!(BxlRequest, has(context, build_options));
define_request!(TargetsRequest, has(context));
define_request!(AqueryRequest, has(context));
define_request!(CqueryRequest, has(context));
define_request!(UqueryRequest, has(context));
define_request!(TestRequest, has(context, build_options));
define_request!(GenericRequest, has(context));
define_request!(UnstableDocsRequest, has(context));
define_request!(ProfileRequest, has(context));
define_request!(MaterializeRequest, has(context));

define_request!(CleanRequest, has(context));
define_request!(InstallRequest, has(context, build_options));
