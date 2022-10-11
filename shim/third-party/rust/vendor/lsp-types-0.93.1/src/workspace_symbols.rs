use crate::{PartialResultParams, SymbolKindCapability, WorkDoneProgressParams};

use crate::{SymbolTag, TagSupport};

use serde::{Deserialize, Serialize};

#[derive(Debug, Eq, PartialEq, Clone, Default, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct WorkspaceSymbolClientCapabilities {
    /// This capability supports dynamic registration.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub dynamic_registration: Option<bool>,

    /// Specific capabilities for the `SymbolKind` in the `workspace/symbol` request.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub symbol_kind: Option<SymbolKindCapability>,

    /// The client supports tags on `SymbolInformation`.
    /// Clients supporting tags have to handle unknown tags gracefully.
    ///
    /// @since 3.16.0
    ///
    #[serde(
        default,
        skip_serializing_if = "Option::is_none",
        deserialize_with = "TagSupport::deserialize_compat"
    )]
    pub tag_support: Option<TagSupport<SymbolTag>>,
}

/// The parameters of a Workspace Symbol Request.
#[derive(Debug, Eq, PartialEq, Clone, Default, Deserialize, Serialize)]
pub struct WorkspaceSymbolParams {
    #[serde(flatten)]
    pub partial_result_params: PartialResultParams,

    #[serde(flatten)]
    pub work_done_progress_params: WorkDoneProgressParams,

    /// A non-empty query string
    pub query: String,
}
