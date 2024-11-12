/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use dupe::Dupe;

#[derive(Debug, PartialEq, Eq, Ord, PartialOrd, Copy, Clone, Dupe)]
pub enum Applicability {
    All,
    /// Not meaningful in open source
    Internal,
    /// Only used in self-tests of buck2
    Testing,
}

/// Environment variable description.
#[derive(Debug, PartialEq, Eq, Ord, PartialOrd, Copy, Clone, Dupe)]
pub struct EnvInfoEntry {
    pub name: &'static str,
    pub ty: &'static str,
    pub default: Option<&'static str>,
    #[allow(dead_code)] // TODO(JakobDegen): Use next diff
    pub applicability: Applicability,
}

impl EnvInfoEntry {
    pub fn ty_short(&self) -> &'static str {
        self.ty.rfind(':').map_or(self.ty, |i| &self.ty[i + 2..])
    }
}

#[linkme::distributed_slice]
pub static ENV_INFO: [EnvInfoEntry];

#[cfg(test)]
mod tests {
    use crate::buck2_env;
    use crate::env::registry::Applicability;
    use crate::env::registry::EnvInfoEntry;
    use crate::env::registry::ENV_INFO;

    #[test]
    fn test_env_info() {
        let _ignore = buck2_env!("TEST_VAR_1", applicability = internal);
        let _ignore = buck2_env!("TEST_VAR_2", type = u32, default=20);
        let var_1 = ENV_INFO.iter().find(|e| e.name == "TEST_VAR_1").unwrap();
        let var_2 = ENV_INFO.iter().find(|e| e.name == "TEST_VAR_2").unwrap();
        assert_eq!(
            &EnvInfoEntry {
                name: "TEST_VAR_1",
                ty: "std :: string :: String",
                default: None,
                applicability: Applicability::Internal,
            },
            var_1
        );
        assert_eq!(
            &EnvInfoEntry {
                name: "TEST_VAR_2",
                ty: "u32",
                default: Some("20"),
                applicability: Applicability::All,
            },
            var_2
        );
    }

    #[test]
    fn test_ty_short() {
        let _ignore = buck2_env!("TEST_VAR_TY_SHORT");
        let var = ENV_INFO
            .iter()
            .find(|e| e.name == "TEST_VAR_TY_SHORT")
            .unwrap();
        assert_eq!("String", var.ty_short());
    }
}
