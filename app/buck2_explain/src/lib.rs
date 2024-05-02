/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fs;

use buck2_core::fs::paths::abs_path::AbsPathBuf;

#[allow(unused_imports)]
#[allow(unused_extern_crates)]
#[allow(clippy::extra_unused_lifetimes)]
mod explain_generated;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_query::query::environment::QueryTarget;
use flatbuffers::FlatBufferBuilder;

// use crate::explain_generated::explain::Bool;

mod fbs {
    pub use crate::explain_generated::explain::Build;
    pub use crate::explain_generated::explain::BuildArgs;
    pub use crate::explain_generated::explain::ConfiguredTargetNode;
    pub use crate::explain_generated::explain::ConfiguredTargetNodeArgs;
}

pub fn main(
    data: Vec<ConfiguredTargetNode>,
    output: &AbsPathBuf,
    fbs_dump: Option<&AbsPathBuf>,
) -> anyhow::Result<()> {
    let fbs = gen_fbs(data);

    let fbs = fbs.finished_data();
    let base64 = base64::encode(&fbs);

    // For dev purposes, dump the base64 encoded flatbuffer to a file
    if let Some(fbs_dump) = fbs_dump {
        fs::write(fbs_dump, &base64)?;
    }

    let html_out = {
        let html_in = include_str!("explain.html");
        let html_out = html_in.replace("XXDATAXX", &base64);
        // TODO: find a better way to assert it actually replaced something
        if html_in == html_out {
            return Err(anyhow::anyhow!("HTML template is not valid"));
        }
        html_out
    };

    fs::write(output, &html_out)?;
    Ok(())
}

fn gen_fbs(data: Vec<ConfiguredTargetNode>) -> FlatBufferBuilder<'static> {
    let mut builder = FlatBufferBuilder::new();

    // TODO iguridi: just 1 node for now
    let node = &data[0];
    let name = builder.create_shared_string(&node.name());

    // TODO iguridi: fill in other fields
    let target = fbs::ConfiguredTargetNode::create(
        &mut builder,
        &fbs::ConfiguredTargetNodeArgs {
            configured_target_label: None,
            type_: None,
            name: Some(name),
            default_target_platform: None,
            deps: None,
            package: None,
            oncall: None,
            target_configuration: None,
            execution_platform: None,
            plugins: None,
            target_compatible_with: None,
            compatible_with: None,
            exec_compatible_with: None,
            visibility: None,
            within_view: None,
            tests: None,
            attrs: None,
        },
    );

    let targets = builder.create_vector(&[target]);
    let build = fbs::Build::create(
        &mut builder,
        &fbs::BuildArgs {
            targets: Some(targets),
        },
    );
    builder.finish(build, None);
    builder
}

#[cfg(test)]
mod tests {
    use buck2_core::configuration::data::ConfigurationData;
    use buck2_core::target::label::label::TargetLabel;

    use super::*;
    pub use crate::explain_generated::explain::Build;
    #[test]
    fn test_gen_fbs() {
        // Setup data
        let data = {
            let target_label = TargetLabel::testing_parse("cell//pkg:foo");
            let configured_target_label = target_label.configure(ConfigurationData::testing_new());

            let target = ConfiguredTargetNode::testing_new(configured_target_label, "foo_lib");
            vec![target]
        };

        // Generate fbs
        let fbs = gen_fbs(data);
        let fbs = fbs.finished_data();

        // Read fbs
        let build = flatbuffers::root::<Build>(fbs).unwrap();
        let target = build.targets().unwrap().get(0);

        // Assert contents
        assert_eq!(target.name(), Some("foo"));
    }
}
