/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

/////////////////////////////////////////////////////////////////////
// DEMO USAGE
// Reading a file is based on the value in that file, plus the file encoding

use std::fs;
use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;

use async_trait::async_trait;
use derive_more::Display;
use gazebo::dupe::Dupe;
use tempfile::NamedTempFile;

use crate::DetectCycles;
use crate::Dice;
use crate::DiceComputations;
use crate::InjectedKey;
use crate::Key;

#[derive(Debug, Clone, Dupe, PartialEq)]
enum Encoding {
    Utf8,
    Ascii,
}

#[derive(Clone, Dupe, Debug, Display, Eq, Hash, PartialEq)]
#[display(fmt = "{:?}", self)]
struct EncodingConfig();

impl InjectedKey for EncodingConfig {
    type Value = Encoding;

    fn compare(x: &Self::Value, y: &Self::Value) -> bool {
        x == y
    }
}

struct Encodings<'c>(&'c DiceComputations);

impl<'c> Encodings<'c> {
    async fn get(&self) -> Result<Encoding, Arc<anyhow::Error>> {
        self.0
            .compute(&EncodingConfig())
            .await
            .map_err(|e| Arc::new(anyhow::anyhow!(e)))
    }

    fn set(&self, enc: Encoding) {
        self.0.changed_to(vec![(EncodingConfig(), enc)]);
    }
}

trait HasEncodings {
    fn encodings(&self) -> Encodings;
}

impl HasEncodings for DiceComputations {
    fn encodings(&self) -> Encodings {
        Encodings(self)
    }
}

struct Filesystem<'c>(&'c DiceComputations);

#[derive(Clone, Display, Debug, Eq, Hash, PartialEq)]
#[display(fmt = "File({})", "_0.display()")]
struct File(PathBuf);

impl<'c> Filesystem<'c> {
    async fn read_file(&self, file: &Path) -> Result<Arc<String>, Arc<anyhow::Error>> {
        #[async_trait]
        impl Key for File {
            type Value = Result<Arc<String>, Arc<anyhow::Error>>;
            async fn compute(&self, ctx: &DiceComputations) -> Self::Value {
                let encoding = ctx.encodings().get().await?;

                let s = fs::read_to_string(&self.0).unwrap();

                Ok(Arc::new(match encoding {
                    Encoding::Utf8 => s,
                    Encoding::Ascii => s.replace(":-)", "smile"),
                }))
            }

            fn equality(x: &Self::Value, y: &Self::Value) -> bool {
                match (x, y) {
                    (Ok(x), Ok(y)) => x == y,
                    _ => false,
                }
            }
        }

        self.0
            .compute(&File(file.to_path_buf()))
            .await
            .map_err(|e| Arc::new(anyhow::anyhow!(e)))?
    }

    fn changed(&self, file: &Path) {
        self.0.changed(vec![File(file.to_path_buf())])
    }
}

trait HasFilesystem<'c> {
    fn filesystem(&'c self) -> Arc<Filesystem<'c>>;
}

impl<'c> HasFilesystem<'c> for DiceComputations {
    fn filesystem(&'c self) -> Arc<Filesystem<'c>> {
        Arc::new(Filesystem(self))
    }
}

#[test]
fn demo() {
    let temp = NamedTempFile::new().unwrap();
    let f = PathBuf::from(temp.path());

    let rt = tokio::runtime::Runtime::new().unwrap();
    let dice = Dice::builder().build(DetectCycles::Enabled);

    let ctx = dice.ctx();
    ctx.encodings().set(Encoding::Utf8);
    ctx.commit();

    let set = |x: &str| fs::write(&f, x).unwrap();

    let get = |x: &str| {
        rt.block_on(async {
            let contents = dice.ctx().filesystem().read_file(&f).await.unwrap();
            assert_eq!(*contents, x)
        })
    };

    set(":-)");

    get(":-)");

    // doesn't change because I didn't dirty it
    set("hello :-)");

    get(":-)");

    let ctx = dice.ctx();
    ctx.filesystem().changed(&f);
    ctx.commit();
    get("hello :-)");

    let ctx = dice.ctx();
    ctx.encodings().set(Encoding::Ascii);
    ctx.commit();
    get("hello smile");
}
