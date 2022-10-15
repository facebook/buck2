/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use futures::Future;
use once_cell::sync::OnceCell;
use tokio::sync::Mutex;

pub struct AsyncOnceCell<T> {
    cell: OnceCell<T>,
    initialized: Mutex<bool>,
}

impl<T> AsyncOnceCell<T> {
    pub fn new() -> Self {
        Self {
            cell: OnceCell::new(),
            initialized: Mutex::new(false),
        }
    }

    pub fn get(&self) -> Option<&T> {
        self.cell.get()
    }

    pub async fn get_or_init<F: Future<Output = T>>(&self, fut: F) -> &T {
        if let Some(val) = self.cell.get() {
            return val;
        }

        let mut initialized = self.initialized.lock().await;

        if *initialized {
            return self.cell.get().unwrap();
        }

        let val = fut.await;

        if self.cell.set(val).is_err() {
            unreachable!();
        }
        *initialized = true;
        return self.cell.get().unwrap();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test() -> anyhow::Result<()> {
        let cell1 = AsyncOnceCell::new();

        let counter = cell1.get_or_init(async { 42u32 }).await;
        assert_eq!(42, *counter);

        let counter = cell1.get_or_init(async { 11111111u32 }).await;
        assert_eq!(42, *counter);

        // There's no guarantee that these two actually race with each other, we can only hope.
        let cell2 = AsyncOnceCell::new();
        let ref1 = cell2.get_or_init(async { tokio::spawn(async { 1u32 }).await.unwrap() });
        let ref2 = cell2.get_or_init(async { tokio::spawn(async { 2u32 }).await.unwrap() });

        let (v1, v2) = tokio::join!(ref1, ref2);

        assert!(*v1 == 1 || *v1 == 2);
        assert_eq!(v1, v2);

        Ok(())
    }
}
