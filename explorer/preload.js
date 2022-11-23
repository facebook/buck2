/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

const { contextBridge, ipcRenderer } = require('electron');

contextBridge.exposeInMainWorld('api', {
    // Get/set the current directory
    current_buck_dir: () => ipcRenderer.invoke('current-buck-dir'),
    select_buck_dir: () => ipcRenderer.invoke('select-buck-dir'),

    // Run buck2 <action>
    status: () => ipcRenderer.invoke('buck2-status'),
    targets: (target, host, mode) => ipcRenderer.invoke('buck2-targets', target, host, mode),
    attributes: (target, host, mode) => ipcRenderer.invoke('buck2-attributes', target, host, mode),
    providers: (target, host, mode) => ipcRenderer.invoke('buck2-providers', target, host, mode),
});
