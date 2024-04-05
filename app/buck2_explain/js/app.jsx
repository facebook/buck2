/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

import React from 'react';
import { createRoot } from 'react-dom/client';


function App() {
    const target = 'fbcode//buck2:buck2'
    // hardcoded information from just 1 target
    const blobBase64 = "XXDATAXX"
    const decodedString = atob(blobBase64);
    const info = JSON.parse(decodedString);

    const attrs = info["fbcode//buck2:buck2 (cfg:macos-x86_64-macosx-no-san#8be81d993dfb76a0)"]

    const keys = Object.keys(attrs);

    return (
      <>
        <p><i><span>{target}</span></i></p>
        <h1>{target}</h1>
        <table>
          <tbody>
            {keys.map(key => (
              <tr key={key}>
                <td>{key}</td>
                <td>{JSON.stringify(attrs[key])}</td>
              </tr>
            ))}
          </tbody>
        </table>
      </>
    )
  }

const container = document.getElementById('root');
const root = createRoot(container)

root.render(<App />);
