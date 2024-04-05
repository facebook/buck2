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
    const blobBase64 = "ewogICAgImZiY29kZS8vYnVjazI6YnVjazIgKGNmZzptYWNvcy14ODZfNjQtbWFjb3N4LW5vLXNhbiM4YmU4MWQ5OTNkZmI3NmEwKSI6IHsKICAgICAgImJ1Y2sudHlwZSI6ICJhbGlhcyIsCiAgICAgICJidWNrLnBhY2thZ2UiOiAiZmJjb2RlLy9idWNrMjpUQVJHRVRTIiwKICAgICAgIm5hbWUiOiAiYnVjazIiLAogICAgICAiZGVmYXVsdF90YXJnZXRfcGxhdGZvcm0iOiAib3ZyX2NvbmZpZy8vcGxhdGZvcm0vbWFjb3M6eDg2XzY0LWZic291cmNlIChjZmc6bWFjb3MteDg2XzY0LW1hY29zeC1uby1zYW4jOGJlODFkOTkzZGZiNzZhMCkiLAogICAgICAidGFyZ2V0X2NvbXBhdGlibGVfd2l0aCI6IFtdLAogICAgICAiY29tcGF0aWJsZV93aXRoIjogW10sCiAgICAgICJleGVjX2NvbXBhdGlibGVfd2l0aCI6IFtdLAogICAgICAidmlzaWJpbGl0eSI6IFsKICAgICAgICAiUFVCTElDIgogICAgICBdLAogICAgICAid2l0aGluX3ZpZXciOiBbCiAgICAgICAgIlBVQkxJQyIKICAgICAgXSwKICAgICAgIm1ldGFkYXRhIjoge30sCiAgICAgICJ0ZXN0cyI6IFtdLAogICAgICAiX2FwcGxlX3BsYXRmb3JtcyI6IHt9LAogICAgICAiYWN0dWFsIjogImZiY29kZS8vYnVjazIvYXBwL2J1Y2syOmJ1Y2syLWJpbiAoY2ZnOm1hY29zLXg4Nl82NC1tYWNvc3gtbm8tc2FuIzhiZTgxZDk5M2RmYjc2YTApIiwKICAgICAgImJ1Y2syX2NvbXBhdGliaWxpdHkiOiAidW5rbm93biIsCiAgICAgICJjb250YWN0cyI6IFtdLAogICAgICAiZGVmYXVsdF9ob3N0X3BsYXRmb3JtIjogIm92cl9jb25maWcvL3BsYXRmb3JtL21hY29zOng4Nl82NC1mYnNvdXJjZS1sb2NhbCAoY2ZnOm1hY29zLXg4Nl82NC1tYWNvc3gtbm8tc2FuIzhiZTgxZDk5M2RmYjc2YTApIiwKICAgICAgImxhYmVscyI6IFsKICAgICAgICAiY2k6YWFyY2g2NDpza2lwX3Rlc3QiCiAgICAgIF0sCiAgICAgICJsaWNlbnNlcyI6IFtdCiAgICB9CiAgfQo="
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
