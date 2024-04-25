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

import { ByteBuffer } from 'flatbuffers';
import { Build } from './fbs/explain'



function App() {
    const target = 'fbcode//buck2:buck2'
    // TODO iguridi: hardcoded information from just 1 target
    const blobBase64 = "XXDATAXX"
    const decodedString = atob(blobBase64);
    let bytes = new Uint8Array(decodedString);

    let buf = new ByteBuffer(bytes);

    // Get an accessor to the root object inside the buffer.
    var build = Build.getRootAsBuild(buf);

    var target2 = build.targets(1);

    console.log(target2?.name());

    name = target2?.name();

    return (
      <>
        <p><i><span>{target}</span></i></p>
            <h2>{name}</h2>
      </>
    )
  }

const container = document.getElementById('root');
const root = createRoot(container)

root.render(<App />);
