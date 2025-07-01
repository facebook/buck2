/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */


import React from 'react';
import { useColorMode } from '@docusaurus/theme-common';
import Mermaid from '@theme/Mermaid';

export default function TutorialMermaidDiagram({ children }) {
  const { colorMode } = useColorMode();

  const darkStyleLine = 'linkStyle default stroke:#ffffff';

  const isDarkTheme = colorMode === 'dark';
  const initConfig = "%%{init: {'theme':'default'}}%% \n\n";

  const lineStrokeStyle = isDarkTheme ? '\n\n' + darkStyleLine : '';
  const finalCode = initConfig + children + lineStrokeStyle;

  return <Mermaid value={finalCode} />;
}
