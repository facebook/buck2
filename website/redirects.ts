/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

import { isInternal } from 'docusaurus-plugin-internaldocs-fb/internal';

const baseRedirects = [
    {
      to: '/docs/about/why',
      from: '/docs/why',
    },
    {
      to: '/docs/about/getting_started',
      from: '/docs/getting_started',
    },
    {
      to: '/docs/about/benefits/compared_to_buck1',
      from: '/docs/benefits',
    },
    {
      to: '/docs/about/bootstrapping',
      from: '/docs/bootstrapping',
    },
    {
      to: '/docs/prelude/globals',
      from: '/docs/api/rules',
    },
    {
      to: '/docs/api/starlark',
      from: '/docs/api/starlark/globals',
    },
    {
      to: '/docs/api/build',
      from: '/docs/api/build/globals',
    },
    {
      to: '/docs/api/bxl',
      from: '/docs/api/bxl/globals',
    },
];

// Internal-only redirects
const internalRedirects = !isInternal() ? [] : [
    {
        to: '/docs/about/benefits/testimonials',
        from: '/docs/testimonials',
      },
      {
        to: '/docs/about/knowledge_sharing',
        from: '/docs/knowledge_sharing',
      },
];

export const redirects = [...baseRedirects, ...internalRedirects];
