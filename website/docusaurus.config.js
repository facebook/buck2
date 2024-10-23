/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

const lightCodeTheme = require('prism-react-renderer/themes/github');
const darkCodeTheme = require('prism-react-renderer/themes/dracula');
const { postProcessItems } = require('./sidebars.js');
const { fbContent, isInternal } = require('docusaurus-plugin-internaldocs-fb/internal');

// With JSDoc @type annotations, IDEs can provide config autocompletion
/** @type {import('@docusaurus/types').DocusaurusConfig} */
(module.exports = {
  title: 'Buck2',
  url: 'https://buck2.build',
  baseUrl: '/',
  onBrokenLinks: 'throw',
  trailingSlash: true,
  onBrokenMarkdownLinks: 'warn',
  favicon: 'img/logo.png',
  organizationName: 'facebook', // Usually your GitHub org/user name.
  projectName: 'buck2', // Usually your repo name.

  presets: [
    [
      require.resolve('docusaurus-plugin-internaldocs-fb/docusaurus-preset'),
      /** @type {import('@docusaurus/preset-classic').Options} */
      ({
        docs: {
          path: '../docs',
          sidebarPath: require.resolve('./sidebars_generated.js'),
          // Please change this to your repo.
          // editUrl: 'https://github.com/facebook/docusaurus/edit/main/website/',
          async sidebarItemsGenerator({ defaultSidebarItemsGenerator, ...args }) {
            const items = await defaultSidebarItemsGenerator({
              ...args
            });
            return postProcessItems(items);
          },
        },
        theme: {
          customCss: require.resolve('./src/css/custom.css'),
        },
      }),
    ],
  ],

  plugins: [
    [require.resolve('docusaurus-lunr-search'), {
      excludeRoutes: [
      ]
    }],
    [
      '@docusaurus/plugin-google-gtag',
      {
        trackingID: 'G-GEGGHE39PE',
        anonymizeIP: true,
      },
    ],
    [
      '@docusaurus/plugin-client-redirects',
      {
        redirects: [
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
        ] + (!isInternal()) ? [] : [
          // Internal-only redirects
          {
            to: '/docs/about/benefits/testimonials',
            from: '/docs/testimonials',
          },
          {
            to: '/docs/about/knowledge_sharing',
            from: '/docs/knowledge_sharing',
          },
        ],
      },
    ],
  ],

  themeConfig:
    /** @type {import('@docusaurus/preset-classic').ThemeConfig} */
    ({
      docs: {
        sidebar: {
          hideable: true,
        },
      },
      navbar: {
        title: 'Buck2',
        logo: {
          alt: 'My Site Logo',
          src: 'img/logo.svg',
        },
        items: [
          {
            type: 'doc',
            docId: 'index',
            position: 'left',
            label: 'Docs',
          },
          {
            to: '/docs/api',
            position: 'left',
            label: 'API',
            activeBaseRegex: '/docs/api',
          },
          {
            to: '/docs/prelude/globals',
            position: 'left',
            label: 'Rules',
            activeBasePath: '/docs/prelude',
          },
          {
            href: fbContent({
              internal: 'https://www.internalfb.com/code/buck2',
              external: 'https://github.com/facebook/buck2',
            }),
            label: fbContent({
              internal: 'CodeHub',
              external: 'GitHub',
            }),
            position: 'right',
          },
        ],
      },
      footer: {
        style: 'dark',
        links: [
          {
            title: 'Docs',
            items: [
              {
                label: 'User guide',
                to: '/docs',
              },
            ],
          },
          {
            title: 'Community',
            items: isInternal() ? [
              {
                label: 'User group',
                href: 'https://fb.workplace.com/groups/buck2users',
              },
              {
                label: 'Announcement group',
                href: 'https://fb.workplace.com/groups/buck2prototyping',
              },
            ] : [
              {
                label: 'GitHub issues',
                href: 'https://github.com/facebook/buck2/issues',
              },
            ],
          },
          {
            title: 'More',
            items: [
              {
                label: 'Code',
                href: fbContent({
                  internal: 'https://www.internalfb.com/code/fbsource/fbcode/buck2/',
                  external: 'https://github.com/facebook/buck2',
                }),
              },
              {
                label: 'Terms of Use',
                href: 'https://opensource.fb.com/legal/terms',
              },
              {
                label: 'Privacy Policy',
                href: 'https://opensource.fb.com/legal/privacy',
              },
            ],
          },
        ],
        copyright: `Copyright © ${new Date().getFullYear()} Meta Platforms, Inc. Built with Docusaurus.`,
      },
      prism: {
        theme: lightCodeTheme,
        darkTheme: darkCodeTheme,
      },
    }),
});
