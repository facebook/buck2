const lightCodeTheme = require('prism-react-renderer/themes/github');
const darkCodeTheme = require('prism-react-renderer/themes/dracula');
const itemFilter = require('./sidebars.js').itemFilter;
const {fbContent, isInternal} = require('docusaurus-plugin-internaldocs-fb/internal');

// With JSDoc @type annotations, IDEs can provide config autocompletion
/** @type {import('@docusaurus/types').DocusaurusConfig} */
(module.exports = {
  title: 'Buck2',
  url: 'https://your-docusaurus-test-site.com',
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
          async sidebarItemsGenerator({defaultSidebarItemsGenerator, docs, item, ...args}) {
            const items = await defaultSidebarItemsGenerator({
              docs: itemFilter(item, docs),
              item: item,
              ...args
            });
            return items;
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
    }]
  ],

  themeConfig:
    /** @type {import('@docusaurus/preset-classic').ThemeConfig} */
    ({
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
            label: 'User guide',
          },
          {
            type: 'doc',
            docId: '/generated/starlark/prelude',
            position: 'left',
            label: 'API',
          },
          {
            href: fbContent({
              internal: 'https://www.internalfb.com/code/buck2',
              external: 'https://github.com/facebookincubator/buck2',
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
                href: 'https://github.com/facebookincubator/buck2/issues',
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
                  external: 'https://github.com/facebookincubator/buck2',
                }),
              },
            ],
          },
        ],
        copyright: `Copyright © ${new Date().getFullYear()} Meta, Inc. Built with Docusaurus.`,
      },
      prism: {
        theme: lightCodeTheme,
        darkTheme: darkCodeTheme,
      },
    }),
});
