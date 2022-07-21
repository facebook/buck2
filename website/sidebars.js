/**
 * Creating a sidebar enables you to:
 - create an ordered group of docs
 - render a sidebar for each doc of that group
 - provide next/previous navigation

 The sidebars can be generated from the filesystem, or explicitly defined here.

 Create as many sidebars as you want.
 */

module.exports = {
  // By default, Docusaurus generates a sidebar from the docs folder structure
  // tutorialSidebar: [{type: 'autogenerated', dirName: '.'}],

  // But you can create a sidebar manually
  manualSidebar: [
    'index',
    'benefits',
    'migration_guide',
    'faq',
    'testimonials',
    {
      type: 'category',
      label: 'Rule authors',
      items: ['writing_rules', 'rule_api', 'transitive_sets', 'configurations', 'configuration_transitions', 'dynamic_dependencies', 'test_execution', 'optimization', 'rule_writing_tips', 'incremental_actions'],
    },
    {
      type: 'category',
      label: 'Developers',
      items: ['developers', 'heap_profiling', 'observability', 'options', 'parity_script', 'what-ran', 'bxl'],
    },
    {
      type: 'category',
      label: 'API docs',
      items: [{type: 'autogenerated', dirName: 'generated/native'}],
    },
    {
      type: 'category',
      label: 'Rules',
      items: ['generated/starlark/fbcode/buck2/prelude/prelude.bzl'],
    }
  ],
};
