// Small module that re-exports a single entry from sidebars.js. This is done
// because docusaurus really does not like having anything else exported from the
// sidebars module, and we need to make some functionality available (itemFilter)
// in docusaurus.config.js

const manualSidebar = require('./sidebars.js').manualSidebar;

module.exports = {
  manualSidebar: manualSidebar,
}
