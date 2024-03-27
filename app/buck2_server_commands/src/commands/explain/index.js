/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

/**
 * A target and it's attibutes
 * @typedef {Object} ConfiguredTarget
 * @property {string[]} buck.deps - An array of dependencies for the target.
 */


// Global values. X<>X values are chosen via html script imports
const _data = {
	TARGET: XTARGETX,
	deps: XDEPSX,

	/**
	 * Returns a dictionary of reverse dependencies for a given set of dependencies.
	 * @param {Object.<[string], ConfiguredTarget>} deps - A dictionary of dependencies, where each
	 * key is a target name and the value is an object containing its dependencies.
	 * @returns {Object.<[string], Array.<string>>} A dictionary of reverse dependencies, where
	 * each key is a package name and the value is an array of packages that depend on it.
	 */
	getRdeps: function (deps) {
		const rdeps = {};
		for (const label in deps) {
			rdeps[label] = [];
			for (const t_dep of deps[label]["buck.deps"]) {
				if (t_dep in rdeps) {
					rdeps[t_dep].push(label);
				}
			}
		};
		return rdeps
	},
}
_data.rdeps = _data.getRdeps(_data.deps); // ugly hack

// Called from the html
const html = {
	last_id: 0,

	modal: function (title, contents, btn_class) {
		this.last_id += 1;

		const id = `modal-${this.last_id}`;

		btn_class = btn_class ?? "btn-info";

		return `
			<!-- Button to open modal -->
			<button class="badge text-body-secondary" data-bs-toggle="modal" data-bs-target="#${id}">
			  ${title}
			</button>

			<!-- Modal window -->
			<div class="modal modal-xl fade" id="${id}" tabindex="-1" aria-labelledby="exampleModalLabel" aria-hidden="true">
			  <div class="modal-dialog">
			    <div class="modal-content">
			      <div class="modal-header">
			        <h1 class="modal-title fs-5" id="exampleModalLabel">${title}</h1>
			        <button type="button" class="btn-close" data-bs-dismiss="modal" aria-label="Close"></button>
			      </div>
			      <div class="modal-body">
					${contents}
			      </div>
			      <div class="modal-footer">
			        <button type="button" class="btn btn-secondary" data-bs-dismiss="modal">Close</button>
			      </div>
			    </div>
			  </div>
			</div>
		`;
	},

	inlineHelp: function (message) {
		return `<div class="alert alert-dark" role="alert">${message}</div>`;
	},

	buttonHelp: function (message) {
		return `
			<div style="font-weight: normal; display: inline;">
				${html.modal("?", message, "btn-light")}
			</div>
		`;
	},

	modalPre: function (title, contents) {
		return html.modal(title, `<div class="font-monospace" style="white-space: pre-wrap">${contents}</div>`)
	},

	/**
	 * Writes to history for navigation and searchs
	 */
	searchWrite: function () {
		document.title = `buck2 viz`;
		let substr = _utils.searchBoxX().value;
		const params = new URLSearchParams(window.location.search);
		params.delete('node')
		params.delete('search')
		params.append('search', substr);
		window.history.pushState({}, '', `${window.location.pathname}?${params.toString()}`);
		_html.search(substr)
	},

	/**
	 * Checks params, loads target or makes search
	 */
	init: function () {
		// Default title unless overridden later.
		document.title = `buck2 viz`;

		const params = new URLSearchParams(window.location.search);
		// Set root target
		let targetq = params.get('target') || _data.TARGET;
		const title = _utils.getByIdX("root_target")
		title.innerHTML = `<p><i><span onclick='html.rootClick(event)' style="cursor: pointer">${_data.TARGET}</span></i></p>`

		const searchq = params.get('search');
		const nodeq = params.get('node');
		if (searchq != null && searchq !== "") {
			let search_elem = _utils.searchBoxX()
			search_elem.value = searchq
			_html.search(searchq)
			return
		}
		if (nodeq != null && nodeq !== "") {
			// Extremely hacky, but should do for now
			if (nodeq.indexOf("platform") != -1 && nodeq.indexOf("execution") != -1) {
				_html.loadExecCfg(nodeq)
				return
			}
			if (nodeq.indexOf(" ") == -1){
				_html.loadCfg(nodeq)
				return
			}
			_html.loadNode(nodeq, _data.deps, _data.rdeps)
			return
		}

		// Load root
		const root = Object.keys(_data.deps).filter((key) => key.includes(targetq));
		_html.loadNode(root[0], _data.deps, _data.rdeps)
	},

	rootClick: function (event) {
		// remove url params
		const params = new URLSearchParams(window.location.search);
		params.delete(SHOW_LIST_PARAM)
		params.delete('node')
		params.delete('search')

		let url = `${window.location.pathname}?${params.toString()}`
		_html.open(event, url);
	},

	/**
	 * Prevents clicking action if user is trying to select
	 * @param {string} id - The label of the target to load.
	 */
	openClick: function (event, id, show) {
		if (window.getSelection()?.toString() !== "") {
			return
		}
		// write url + history
		const params = new URLSearchParams(window.location.search);
		params.delete('node')
		params.delete('search')
		params.append('node', id);
		if (show) {
			params.delete('show');
			params.append('show', show);
		}

		let url = `${window.location.pathname}?${params.toString()}`
		_html.open(event, url);
	},

	/**
	 * This function hides all list types and then displays the specified one.
	 * @param {string} type - The list type to be displayed. The function
	 * will append '_list' to this type to find the actual element.
	 */
	show: function (type) {
		const params = new URLSearchParams(window.location.search);
		params.delete(SHOW_LIST_PARAM)
		params.append(SHOW_LIST_PARAM, type);
		window.history.pushState({}, '', `${window.location.pathname}?${params.toString()}`);
		_html.show(type)
	},
}

const SHOW_LIST_PARAM = 'show';
/**
 * @typedef {Object} Lists
 * @property {string} DEPS - Dependencies for that target
 * @property {string} RDEPS - Reverse dependencies
 * @property {string} ATTRS - Attributes obtained through cquery -A
 * @property {string} STACK - The call stack for that target
 * @property {string} ACTIONS - Actions associated with that target
 * @property {string} PROVIDERS
 * @property {string} SUBTARGETS
 */
/** @type {Lists} */
const Lists = Object.freeze({
	DEPS: 'deps',
	RDEPS: 'rdeps',
	ATTRS: 'attrs',
	STACK: 'stack',
	ACTIONS: 'actions',
	PROVIDERS: 'providers',
	SUBTARGETS: 'subtargets',
})

const FILE_PATTERN = /^.*\* (.+):(\d+), in (.*)$/g

// The kinds of ways an action can be executed by buck2.
const ExecutionKind = Object.freeze({
	0: "NOT_SET",
	// This action was executed locally.
	1: "LOCAL",
	// This action was executed via a remote execution service.
	2: "ACTION_CACHE",
	// This action was served via a remote execution service's action cache.
	3: "SIMPLE",
	// This action was served inline within buck2 due to its simplicity (e.g.
	// write, symlink, etc.
	4: "DEFERRED",
	// reserving 5 for deprecated ACTION_EXECUTION_KIND_SKIPPED
	// This action was logically executed, but didn't perform all the work.
	6: "DEFERRED",
	// This action was served by the local dep file cache and not executed.
	7: "LOCAL_DEP_FILE",
	// This action was executed locally via a worker.
	8: "LOCAL_WORKER",
	// This action was served by a remote execution service's action cache based
	// on a dep file based key.
	9: "REMOTE_DEP_FILE_CACHE",
})

// private stuff
const _html = {

	/**
	 * Opens the URL in a new tab if cmd/ctrl is pressed while clicking. Otherwise rerenders html
	 * @param {MouseEvent} event - The event object passed coming from the click
	 * @param {string} url - The URL to navigate to.
	 */
	open: function (event, url) {
		if (event.metaKey || event.ctrlKey) {
			// Open in new tab
			window.open(url);
		} else {
			// Save to navigation history
			window.history.pushState({}, '', url);
			// Replace html inline
			html.init();
		}
	},


	loadExecCfg: function(execution_platform) {
		// update DOM
		const xxx = _utils.getByIdX("xxx")
		xxx.innerHTML = ''
		xxx.innerHTML += `<h3>Execution platform: ${execution_platform}</h3>`;
		xxx.innerHTML += html.inlineHelp(`
			Execution platform defines how and where build command will be executed.
		`);
		xxx.innerHTML += `<p>(Data is fake for the demo)</p>`;
		xxx.innerHTML += `
			<table class="table">
				<tr>
					<th>Local enabled</th><td>true</td>
				</tr>
				<tr>
					<th>Remote enabled</td><td>true</td>
				</tr>
			</table>
			<p>TODO: add more interesting stuff</p>
		`;
	},

	loadCfg: function(config) {
		// update DOM
		const xxx = _utils.getByIdX("xxx")
		xxx.innerHTML = ''
		xxx.innerHTML += `
		<h3>Target configuration: ${config}</h3>
		${
			html.inlineHelp(`
				Configuration is a set of constraints. Configuration defines how <span class="font-monospace">select</span> is resolved.
			`)
		}
		<h4>Constraints</h4>
		<p>(Constraints are fake for the demo)</p>
		<table class="table">
			<thead>
				<tr>
					<th>Constraint value</th><th>Constraint setting</th>
				</tr>
			</thead>
			<tbody>
				<tr><td>ovr_config//build_mode/constraints:fbcode-build-info-ldflags-accepted</td> 		<td>ovr_config//build_mode/constraints:fbcode-build-info-ldflags</td></tr>
				<tr><td>ovr_config//build_mode/constraints:fbcode-build-info-mode-full</td> 				<td>ovr_config//build_mode/constraints:fbcode-build-info-mode</td></tr>
				<tr><td>ovr_config//build_mode/constraints:fbcode-custom-allocators-enabled</td> 		<td>ovr_config//build_mode/constraints:fbcode-custom-allocators</td></tr>
				<tr><td>ovr_config//build_mode/constraints:no-san</td>                   				<td>ovr_config//build_mode/constraints:san</td></tr>
				<tr><td>ovr_config//build_mode/constraints:opt</td>                      				<td>ovr_config//build_mode/constraints:core_build_mode</td></tr>
				<tr><td>ovr_config//build_mode/constraints:python-default-package-style-standalone</td> 	<td>ovr_config//build_mode/constraints:python-default-package-style</td></tr>
				<tr><td>ovr_config//compiler/constraints:clang</td>                      				<td>ovr_config//compiler/constraints:toolchain</td></tr>
				<tr><td>ovr_config//compiler/constraints:gcc-or-clang</td>               				<td>ovr_config//compiler/constraints:compiler-flavor</td></tr>
				<tr><td>ovr_config//constraints:any</td>                                 				<td>ovr_config//constraints:_</td></tr>
				<tr><td>ovr_config//cpu/constraints:arm64</td>                           				<td>ovr_config//cpu/constraints:cpu</td></tr>
				<tr><td>ovr_config//os/constraints:macos</td>                            				<td>ovr_config//os/constraints:os</td></tr>
				<tr><td>ovr_config//os/sdk/apple/constraints:macosx</td>                 				<td>ovr_config//os/sdk/apple/constraints:_</td></tr>
				<tr><td>ovr_config//product/constraints:mobileapps</td>                  				<td>ovr_config//product/constraints:product</td></tr>
				<tr><td>ovr_config//third-party/CUDA-projects/constraints:cuda-12</td>   				<td>ovr_config//third-party/CUDA-projects/constraints:version</td></tr>
				<tr><td>ovr_config//third-party/TensorRT/constraints:8.6.1.6</td>        				<td>ovr_config//third-party/TensorRT/constraints:TensorRT-version</td></tr>
				<tr><td>ovr_config//third-party/cuda/constraints:12</td>                 				<td>ovr_config//third-party/cuda/constraints:cuda-version</td></tr>
				<tr><td>ovr_config//third-party/cudnn/constraints:8.9.3</td>             				<td>ovr_config//third-party/cudnn/constraints:cudnn-version</td></tr>
				<tr><td>ovr_config//third-party/python-scientific-stack/constraints:2</td> 				<td>ovr_config//third-party/python-scientific-stack/constraints:version</td></tr>
				<tr><td>ovr_config//toolchain/fb/constraints:macos-minimal</td>          				<td>ovr_config//toolchain/fb/constraints:fbsource-toolchain-version</td></tr>
				<tr><td>ovr_config//toolchain/python/constraints:3.10</td>               				<td>ovr_config//toolchain/python/constraints:python-version</td></tr>
			</tbody>
		</table>
		</div>
	`;
	},

	/**
	 * Loads a target from the fully quallified label.
	 * Shows deps and rdeps for that target
	 * @param {string} id - The label of the target to load.
	 * @param {Object.<[string], import('./types.js').ConfiguredTarget>} deps
	 * @param {Object.<string, Array.<string>>} rdeps
	 */
	loadNode: function (id, deps, rdeps) {
		// Decide what to show based on param
		const params = new URLSearchParams(window.location.search);
		const show = params.get(SHOW_LIST_PARAM);

		document.title = `buck2 viz: ${id}`

		// update DOM
		const xxx = _utils.getByIdX("xxx")
		xxx.innerHTML = `<div id='target_box' class='p-4'></div>`

		const target_box = _utils.getByIdX("target_box")

		target_box.innerHTML = ""
		const info = deps[id]
		if (info == null) {
			// TODO: fallback to searching just by target label (stripping config)
			target_box.innerHTML += `<b>${id}</b>`
			target_box.innerHTML += `
        <ul>
        <li>No info found</li>
        </ul>`
		} else {
			target_box.innerHTML += `
			<ul class="ps-0 pt-3 no-dot">
				<li>
					<b>Rule type</b><br/>
					<p class="mb-2 font-monospace">${info["buck.type"]}</p>
				</li>
				<li>
					<b>Configuration</b><br/>
					<a style="cursor: pointer" class="mb-2" onclick="html.openClick(event, '${info["buck.target_configuration"]}')"><code>${info["buck.target_configuration"]}</code></a>
				</li>
				<li>
					<b>Execution platform</b><br/>
					<a style="cursor: pointer" class="mb-2" onclick="html.openClick(event, '${info["buck.execution_platform"]}')"><code>${info["buck.execution_platform"]}</code></a>
				</li>
				<li>
					<b>Oncall</b><br/>
					<p class="mb-2 font-monospace">${info["buck.oncall"]}</p>
				</li>
			</ul>`
		}

		const genButton = (value, tooltip, name) => `
			<li class="nav-item">
				<!-- TODO(nga): put proper URL here instead of cursor: pointer, so it could be copied -->
				<a style="cursor: pointer" class="nav-link ${show === value ? " active" : ""}" aria-disabled="true" title="${tooltip ?? ""}" onclick="html.openClick(event, '${id}', '${value}')">
					${name ?? value}
				</a>
			</li>
		`
		xxx.innerHTML += `
			<ul class="nav nav-tabs">
				${
					[
						genButton(Lists.DEPS),
						genButton(Lists.RDEPS, 'list of rdeps for this target that are part of the root target dependency tree'),
						genButton(Lists.ATTRS),
					].join(' ')
				}
			</ul>
		`
		if (show === "deps") {
			xxx.innerHTML += html.inlineHelp(`
				Direct dependencies of the target.
			`);
			xxx.innerHTML += `<div id="item_list"></div>`;
			const deps_list = _utils.getByIdX("item_list")
			let t_deps = (deps[id] ?? { "buck.deps": [] })["buck.deps"];
			if (t_deps.length === 0) {
				deps_list.innerHTML += `<i>No deps</i>`
			} else {
				deps_list.innerHTML += `<i>Deps:</i>`
				deps_list.innerHTML += `<ul id='deps_list_inner'></ul>`
				let deps_elem = _utils.getByIdX("deps_list_inner")
				deps_elem.innerHTML += t_deps.map(dep => `<li>${this.clickSpan(dep)}</li>`).join("");
			}
		}

		if (show === "rdeps") {
			xxx.innerHTML += html.inlineHelp(`
				Direct dependents of the target: list of configured target nodes which depend on this target.
			`);
			xxx.innerHTML += `<div id="item_list"></div>`;
			const rdeps_list = _utils.getByIdX("item_list")
			let t_rdeps = rdeps[id] ?? [];
			if (t_rdeps.length === 0) {
				rdeps_list.innerHTML += `<i>No rdeps</i>`
			} else {
				rdeps_list.innerHTML += `<i>Rdeps:</i>`
				rdeps_list.innerHTML += `<ul id='rdeps_list_inner'></ul>`
				let rdeps_elem = _utils.getByIdX("rdeps_list_inner")
				rdeps_elem.innerHTML += t_rdeps.map(rdep => `<li>${this.clickSpan(rdep)}</li>`).join("");
			}
		}

		if (show === "attrs") {
			xxx.innerHTML += html.inlineHelp(`
				Configured target node attributes, after selects are resolved.
			`);
			xxx.innerHTML += `<div id="item_list"></div>`;
			const attrs_list = _utils.getByIdX("item_list")
			const ignore = new Set([
				'buck.deps',
				'buck.execution_platform',
				'buck.oncall',
				'buck.package',
				'buck.type',
				'buck.target_call_stack',
				'buck.target_configuration',
				'name',
				'script_actions',
			])
			const attrs = deps[id] ?? {};

			xxx.innerHTML += `
				<table class="table">
					<thead>
						<tr>
							<th>Attr</th><th>Value</th>
						</tr>
					</thead>
					<tbody id="attrs-table-body">
					</tbody>
				</table>
			`;

			const attrTableBody = _utils.getByIdX("attrs-table-body");

			for (let field of Object.keys(attrs)) {
				const value = attrs[field]
				if (!(ignore.has(field) || value === null || value.length === 0 || _utils.isEmptyObject(value))) {
					attrTableBody.innerHTML += `
						<tr>
							<td>${field}</td>
							<td class="font-monospace" style="white-space: pre-wrap">${JSON.stringify(value, null, 2)}</td>
						</tr>
					`;
				}
			}
		}

		if (show === "actions") {
			xxx.innerHTML += html.inlineHelp(`
				Action is a unit of execution in buck2 (for example, an invocation of a compiler).
				Different actions may be executed for different subtargets.
			`);
			const actions = (deps[id] ?? { "script_actions": null })["script_actions"];
			if (actions == null) {
				xxx.innerHTML += `<p>No actions</p>`
			} else {
				xxx.innerHTML += `
					<table class="table">
						<thead>
							<tr>
								<th>Category</th>
								<th>Identifier</th>
								<th>
									Execution kind
									${
										html.buttonHelp(`
											<p>How the action was executed.</p>
											<table class="table">
												<tr><th>NOT_SET</th><td>Should not happen</td></tr>
												<tr><th>LOCAL</th><td>Executed locally</td></tr>
												<tr><th>ACTION_CACHE</th><td>Action is cached remotely</td></tr>
												<tr><th>SIMPLE</th><td>Simple action (like <span class="font-monospace">write</span>), executed locally</td></tr>
												<tr><th>DEFERRED</th><td>Action was logically executed, but didn't perform all the work</td></tr>
												<tr><th>LOCAL_DEP_FILE</th><td>Served by the local dep file cache and not executed</td></tr>
												<tr><th>LOCAL_WORKER</th><td>Locally via a worker</td></tr>
												<tr><th>REMOTE_DEP_FILE_CACHE</th><td>The action was served by a remote execution service's action cache based on a dep file based key</td></tr>
											</table>
										`)
									}
								</th>
								<th>Success</th>
								<th>Wall time (seconds)</th>
								<th>Output size (MiB)</th>
								<th>Event log</th>
							</tr>
						</thead>
						<tbody id="actions-list-table-body">
						</tbody>
					</table>
				`;
				const actions_list_table_body = _utils.getByIdX("actions-list-table-body");
				for (let a in actions) {
					const action = actions[a]
					const actionData = action.data.SpanEnd.data.ActionExecution
					const category = actionData.name.category
					const identifier = actionData.name.identifier
					const failed = actionData.name.failed
					const kind = actionData.execution_kind
					const kind_str = ExecutionKind[kind]
					const wall_time = (actionData.wall_time_us / 1_000_000)?.toFixed(1) ?? "";
					const output_size = (actionData.output_size >> 20)?.toFixed(1) ?? "";
					// TODO: look into how whatran does this
					actions_list_table_body.innerHTML += `
						<tr>
							<td>${category}</td>
							<td>${identifier}</td>
							<td>${kind_str}</td>
							<td style="text-align: center">${failed ? '&#10060' : '&#9989'}</td>
							<td style="text-align: right">${wall_time}</td>
							<td style="text-align: right">${output_size}</td>
							<td>
								${
									html.modalPre("Show", JSON.stringify(action, null, 2))
								}
							</td>
						</tr>
					`;
				}
			}
			xxx.innerHTML += `
				<div class="alert alert-warning" role="alert">
					Note, only actions found in event logs are shown for now.
					In proper version, all analysis actions will be shown.
				</div>
			`;
		}

		if (show === "providers") {
			xxx.innerHTML += html.inlineHelp(`
				Providers returned after running analysis.
				Providers carry information and build artifacts between targets.
			`);
			xxx.innerHTML += "<h5 class='ms-3'>Not implemented</h3>";
			xxx.innerHTML += `
				<p class='ms-3'>Here will be the tree of providers returned by the analysis.</p>
			`;
		}

		if (show === "subtargets") {
			xxx.innerHTML += html.inlineHelp(`
				Analysis may produce multiple subtargets,
				for example, <span class="font-monospace">rust_library</span> provides
				<span class="font-monospace">[expand]</span> subtarget to emit macro expansion.
				buck2 builds default subtarget by default.
			`);
			xxx.innerHTML += "<h5 class='ms-3'>Not implemented</h5>";
			xxx.innerHTML += `
				<p class='ms-3'>Here will be the tree of subtargets if analysis was ran.</p>
			`;
		}

		if (show === "cfg") {
			xxx.innerHTML += `
				<h3>Target configuration: ${info["buck.target_configuration"]}</h3>
				${
					html.inlineHelp(`
						Configuration is a set of constraints. Configuration defines how <span class="font-monospace">select</span> is resolved.
					`)
				}
				<h4>Constraints</h4>
				<p>(Constraints are fake for the demo)</p>
				<table class="table">
					<thead>
						<tr>
							<th>Constraint value</th><th>Constraint setting</th>
						</tr>
					</thead>
					<tbody>
						<tr><td>ovr_config//build_mode/constraints:fbcode-build-info-ldflags-accepted</td> 		<td>ovr_config//build_mode/constraints:fbcode-build-info-ldflags</td></tr>
						<tr><td>ovr_config//build_mode/constraints:fbcode-build-info-mode-full</td> 				<td>ovr_config//build_mode/constraints:fbcode-build-info-mode</td></tr>
						<tr><td>ovr_config//build_mode/constraints:fbcode-custom-allocators-enabled</td> 		<td>ovr_config//build_mode/constraints:fbcode-custom-allocators</td></tr>
						<tr><td>ovr_config//build_mode/constraints:no-san</td>                   				<td>ovr_config//build_mode/constraints:san</td></tr>
						<tr><td>ovr_config//build_mode/constraints:opt</td>                      				<td>ovr_config//build_mode/constraints:core_build_mode</td></tr>
						<tr><td>ovr_config//build_mode/constraints:python-default-package-style-standalone</td> 	<td>ovr_config//build_mode/constraints:python-default-package-style</td></tr>
						<tr><td>ovr_config//compiler/constraints:clang</td>                      				<td>ovr_config//compiler/constraints:toolchain</td></tr>
						<tr><td>ovr_config//compiler/constraints:gcc-or-clang</td>               				<td>ovr_config//compiler/constraints:compiler-flavor</td></tr>
						<tr><td>ovr_config//constraints:any</td>                                 				<td>ovr_config//constraints:_</td></tr>
						<tr><td>ovr_config//cpu/constraints:arm64</td>                           				<td>ovr_config//cpu/constraints:cpu</td></tr>
						<tr><td>ovr_config//os/constraints:macos</td>                            				<td>ovr_config//os/constraints:os</td></tr>
						<tr><td>ovr_config//os/sdk/apple/constraints:macosx</td>                 				<td>ovr_config//os/sdk/apple/constraints:_</td></tr>
						<tr><td>ovr_config//product/constraints:mobileapps</td>                  				<td>ovr_config//product/constraints:product</td></tr>
						<tr><td>ovr_config//third-party/CUDA-projects/constraints:cuda-12</td>   				<td>ovr_config//third-party/CUDA-projects/constraints:version</td></tr>
						<tr><td>ovr_config//third-party/TensorRT/constraints:8.6.1.6</td>        				<td>ovr_config//third-party/TensorRT/constraints:TensorRT-version</td></tr>
						<tr><td>ovr_config//third-party/cuda/constraints:12</td>                 				<td>ovr_config//third-party/cuda/constraints:cuda-version</td></tr>
						<tr><td>ovr_config//third-party/cudnn/constraints:8.9.3</td>             				<td>ovr_config//third-party/cudnn/constraints:cudnn-version</td></tr>
						<tr><td>ovr_config//third-party/python-scientific-stack/constraints:2</td> 				<td>ovr_config//third-party/python-scientific-stack/constraints:version</td></tr>
						<tr><td>ovr_config//toolchain/fb/constraints:macos-minimal</td>          				<td>ovr_config//toolchain/fb/constraints:fbsource-toolchain-version</td></tr>
						<tr><td>ovr_config//toolchain/python/constraints:3.10</td>               				<td>ovr_config//toolchain/python/constraints:python-version</td></tr>
					</tbody>
				</table>
				</div>
			`;
		}
	},


	/**
	 * Prevents clicking action if user is trying to select
	 * @param {string} dep - The label of the target in question,
	 * or ".. and N targets more" if there are too many targets
	 * @returns {string} A clickable span
	 */
	clickSpan: function (dep) {
		const parts = dep.split(" ", 2);
		const dep_html = parts.length ? `${parts[0]} <span style="white-space: nowrap">${parts[1]}<span>` : dep;
		if (dep.includes('targets more')) {
			return `<span>${dep}</span>`
		}
		return `<span onclick='html.openClick(event, "${dep}")' style="cursor: pointer">${dep_html}</span>`
	},

	/**
	 * Finds substring and shows matching targets
	 * @param {string} substr - The string we are searching for
	 */
	// TODO: move search to standalone html
	search: function (substr) {
		if (substr == null) {
			substr = _utils.searchBoxX().value
		}

		console.log("search substring", substr)

		const xxx = document.getElementById("xxx")
		if (xxx == null) { throw "xxx is null" }
		xxx.innerHTML = ""
		xxx.innerHTML += `<b>Search results for "${substr}"</b>`
		let search_universe = new Set([...Object.keys(_data.deps), ...Object.keys(_data.deps)])

		let nodes = Array.from(search_universe).filter(function (key) {
			return key.indexOf(substr) !== -1;
		});

		const len = nodes.length
		if (len === 0) {
			xxx.innerHTML += `<b>No results</b>`
		} else {
			xxx.innerHTML += `<ul id='search_targets'></ul>`
			let target_elem = _utils.getByIdX("search_targets")
			target_elem.innerHTML += nodes.map(dep => {
				return `<li>${this.clickSpan(dep)}</li>`
			}).join("");
		}
	},
}

// Make sure we check for params when we navigate around
window.addEventListener("popstate", function (_event) {
	html.init()
})


const _utils = {
	codeLinks: function (file, line) {
		const vscode_link = `<a class='text-body-secondary badge ps-0' href=https://www.internalfb.com/intern/nuclide/open/arc/?project=fbsource&paths[0]=${file}&lines=${line}>vscode</a>`
		const codehub_link = `<a class='text-body-secondary badge' href=https://www.internalfb.com/code/fbsource/${file}?lines=${line}>codehub</a>`
		return `${vscode_link}-${codehub_link}`
	},

	isEmptyObject: function (value) {
		if (value == null) {
			// null or undefined
			return false;
		}

		if (typeof value !== 'object') {
			// boolean, number, string, function, etc.
			return false;
		}

		const proto = Object.getPrototypeOf(value);

		// consider `Object.create(null)`, commonly used as a safe map
		// before `Map` support, an empty object as well as `{}`
		if (proto !== null && proto !== Object.prototype) {
			return false;
		}

		for (const prop in value) {
			if (Object.hasOwn(value, prop)) {
				return false;
			}
		}

		return true;
	},

	/**
	 * Retrieves the value of the searchbox input element.
	 * @returns {HTMLInputElement} The searchbox input element.
	 * @throws {Error} If the searchbox input element is not found or is not an HTMLInputElement.
	 */
	searchBoxX: function () {
		const box = document.querySelector('input[name="searchbox"]')
		if (!(box instanceof HTMLInputElement)) {
			throw "searchbox error"
		}
		return box;
	},

	/**
	 * Retrieves the element with the specified ID.
	 * @param {string} id - The ID of the element to retrieve.
	 * @returns {HTMLElement} The element with the specified ID.
	 * @throws {Error} If the element with the specified ID is not found.
	 */
	getByIdX: function (id) {
		let target_elem = document.getElementById(id)
		if (target_elem == null) { throw `${id} is null` }
		return target_elem
	}
}
