/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

import React, {useState} from 'react'

export function RuleTypeDropdown(props: {options: {category: string; count: number}[]}) {
  const [dropdownActive, setDropdownActive] = useState(false)

  return (
    <div className={'dropdown ' + (dropdownActive ? 'is-active' : '')}>
      <div className="dropdown-trigger">
        <button
          className="button"
          aria-haspopup="true"
          aria-controls="dropdown-menu"
          onClick={() => setDropdownActive(!dropdownActive)}>
          <span>Select</span>
          <span className="icon is-small">
            <i className="fas fa-angle-down" aria-hidden="true"></i>
          </span>
        </button>
      </div>
      <div className="dropdown-menu" id="dropdown-menu" role="menu">
        <div className="dropdown-content">
          {props.options.map((v, index: number) => (
            <CheckboxItem key={index} label={v.category} count={v.count} />
          ))}
        </div>
      </div>
    </div>
  )
}

function CheckboxItem(props: {label: string; count: number}) {
  return (
    <div className="dropdown-item">
      <label className="checkbox">
        <input type="checkbox" />
        {' ' + props.label + ' (' + props.count + ')'}
      </label>
    </div>
  )
}
