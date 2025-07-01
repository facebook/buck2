/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

import {tokenizer} from './flexSearch'

export function HighlightedText(props: {text: string; regex: RegExp}) {
  const {text, regex} = props
  const parts = text.split(regex)

  return (
    <>
      {parts.map((part, index) => {
        if (regex.test(part)) {
          return (
            <span key={index} className="highlight">
              {part}
            </span>
          )
        } else {
          return part
        }
      })}
    </>
  )
}

export function searchRegex(query: string): RegExp {
  const words = tokenizer(query)
  const escapedWords = words.map(word => {
    return word.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')
  })
  return new RegExp(`(${escapedWords.join('|')})`, 'gi')
}
