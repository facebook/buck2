/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
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
