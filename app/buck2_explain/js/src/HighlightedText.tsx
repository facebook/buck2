/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

import {tokenizer} from './flexSearch'

export function HighlightedText(props: {text: string; searchQuery: string}) {
  const words = tokenizer(props.searchQuery)
  const escapedWords = words.map(word => {
    return word.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')
  })
  const regex = new RegExp(`(${escapedWords.join('|')})`, 'gi')

  const parts = props.text.split(regex)

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
