/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

import React from 'react';

/**
 * Column: Renders a single “colored box + caption” column.
 *
 * Props:
 *   - label:     Text to show inside the colored box (e.g., "root", "buck2_lab/greeter_bin", "main")
 *   - caption:   Text to show below the box (e.g., "cell name", "target package", "target name")
 *   - background: Background color for the box (CSS color string)
 */
function Column({ label, caption, background, href}) {
  return (
    <div
      style={{
        display: 'flex',
        flexDirection: 'column',
        alignItems: 'center', // controls how the entire column (box + caption) is aligned horizontally
      }}
    >
      {/* Colored box with fixed height 2rem and vertical centering via `line-height` */}
      <div
        style={{
          backgroundColor: background,
          color: '#000000',
          height: '2.5rem',          // Fixed box height
          lineHeight: '2.5rem',      // Line-height matches height for vertical centering
          padding: '0 0.75rem',    // Horizontal padding to avoid text touching edges
          borderRadius: '4px',
          textAlign: 'center',     // Horizontal text alignment inside the box
          fontSize: '1.5rem',
        }}
      >
        {label}
      </div>

      {/* Caption below the colored box, with smaller font */}
      <div
        style={{
          fontSize: '1rem',
          color: background,
          marginTop: '0.5rem',
          lineHeight: 1,
        }}
      >
        {href ? (
          <a href={href} style={{ color: 'inherit', textDecoration: 'underline' }}>
            {caption}
          </a>
        ) : (
          caption
        )}
      </div>
    </div>
  );
}

/**
 * Separator: Renders a “//” or “:” symbol, with blank space underneath to align captions.
 *
 * Props:
 *   - symbol: The separator text to display (e.g., "//" or ":")
 */
function Separator({ symbol }) {
  return (
    <div
      style={{
        display: 'flex',
        flexDirection: 'column',
        alignItems: 'center',
      }}
    >
      {/*
        Margin-top = (2.5rem box height - ~1.5rem text row) / 2 ≈ 0.5rem
        so that the symbol vertically centers alongside the colored boxes above.
      */}
      <div
        style={{
          marginTop: '0.5rem',
          fontSize: '1.5rem',
          lineHeight: 1,
        }}
      >
        {symbol}
      </div>

      {/*
        Blank space of height 2rem, so that the second-row captions
        (in Column components) line up horizontally with other captions.
      */}
      <div style={{ height: '2rem' }} />
    </div>
  );
}

/**
 * TargetDiagram: Assembles three Column components and two Separator components
 * in a single horizontal flex container to illustrate, e.g.:
 *
 *   [root]   //   [buck2_lab/greeter_bin]   :   [main]
 *     cell name           target package           target name
 */
export default function TargetDiagram({ cell_name, pkg_name, target_name, cell_href, pkg_href, target_href }) {
  return (
    <div
      style={{
        display: 'flex',
        fontFamily: 'sans-serif',
        gap: '0.5rem',
        alignItems: 'flex-start',
      }}
    >
      <Column
        label={cell_name}
        caption="Cell Name"
        background="#9B82E5"
        href={cell_href}
      />
      <Separator symbol="//" />
      <Column
        label={pkg_name}
        caption="Target Package"
        background="#FFC74D"
        href={pkg_href}
      />
      <Separator symbol=":" />
      <Column
        label={target_name}
        caption="Target Name"
        background="#A0D995"
        href={target_href}
      />
    </div>
  );
}
