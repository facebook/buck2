/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

import React from 'react';
import clsx from 'clsx';
import Layout from '@theme/Layout';
import Link from '@docusaurus/Link';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import styles from './index.module.css';
import HomepageFeatures from '../components/HomepageFeatures';
import { FbInternalOnly, OssOnly } from 'docusaurus-plugin-internaldocs-fb/internal';

function HomepageHeader() {
  const {siteConfig} = useDocusaurusContext();
  return (
    <header className={clsx('hero hero--primary', styles.heroBanner)}>
      <div className="container">
        <h1 className="hero__title">{siteConfig.title}</h1>
        <p className="hero__subtitle">
          A large-scale build tool. The successor to Buck.<br/>
          Ready for users ∈ &#123;C++, Python, Rust<FbInternalOnly>, OCaml, Go</FbInternalOnly>&#125;
        </p>
        <FbInternalOnly>
          <div className={styles.buttons}>
            <Link
              className="button button--secondary button--lg"
              to="/docs/benefits">
              Why switch?
            </Link>
            <Link
              className="button button--secondary button--lg"
              to="/docs/migration_guide">
              How to switch
            </Link>
          </div>
        </FbInternalOnly>
        <OssOnly>
          <div className={styles.buttons}>
            <Link
              className="button button--secondary button--lg"
              to="/docs/why">
              Why Buck2?
            </Link>
            <Link
              className="button button--secondary button--lg"
              to="/docs/getting_started">
              Getting started
            </Link>
          </div>
        </OssOnly>
      </div>
    </header>
  );
}

export default function Home() {
  const {siteConfig} = useDocusaurusContext();
  return (
    <Layout
      title="Buck2 build system website"
      description="Description will go into a meta tag in <head />">
      <HomepageHeader />
      <main>
        <HomepageFeatures />
      </main>
    </Layout>
  );
}
