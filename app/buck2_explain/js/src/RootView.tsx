import React, {useContext} from 'react'
import {Target} from './Target'
import {DataContext} from './App'

/**
 * Shows the root target
 */
export function RootView(props: {view: string}) {
  const {rootTarget} = useContext(DataContext)

  return rootTarget == null ? <p>No root target</p> : <Target target={rootTarget} />
}
