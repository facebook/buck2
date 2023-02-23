/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;
use std::fmt::Debug;
use std::fmt::Display;
use std::future::Future;
use std::hash::Hash;
use std::time::Duration;

use futures::lock::Mutex;
use starlark_map::small_set::SmallSet;
use tokio::select;
use tokio::sync::mpsc;
use tokio::sync::oneshot;
use tracing::debug;
use tracing::error;
use tracing::trace;

pub trait CycleDescriptor: Debug + 'static {
    type Key: Debug + Display + Clone + Eq + Hash + Send + Sync;
    type Error: Debug + Display + Clone + Send + Sync;

    fn cycle_error(cycle: Vec<&Self::Key>) -> Self::Error;
}

/// The LazyCycleDetector is used to detect cycles in a parallel graph traversal. In essence, it
/// operates on a sequence of events for nodes that inform it when a node is "started", an edge is
/// discovered and a node is "finished" (which indicates that we won't see any new edges for that
/// node and that the entire graph below that node is guaranteed to have no cycles). After receiving
/// any events, the cycle detector will wait for an idle period (without new events) and then traverse
/// the active graph to check for cycles. Any nodes that are in or depend on a cycle will have their active
/// LazyCycleDetectorGuard notified of the cycle. The guard provides a function `guard_this` that
/// should be used to wrap futures where one "node" is waiting on dependencies, this will then
/// return a future that will exit early with an error if a cycle is detected.
///
/// For example:
///
/// ```ignore
/// struct FibDescriptor;
/// impl CycleDescriptor for FibDescriptor {
///     type Key: u32;
///     type Error: String;
///     fn cycle_error(cycle: Vec<&u32>) -> String {
///         "Detected Cycle".to_owned()
///     }
/// }
///
/// let cycle_detector = CycleDetector::<FibDescriptor>::new();
/// async fn fib(key: u32) -> u32 {
///   let guard = cycle_detector.start(key);
///   if key < 2 {
///     guard.finish();
///     return key;
///   }
///   guard.add_edge(key - 2);
///   guard.add_edge(key - 1);
///   let res = guard.guard_this(async move {
///     fib(key - 2).await + fib(key - 1).await
///   }.boxed()).await.unwrap().unwrap();
///   guard.finish();
///   res
/// }
/// ```
#[derive(Debug)]
pub struct LazyCycleDetector<C: CycleDescriptor> {
    events_sender: mpsc::UnboundedSender<Event<C>>,
}

impl<C: CycleDescriptor> LazyCycleDetector<C> {
    pub fn new() -> Self {
        Self::new_with_delay(Duration::from_millis(1000))
    }

    pub fn new_with_delay(idle_delay: Duration) -> Self {
        let (sender, receiver) = mpsc::unbounded_channel();
        tokio::spawn(
            CycleDetectorState {
                nodes: Vec::new(),
                events_receiver: receiver,
                node_ids: HashMap::new(),
                dirtied_nodes: HashSet::new(),
                idle_delay,
            }
            .run(),
        );
        Self {
            events_sender: sender,
        }
    }

    pub fn start(&self, key: C::Key) -> LazyCycleDetectorGuard<C> {
        let (sender, receiver) = oneshot::channel();
        self.events_sender
            .send(Event::Started(key.clone(), sender))
            .expect("cycle detector events receiver died while cycle detector still alive");

        LazyCycleDetectorGuard {
            key,
            events_sender: self.events_sender.clone(),
            error_receiver: Mutex::new(receiver),
        }
    }

    pub fn finish(&self, key: C::Key) {
        self.events_sender
            .send(Event::Finished(key))
            .expect("cycle detector events receiver died while cycle detector still alive");
    }
}

pub struct LazyCycleDetectorGuard<C: CycleDescriptor> {
    key: C::Key,
    error_receiver: Mutex<oneshot::Receiver<C::Error>>,
    events_sender: mpsc::UnboundedSender<Event<C>>,
}

impl<C: CycleDescriptor> LazyCycleDetectorGuard<C> {
    pub async fn guard_this<R, Fut: Future<Output = R>>(
        &self,
        fut: Fut,
    ) -> anyhow::Result<Result<R, C::Error>> {
        let mut guard = {
            match self.error_receiver.try_lock() {
                Some(v) => v,
                None => self.error_receiver.lock().await,
            }
        };

        select! {
            res = fut => {
                Ok(Ok(res))
            }
            cycle = &mut *guard => {
                match cycle {
                    Ok(e) => Ok(Err(e)),
                    Err(e) => Err(anyhow::anyhow!("error on cycle detector guard receiver: {}", e))
                }
            }
        }
    }

    pub fn add_edge(&self, dep: C::Key) {
        self.events_sender
            .send(Event::Edge(self.key.clone(), dep))
            .expect("cycle detector events receiver died while cycle detector still alive");
    }

    pub fn finish(&self) {
        self.events_sender
            .send(Event::Finished(self.key.clone()))
            .expect("cycle detector events receiver died while cycle detector still alive");
    }
}

#[derive(Debug)]
enum Event<C: CycleDescriptor> {
    // Guard is dropped for a node.
    Finished(C::Key),
    // A guard is created for a node.
    Started(C::Key, oneshot::Sender<C::Error>),
    // A (new) set of deps for a node is discovered. It must be the case that the node is no longer waiting on any previous set of deps.
    Edge(C::Key, C::Key),
}

// In typical use, this CycleDetectorState will sit with all nodes finished and
// so we want to keep this object small and so states with data are boxed.
enum NodeState<C: CycleDescriptor> {
    // Once a node is finished, we just track that as we no longer need to traverse it to detect cycles (as cycles
    // cause hangs, a node cannot get to the finished state if it has cycles within its deps).
    Finished,
    // A node will be in the "Known" state when we've seen an edge to it, but haven't yet seen its Started event.
    Known,
    // We store that a cycle has been detected so that any new edges pointing to this node can get informed of
    // the cycle.
    CycleDetected(Box<C::Error>),
    Working(Box<(VecDeque<u32>, oneshot::Sender<C::Error>)>),
}

struct CycleDetectorState<C: CycleDescriptor> {
    node_ids: HashMap<C::Key, u32>,
    nodes: Vec<(C::Key, NodeState<C>)>,
    // These are nodes for which we've seen a new out-edge since last we checked for cycles. We will start our next search at these nodes.
    dirtied_nodes: HashSet<u32>,
    events_receiver: mpsc::UnboundedReceiver<Event<C>>,
    idle_delay: Duration,
}

impl<C: CycleDescriptor> CycleDetectorState<C> {
    async fn run(mut self) {
        'outer: while let Some(ev) = self.events_receiver.recv().await {
            self.handle_event(ev);
            loop {
                // drain the queue without .awaiting.
                while let Ok(ev) = self.events_receiver.try_recv() {
                    self.handle_event(ev);
                }
                // Now we'll start the idle delay, if we get any events
                let timer = tokio::time::sleep(self.idle_delay);
                tokio::select! {
                    () = timer => {
                        self.check_for_cycles().await;
                        // break to the outer loop so that we don't start the timer again until we see a new event.
                        break;
                    }
                    // recv() is cancel-safe, see its doc.
                    ev = self.events_receiver.recv() => {
                        match ev {
                            Some(ev) => self.handle_event(ev),
                            None => {
                                self.check_for_cycles().await;
                                // the events_senders are all gone, break all the way to shutdown.
                                break 'outer;
                            },
                        }
                    }
                }
            }
        }
        debug!("shutting down cycle detector");
    }

    fn node_id(&mut self, k: &C::Key) -> u32 {
        if let Some(v) = self.node_ids.get(k) {
            return *v;
        }
        let id = self.node_ids.len() as u32;
        self.node_ids.insert(k.clone(), id);
        self.nodes.push((k.clone(), NodeState::Known));
        id
    }

    fn node_mut_by_id(&mut self, id: u32) -> &mut NodeState<C> {
        &mut self.nodes.get_mut(id as usize).unwrap().1
    }

    fn node_by_id(&self, id: u32) -> &NodeState<C> {
        &self.nodes.get(id as usize).unwrap().1
    }

    fn key_for_id(&self, id: u32) -> &C::Key {
        &self.nodes.get(id as usize).unwrap().0
    }

    fn node_mut(&mut self, k: &C::Key) -> &mut NodeState<C> {
        let id = self.node_id(k);
        self.node_mut_by_id(id)
    }

    fn handle_event(&mut self, ev: Event<C>) {
        match ev {
            Event::Finished(k) => {
                debug!("finished {}", k);
                *self.node_mut(&k) = NodeState::Finished;
            }
            Event::Started(k, sender) => {
                debug!("started {}", k);
                match self.node_mut(&k) {
                    NodeState::Finished => {
                        // this probably indicates a bug.
                        error!("cycle detector got start event after finished for {}", k)
                    }
                    NodeState::CycleDetected(e) => {
                        let _ignored = sender.send((**e).clone());
                    }
                    NodeState::Working(v) => v.1 = sender,
                    v @ NodeState::Known => *v = NodeState::Working(box (VecDeque::new(), sender)),
                }
            }
            Event::Edge(k, dep) => {
                let k_id = self.node_id(&k);
                let dep_id = self.node_id(&dep);
                let mut dirtied = false;
                match self.node_mut_by_id(k_id) {
                    NodeState::Finished => {
                        // ignore the event
                    }
                    NodeState::CycleDetected(_) => {
                        // ignored, if a node has a cycle (1) we already sent the cycle error and (2) we don't need to traverse it any more
                    }
                    NodeState::Known => {
                        error!(
                            "cycle detector received ::Edge event for a non-started node {}",
                            k
                        );
                    }
                    NodeState::Working(node) => {
                        dirtied = true;
                        node.0.push_back(dep_id);
                    }
                }

                if dirtied {
                    self.dirtied_nodes.insert(k_id);
                }
            }
        }
    }

    async fn check_for_cycles(&mut self) {
        enum PendingItem<T> {
            Pop,
            Node(T),
        }

        let mut pending = Vec::new();
        for id in std::mem::take(&mut self.dirtied_nodes) {
            // The node may have been dirtied by an event and then we saw
            // a ::Finished event before getting to idle.
            if let NodeState::Working(..) = self.node_by_id(id) {
                pending.push(PendingItem::Node(id));
            }
        }

        let mut visited = HashSet::new();

        let mut stack = SmallSet::new();

        // TODO(cjhopman): We should try to find the smallest cycle.
        // TODO(cjhopman): If we find a cycle, we should consider looking at the non-dirtied nodes
        // as well since they could depend on the cycle we found and so maybe we should be alerting
        // them as well. In practice, we know that any such nodes will be waiting on other such nodes
        // or ones that are dirtied and will get the error and we'd expect user code to then propagate
        // that up such that no nodes are waiting on the cycle. In any case, we still ensure that no
        // nodes are hanging waiting on the cycle.

        // We perform a dfs, maintaining a set of items in the current stack so that we can quickly check
        // for a cycle.
        while let Some(v) = pending.pop() {
            match v {
                PendingItem::Pop => {
                    let v = stack.pop().unwrap();
                    trace!("popping {}", v);
                }
                PendingItem::Node(v) => {
                    trace!("got item {}", v);
                    if !visited.insert(v) {
                        continue;
                    }

                    trace!("visiting {}", v);
                    let mut found_cycle: Option<C::Error> = None;

                    match self.node_by_id(v) {
                        // Above a node is only added to pending if it's in the ::Working state, below the same. It can only
                        // be changed to ::CycleDetected while its on the stack (and so is already visited and would've already
                        // passed this check).
                        NodeState::Finished | NodeState::Known | NodeState::CycleDetected(_) => {
                            unreachable!()
                        }
                        NodeState::Working(node) => {
                            if !stack.insert(v) {
                                unreachable!("v: {}, stack: {:?}", v, stack);
                            }
                            // Pop this item off the stack (children get pushed after this).
                            pending.push(PendingItem::Pop);
                            for child in node.0.iter() {
                                match self.node_by_id(*child) {
                                    NodeState::Finished | NodeState::Known => {
                                        // nothing to do
                                    }
                                    NodeState::CycleDetected(v) => {
                                        found_cycle = Some((**v).clone());
                                    }
                                    _ => {
                                        // It's important that this cycle detection happens after the check for whether child is already a Node::CycleDetected to
                                        // ensure that each node only ever gets added to a cycle_items vec once. Without that property, we could get N^2 runtime as
                                        // we could potentially compute the cycle_items list repeatedly for every node.
                                        if stack.contains(child) {
                                            // detected a cycle, now just collect the items in the cycle
                                            let mut cycle_items = Vec::new();
                                            let mut in_cycle = false;
                                            for item in &stack {
                                                if item == child {
                                                    in_cycle = true;
                                                }

                                                if in_cycle {
                                                    cycle_items.push(self.key_for_id(*item));
                                                }
                                            }
                                            found_cycle = Some(C::cycle_error(cycle_items));
                                        } else if !visited.contains(child) {
                                            trace!("pushing {}", child);
                                            pending.push(PendingItem::Node(*child));
                                        }
                                    }
                                }
                            }
                        }
                    }

                    if let Some(cycle) = found_cycle {
                        debug!("found cycle {}", cycle);
                        // We want to make sure that we don't iterate over the whole stack for every node. To ensure that, we iterate from the bottom and
                        // stop at the first that already has a detected cycle. This works fine because this loop is the only place that we mark CycleDetected
                        // and so we know if one item in the stack has it, everything before does as well.
                        for key in stack.iter().rev() {
                            let node = self.node_mut_by_id(*key);
                            // node should never be a ::Finished or ::Started because they never end up in the stack. If it's CycleDetected, we
                            // don't need to inform it about this cycle (which may or may not be the same).

                            if matches!(node, NodeState::CycleDetected(..)) {
                                break;
                            }

                            if matches!(node, NodeState::Working(..)) {
                                debug!("sending cycle error for {}", key);
                                let mut working = NodeState::CycleDetected(box cycle.clone());
                                std::mem::swap(&mut *node, &mut working);
                                match working {
                                    NodeState::Working(v) => {
                                        let _ignored = v.1.send(cycle.clone());
                                    }
                                    _ => {
                                        unreachable!()
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use futures::FutureExt;

    use super::*;

    #[derive(Debug)]
    struct SimpleCycleDescriptor;

    #[derive(Clone, Debug)]
    struct Error(Arc<Vec<u64>>);

    impl std::fmt::Display for Error {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "CycleError")
        }
    }

    impl CycleDescriptor for SimpleCycleDescriptor {
        type Key = u64;

        type Error = Error;

        fn cycle_error(cycle: Vec<&Self::Key>) -> Self::Error {
            Error(Arc::new(cycle.into_iter().copied().collect()))
        }
    }

    #[tokio::test]
    async fn should_detect_simple_cycle() {
        let detector =
            LazyCycleDetector::<SimpleCycleDescriptor>::new_with_delay(Duration::from_millis(1));
        let g0 = detector.start(0);
        let g1 = detector.start(1);
        let g2 = detector.start(2);

        g0.add_edge(1);
        g1.add_edge(2);
        g2.add_edge(0);

        let res = g0
            .guard_this(tokio::time::sleep(Duration::from_secs(120)).boxed())
            .await;
        match res {
            Ok(Err(..)) => {
                // good
            }
            Ok(Ok(..)) => {
                panic!("should've detected a cycle")
            }
            Err(..) => {
                panic!("should not have gotten a cycle detector error")
            }
        }
    }
}
