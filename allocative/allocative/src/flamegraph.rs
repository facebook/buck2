/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cell::Ref;
use std::cell::RefCell;
use std::cell::RefMut;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt::Write as _;
use std::mem;
use std::rc::Rc;

use crate::global_root::roots;
use crate::key::Key;
use crate::visitor::NodeKind;
use crate::visitor::Visitor;
use crate::visitor::VisitorImpl;
use crate::Allocative;

#[derive(Debug)]
pub struct FlameGraphOutput {
    flamegraph: String,
    warnings: String,
}

impl FlameGraphOutput {
    /// Flamegraph source, can be fed to `flamegraph.pl` or `inferno`.
    pub fn flamegraph(&self) -> String {
        self.flamegraph.clone()
    }

    /// Warnings. Text file in unspecified format.
    pub fn warnings(&self) -> String {
        self.warnings.clone()
    }
}

#[derive(Default, Eq, PartialEq, Clone, Debug)]
struct TreeData {
    /// Size of this node including children but excluding unique/shared children.
    /// For example for `String` this would be `size_of::<String>()`.
    size: usize,
    /// Size excluding children. This value is output to flamegraph for given stack.
    /// Can be negative if nodes provides sizes incorrectly.
    rem_size: isize,
    /// Whether this node is `Box` something.
    unique: bool,
    /// Child nodes.
    children: HashMap<Key, Tree>,
}

impl TreeData {
    fn inline_children_size(&self) -> isize {
        self.size as isize - self.rem_size
    }
}

#[derive(Default, Debug, Clone, Eq, PartialEq)]
struct Tree(Rc<RefCell<TreeData>>);

impl Tree {
    fn borrow_mut(&self) -> RefMut<TreeData> {
        self.0.borrow_mut()
    }

    fn borrow(&self) -> Ref<TreeData> {
        self.0.borrow()
    }

    fn child(&self, name: Key) -> Tree {
        self.0
            .borrow_mut()
            .children
            .entry(name)
            .or_default()
            .clone()
    }

    fn children(&self) -> Vec<Tree> {
        self.0.borrow().children.values().cloned().collect()
    }

    fn write_flame_graph(&self, stack: &[&str], w: &mut String, warnings: &mut String) {
        let borrow = self.borrow();
        if borrow.rem_size > 0 {
            if stack.is_empty() {
                // don't care.
            } else {
                writeln!(w, "{} {}", stack.join(";"), borrow.rem_size).unwrap();
            }
        } else if borrow.rem_size < 0 && !stack.is_empty() {
            writeln!(
                warnings,
                "Incorrect size declaration for node `{}`, size of self: {}, size of inline children: {}",
                stack.join(";"),
                borrow.size,
                borrow.inline_children_size()
            )
            .unwrap();
        }
        let mut children: Vec<(&Key, &Tree)> = Vec::from_iter(&borrow.children);
        let mut stack = stack.to_vec();
        children.sort_by_key(|(k, _)| *k);
        for (key, child) in children {
            stack.push(key);
            child.write_flame_graph(&stack, w, warnings);
            stack.pop().unwrap();
        }
    }

    fn to_flame_graph(&self) -> (String, String) {
        let mut s = String::new();
        let mut warnings = String::new();
        self.write_flame_graph(&[], &mut s, &mut warnings);
        (s, warnings)
    }
}

#[derive(Default, Clone, Debug)]
struct TreeStack {
    stack: Vec<Tree>,
    tree: Tree,
}

impl TreeStack {
    fn down(&mut self, key: Key) {
        self.stack.push(self.tree.clone());
        let child = self.tree.child(key);
        self.tree = child;
    }

    #[must_use]
    fn up(&mut self) -> bool {
        if let Some(pop) = self.stack.pop() {
            self.tree = pop;
            true
        } else {
            false
        }
    }
}

/// Build a flamegraph from given root objects.
///
/// # Example
///
/// ```
/// use allocative::FlameGraphBuilder;
/// use allocative::Allocative;
///
/// #[derive(Allocative)]
/// struct Foo {
///     data: String,
/// };
///
/// let foo1 = Foo { data: "Hello, world!".to_owned() };
/// let foo2 = Foo { data: "Another message!".to_owned() };
///
/// let mut  flamegraph = FlameGraphBuilder::default();
/// flamegraph.visit_root(&foo1);
/// flamegraph.visit_root(&foo2);
/// let flamegraph_src = flamegraph.finish().flamegraph();
/// ```
///
/// And now `flamegraph_src` can be fed to either [flamegraph.pl] or [inferno].
///
/// [flamegraph.pl]: https://github.com/brendangregg/FlameGraph
/// [inferno]: https://github.com/jonhoo/inferno
#[derive(Debug)]
pub struct FlameGraphBuilder {
    /// Visited shared pointers.
    visited_shared: HashSet<*const ()>,
    /// Current node we are processing in `Visitor`.
    current: TreeStack,
    /// Previous stack when entering shared pointer.
    shared: Vec<TreeStack>,
    /// Data root.
    root: Tree,
    /// Is root visitor created?
    entered_root_visitor: bool,
}

impl Default for FlameGraphBuilder {
    fn default() -> FlameGraphBuilder {
        let root = Tree::default();
        FlameGraphBuilder {
            visited_shared: HashSet::new(),
            current: TreeStack {
                stack: Vec::new(),
                tree: root.clone(),
            },
            shared: Vec::new(),
            root,
            entered_root_visitor: false,
        }
    }
}

impl FlameGraphBuilder {
    pub fn root_visitor(&mut self) -> Visitor {
        assert!(!self.entered_root_visitor);
        self.entered_root_visitor = true;
        Visitor {
            visitor: self,
            node_kind: NodeKind::Root,
        }
    }

    /// Collect tree sizes starting from given root.
    pub fn visit_root(&mut self, root: &dyn Allocative) {
        let mut visitor = self.root_visitor();
        root.visit(&mut visitor);
        visitor.exit();
    }

    /// Collect data from global roots registered with
    /// [`register_root`](crate::register_root).
    pub fn visit_global_roots(&mut self) {
        for root in roots() {
            self.visit_root(root);
        }
    }

    fn finish_impl(self) -> Tree {
        assert!(self.shared.is_empty());
        assert!(self.current.stack.is_empty());
        assert!(!self.entered_root_visitor);
        Self::update_sizes(self.root.clone());
        self.root
    }

    /// Finish building the flamegraph.
    pub fn finish(self) -> FlameGraphOutput {
        let (flamegraph, warnings) = self.finish_impl().to_flame_graph();
        FlameGraphOutput {
            flamegraph,
            warnings,
        }
    }

    /// Finish building the flamegraph and return the flamegraph output.
    pub fn finish_and_write_flame_graph(self) -> String {
        self.finish().flamegraph
    }

    fn update_sizes(tree: Tree) {
        for child in tree.children() {
            Self::update_sizes(child);
        }
        let children_size = if tree.borrow().unique {
            0
        } else {
            tree.children()
                .into_iter()
                .map(|child| child.borrow().size)
                .sum::<usize>()
        };
        let size = tree.borrow().size;
        tree.borrow_mut().rem_size = (size as isize).saturating_sub(children_size as isize);
    }

    fn exit_impl(&mut self) {
        assert!(self.entered_root_visitor);

        let up = self.current.up();
        if !up {
            if let Some(mut shared) = self.shared.pop() {
                assert!(shared.up());
                self.current = shared;
            } else {
                self.entered_root_visitor = false;
            }
        }
    }
}

impl VisitorImpl for FlameGraphBuilder {
    fn enter_inline_impl(&mut self, name: Key, size: usize, _parent: NodeKind) {
        self.current.down(name);
        self.current.tree.borrow_mut().size += size;
    }

    fn enter_unique_impl(&mut self, name: Key, size: usize, _parent: NodeKind) {
        self.current.down(name);
        self.current.tree.borrow_mut().size += size;
        // TODO: deal with potential issue when node is both unique and not.
        // TODO: record some malloc overhead.
        self.current.tree.borrow_mut().unique = true;
    }

    #[must_use]
    fn enter_shared_impl(
        &mut self,
        name: Key,
        size: usize,
        _ptr: *const (),
        _parent: NodeKind,
    ) -> bool {
        self.current.down(name);
        self.current.tree.borrow_mut().size += size;

        if !self.visited_shared.insert(_ptr) {
            self.exit_impl();
            return false;
        }

        self.shared.push(mem::take(&mut self.current));
        self.current = TreeStack {
            stack: Vec::new(),
            tree: self.root.clone(),
        };
        true
    }

    fn exit_inline_impl(&mut self) {
        self.exit_impl();
    }

    fn exit_unique_impl(&mut self) {
        self.exit_impl();
    }

    fn exit_shared_impl(&mut self) {
        self.exit_impl();
    }

    fn exit_root_impl(&mut self) {
        self.exit_impl();
    }
}

#[cfg(test)]
mod tests {

    use crate::flamegraph::FlameGraphBuilder;
    use crate::flamegraph::Tree;
    use crate::key::Key;

    #[test]
    fn test_empty() {
        let mut fg = FlameGraphBuilder::default();
        fg.root_visitor().exit();
        let tree = fg.finish_impl();

        let expected = Tree::default();
        assert_eq!(expected, tree);
        assert_eq!("", tree.to_flame_graph().0);
    }

    #[test]
    fn test_simple() {
        let mut fg = FlameGraphBuilder::default();
        fg.root_visitor().visit_simple(Key::new("a"), 10);
        let tree = fg.finish_impl();

        let expected = Tree::default();
        expected.borrow_mut().size = 0;
        expected.borrow_mut().rem_size = -10;
        expected.child(Key::new("a")).borrow_mut().size = 10;
        expected.child(Key::new("a")).borrow_mut().rem_size = 10;
        assert_eq!(expected, tree);
        assert_eq!("a 10\n", tree.to_flame_graph().0);
    }

    #[test]
    fn test_unique() {
        let mut fg = FlameGraphBuilder::default();
        let mut visitor = fg.root_visitor();
        let mut s = visitor.enter(Key::new("Struct"), 10);
        s.visit_simple(Key::new("a"), 3);
        let mut un = s.enter_unique(Key::new("p"), 6);
        un.visit_simple(Key::new("x"), 13);
        un.exit();
        s.exit();
        visitor.exit();

        let tree = fg.finish_impl();

        assert_eq!(
            "\
                Struct 1\n\
                Struct;a 3\n\
                Struct;p 6\n\
                Struct;p;x 13\n\
            ",
            tree.to_flame_graph().0,
            "{:#?}",
            tree,
        );
    }

    #[test]
    fn test_shared() {
        let p = 10;

        let mut fg = FlameGraphBuilder::default();
        let mut visitor = fg.root_visitor();

        for _ in 0..2 {
            let mut s = visitor.enter(Key::new("Struct"), 10);
            s.visit_simple(Key::new("a"), 3);
            {
                let sh = s.enter_shared(Key::new("p"), 6, &p as *const i32 as *const ());
                if let Some(mut sh) = sh {
                    sh.visit_simple(Key::new("Shared"), 13);
                    sh.exit();
                }
            }
            s.exit();
        }

        visitor.exit();

        let tree = fg.finish_impl();

        assert_eq!(
            "\
            Shared 13\n\
            Struct 2\n\
            Struct;a 6\n\
            Struct;p 12\n\
        ",
            tree.to_flame_graph().0,
            "{:#?}",
            tree,
        );
    }

    #[test]
    fn test_inline_children_too_large() {
        let mut fg = FlameGraphBuilder::default();
        let mut visitor = fg.root_visitor();
        let mut child_visitor = visitor.enter(Key::new("a"), 10);
        child_visitor.visit_simple(Key::new("b"), 13);
        child_visitor.exit();
        visitor.exit();
        let output = fg.finish();
        assert_eq!("a;b 13\n", output.flamegraph());
        assert_eq!(
            "Incorrect size declaration for node `a`, size of self: 10, size of inline children: 13\n",
            output.warnings()
        );
    }
}
