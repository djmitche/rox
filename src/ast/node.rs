use crate::src::Src;

/// A Node contains an AST node, pairing it with its source.
#[derive(PartialEq, Eq)]
pub struct Node<K> {
    pub inner: K,
    pub src: Src,
}

impl<K: std::fmt::Debug> Node<K> {
    /// Update the `src` in this node.
    pub fn with_src(mut self, src: Src) -> Self {
        self.src = src;
        self
    }

    /// Add the given src to this node's src.
    pub fn add_src(mut self, src: Src) -> Self {
        self.src = self.src + src;
        self
    }
}

impl<K: std::fmt::Debug> std::fmt::Debug for Node<K> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple(&format!("Node@{}+{}", self.src.offset, self.src.len))
            .field(&self.inner)
            .finish()
    }
}

/// A NodeRef is a boxed node, used to avoid infinite data structures.
pub type NodeRef<K> = Box<Node<K>>;
