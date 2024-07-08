use crate::src::Src;

/// A Node contains an AST node, pairing it with its source.
#[derive(PartialEq, Eq)]
pub struct Node<K> {
    pub inner: K,
    pub src: Src,
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
