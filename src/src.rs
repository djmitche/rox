/// A reference to a position in the source file.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Src {
    pub offset: usize,
    pub len: usize,
}
