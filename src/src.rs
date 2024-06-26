/// A reference to a position in the source file.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Src {
    pub offset: usize,
    pub len: usize,
}

impl std::ops::Add for Src {
    type Output = Self;

    /// The sum of two Src's is the smallest Src covering both.
    fn add(self, other: Self) -> Self {
        if self.offset < other.offset {
            Src {
                offset: self.offset,
                len: other.offset - self.offset + other.len,
            }
        } else {
            Src {
                offset: other.offset,
                len: self.offset - other.offset + self.len,
            }
        }
    }
}
