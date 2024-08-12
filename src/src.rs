/// A reference to a position in the source file.
#[derive(Default, Clone, Copy, Debug, PartialEq, Eq)]
pub struct Src {
    pub offset: usize,
    pub len: usize,
}

impl std::ops::Add for Src {
    type Output = Self;

    /// The sum of two Src's is the smallest Src covering both.
    fn add(self, other: Self) -> Self {
        let start_offset = if self.offset < other.offset {
            self.offset
        } else {
            other.offset
        };
        let end_offset = if self.offset + self.len < other.offset + other.len {
            other.offset + other.len
        } else {
            self.offset + self.len
        };
        Src {
            offset: start_offset,
            len: end_offset - start_offset,
        }
    }
}

impl std::ops::AddAssign for Src {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}
