/// Return the offset of needle in haystack, or None if needle is not a subslice of haystack.
///
/// This only uses pointer arithmetic, not string search.
pub fn str_offset_in(needle: &str, haystack: &str) -> Option<usize> {
    // Check that needle_addr is within or pointing to the byte after haystack.
    let haystack_addr = haystack.as_ptr() as usize;
    let needle_addr = needle.as_ptr() as usize;
    if needle_addr < haystack_addr || needle_addr > haystack_addr.wrapping_add(haystack.len()) {
        return None;
    }

    // SAFETY:
    //  - Both self and origin must be either in bounds or one byte past the end of the same allocated object.
    //    - Verified above.
    // - Both pointers must be derived from a pointer to the same object.
    //    - Verified that needle is within haystack, and thus the same object.
    // - The distance between the pointers, in bytes, must be an exact multiple of the size of T.
    //    - T is u8, so trivially true.
    // - The distance between the pointers, in bytes, cannot overflow an isize.
    //    - isize::MAX is the maximum size of an object
    // - The distance being in bounds cannot rely on “wrapping around” the address space.
    //    - Wrapping around would only occur if needle_addr < haystack_addr, and we verified that
    //      is not the case above.
    let offset = unsafe { needle.as_ptr().offset_from(haystack.as_ptr()) };

    // Cast to usize is safe: needle_addr < haystack_addr, so offset is positive.
    Some(offset as usize)
}

/// Return the minimal string from haystack containing both needles, or None if either
/// needle is not in haystack.
pub fn span_strs<'a>(needle1: &'a str, mut needle2: &'a str, haystack: &'a str) -> Option<&'a str> {
    let mut off1 = str_offset_in(needle1, haystack)?;
    let mut off2 = str_offset_in(needle2, haystack)?;

    if off1 > off2 {
        (off1, off2) = (off2, off1);
        needle2 = needle1;
    }

    Some(&haystack[off1..off2 + needle2.len()])
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn str_offset_in_not_in() {
        assert_eq!(str_offset_in("str", "a string"), None);
    }

    #[test]
    fn str_offset_prefix() {
        let s = "a string";
        assert_eq!(str_offset_in(&s[..3], s), Some(0));
    }

    #[test]
    fn str_offset_suffix() {
        let s = "a string";
        assert_eq!(str_offset_in(&s[3..], s), Some(3));
    }

    #[test]
    fn str_offset_mid() {
        let s = "a string";
        assert_eq!(str_offset_in(&s[2..4], s), Some(2));
    }

    #[derive(PartialEq, Eq)]
    struct PtrLen(*const u8, usize);

    impl std::fmt::Debug for PtrLen {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.debug_struct("PtrLen")
                .field("ptr", &self.0)
                .field("len", &self.1)
                // SAFETY: `ptrlen()` just constructed this from an &str.
                .field("as_str()", unsafe {
                    &std::str::from_utf8_unchecked(std::slice::from_raw_parts(self.0, self.1))
                })
                .finish()
        }
    }

    fn ptrlen(s: &str) -> PtrLen {
        PtrLen(s.as_ptr(), s.len())
    }

    #[test]
    fn span_strs_not_in() {
        let s = "a string";
        assert_eq!(span_strs("str", &s[1..2], s), None);
        assert_eq!(span_strs(&s[1..2], "str", s), None);
    }

    #[test]
    fn span_strs_disjoint_ordered() {
        let s = "a longer string";
        assert_eq!(
            span_strs(&s[1..2], &s[3..4], s).map(ptrlen),
            Some(ptrlen(&s[1..4]))
        );
    }

    #[test]
    fn span_strs_disjoint_reversed() {
        let s = "a longer string";
        assert_eq!(
            span_strs(&s[5..7], &s[2..4], s).map(ptrlen),
            Some(ptrlen(&s[2..7]))
        );
    }

    #[test]
    fn span_strs_adjoint_ordered() {
        let s = "a longer string";
        assert_eq!(
            span_strs(&s[1..3], &s[3..5], s).map(ptrlen),
            Some(ptrlen(&s[1..5]))
        );
    }

    #[test]
    fn span_strs_adjoint_reversed() {
        let s = "a longer string";
        assert_eq!(
            span_strs(&s[5..7], &s[2..5], s).map(ptrlen),
            Some(ptrlen(&s[2..7]))
        );
    }

    #[test]
    fn span_strs_overlap_ordered() {
        let s = "a longer string";
        assert_eq!(
            span_strs(&s[1..4], &s[3..5], s).map(ptrlen),
            Some(ptrlen(&s[1..5]))
        );
    }

    #[test]
    fn span_strs_overlap_reversed() {
        let s = "a longer string";
        assert_eq!(
            span_strs(&s[5..7], &s[2..6], s).map(ptrlen),
            Some(ptrlen(&s[2..7]))
        );
    }

    #[test]
    fn span_strs_same() {
        let s = "a longer string";
        assert_eq!(
            span_strs(&s[5..7], &s[5..7], s).map(ptrlen),
            Some(ptrlen(&s[5..7]))
        );
    }
}
