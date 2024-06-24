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
