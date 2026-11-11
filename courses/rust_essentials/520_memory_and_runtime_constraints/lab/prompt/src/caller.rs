use crate::logger_trait::Logger;

// Caller will log numeric sensor readings into a
// buffer supplied by the user.
// The structure needs to store a reference to the
// buffer and any information necessary to keep track
// of how many readings are valid.
pub struct Caller<'a> {
    samples: &'a mut [u16],
    count: usize,
}

impl<'a> Caller<'a> {
    pub fn new(buffer: &'a mut [u16]) -> Self {
        Self {
            samples: buffer,
            count: 0,
        }
    }
}

impl Logger for Caller<'_> {
    // Return false if the logger is full.
    fn add_sample(&mut self, value: u16) -> bool {
        todo!()
    }

    // Return None if no samples have been collected.
    fn average(&self) -> Option<u16> {
        todo!()
    }

    // Clear the logger so its storage can be reused.
    fn clear(&mut self) {
        todo!()
    }

    // Number of items in use
    fn count(&self) -> usize {
        todo!()
    }

    // Total capacity
    fn capacity(&self) -> usize {
        todo!()
    }
}
