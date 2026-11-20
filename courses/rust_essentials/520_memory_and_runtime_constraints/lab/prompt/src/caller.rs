//! Lab (prompt)
//! Memory and Runtime Constraints
//!
//! Prompt for caller-supplied memory model
//!

use crate::logger_trait::Logger;

pub struct Caller<'a> {
    samples: &'a mut [usize],
    count: usize,
}

impl<'a> Caller<'a> {
    pub fn new(buffer: &'a mut [usize]) -> Self {
        Self {
            samples: buffer,
            count: 0,
        }
    }
}

impl Logger for Caller<'_> {
    // Return false if the logger is full.
    fn add_sample(&mut self, value: usize) -> bool {
        false
    }

    // Return None if no samples have been collected.
    // Otherwise return average of samples.
    fn average(&self) -> Option<usize> {
        None
    }

    // Clear the logger so its storage can be reused.
    fn clear(&mut self) {
    }

    // Number of items in use
    fn count(&self) -> usize {
        0
    }

    // Total capacity
    fn capacity(&self) -> usize {
        0
    }
}
