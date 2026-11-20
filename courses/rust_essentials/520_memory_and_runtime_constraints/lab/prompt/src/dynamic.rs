//! Lab (prompt)
//! Memory and Runtime Constraints
//!
//! Prompt for dynamic memory model
//!

use crate::logger_trait::Logger;

pub struct Dynamic {
}

impl Dynamic {
    pub fn new() -> Self {
        todo!()
    }
}

impl Logger for Dynamic {
    // Return false if the logger is full
    // (exceeds MAX_SAMPLES)
    fn add_sample(&mut self, value: usize) -> bool {
        false
    }

    // Return None if no samples have been collected
    // Otherwise return average of samples
    fn average(&self) -> Option<usize> {
        None
    }

    // Clear the logger so its storage can be reused
    fn clear(&mut self) {
    }

    // Number of items in use
    fn count(&self) -> usize {
        0
    }

    // Total capacity
    // (May exceed MAX_SAMPLES)
    fn capacity(&self) -> usize {
        0
    }
}
