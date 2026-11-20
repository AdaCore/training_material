//! Lab (answer)
//! Memory and Runtime Constraints
//!
//! Answer for caller-supplied memory model
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
        if self.count >= self.samples.len() {
            false
        } else {
            self.samples[self.count] = value;
            self.count += 1;
            true
        }
    }

    // Return None if no samples have been collected.
    // Otherwise return average of samples.
    fn average(&self) -> Option<usize> {
        if self.count == 0 {
            None
        } else {
            let mut total = 0;
            for index in 0..self.count {
                total = total + self.samples[index];
            }
            Some(total / self.count as usize)
        }
    }

    // Clear the logger so its storage can be reused.
    fn clear(&mut self) {
        self.count = 0;
    }

    // Number of items in use
    fn count(&self) -> usize {
        self.count
    }

    // Total capacity
    fn capacity(&self) -> usize {
        self.samples.len()
    }
}
