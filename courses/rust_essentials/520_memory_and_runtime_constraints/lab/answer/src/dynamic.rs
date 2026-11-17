use crate::logger_trait::Logger;
use crate::static_data::MAX_SAMPLES;

pub struct Dynamic {
    samples: Vec<usize>,
}

impl Dynamic {
    pub fn new() -> Self {
        Self {
            samples: Vec::new(),
        }
    }
}

impl Logger for Dynamic {
    // Return false if the logger is full.
    fn add_sample(&mut self, value: usize) -> bool {
        if self.samples.len() >= MAX_SAMPLES {
            return false;
        }

        self.samples.push(value);
        true
    }

    // Return None if no samples have been collected.
    fn average(&self) -> Option<usize> {
        if self.samples.is_empty() {
            return None;
        }
        let sum: usize = self.samples.iter().copied().sum();
        Some(sum / self.samples.len())
    }

    // Clear the logger so its storage can be reused.
    fn clear(&mut self) {
        self.samples.clear();
    }

    // Number of items in use
    fn count(&self) -> usize {
        self.samples.len()
    }

    // Total capacity
    fn capacity(&self) -> usize {
        self.samples.capacity()
    }
}
