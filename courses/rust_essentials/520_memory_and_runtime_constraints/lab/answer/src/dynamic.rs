use crate::logger_trait::Logger;

pub struct Dynamic {
    samples: Vec<u16>,
}

impl Dynamic {
    pub fn new() -> Self {
        Self {
            samples: Vec::new(),
        }
    }
}

const MAX_SAMPLES: usize = 8;

impl Logger for Dynamic {
    // Return false if the logger is full.
    fn add_sample(&mut self, value: u16) -> bool {
        if self.samples.len() >= MAX_SAMPLES {
            return false;
        }

        self.samples.push(value);
        true
    }

    // Return None if no samples have been collected.
    fn average(&self) -> Option<u16> {
        if self.samples.is_empty() {
            return None;
        }

        let sum: u32 = self.samples.iter().map(|&value| value as u32).sum();

        Some((sum / self.samples.len() as u32) as u16)
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
