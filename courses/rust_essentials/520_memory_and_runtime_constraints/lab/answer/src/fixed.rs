use crate::logger_trait::Logger;

pub const MAX_SAMPLES: usize = 8;

pub struct Fixed {
    samples: [u16; MAX_SAMPLES],
    count: usize,
}

impl Fixed {
    pub fn new() -> Self {
        Self {
            samples: [0; MAX_SAMPLES],
            count: 0,
        }
    }
}

impl Logger for Fixed {
    // Return false if the logger is full.
    fn add_sample(&mut self, value: u16) -> bool {
        if self.count >= MAX_SAMPLES {
            return false;
        }

        self.samples[self.count] = value;
        self.count += 1;
        true
    }

    // Return None if no samples have been collected.
    fn average(&self) -> Option<u16> {
        if self.count == 0 {
            None
        } else {
            let mut total = 0;
            for index in 0..self.count {
                total = total + self.samples[index];
            }
            Some(total / self.count as u16)
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
