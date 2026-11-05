pub const MAX_SAMPLES: usize = 8;

pub struct SensorLogger {
    samples: Vec<u16>,
}

impl SensorLogger {
    pub fn new() -> Self {
        Self {
            samples: Vec::new(),
        }
    }

    // Return false if the logger is full.
    pub fn add_sample(&mut self, value: u16) -> bool {
        if self.samples.len() >= MAX_SAMPLES {
            return false;
        }

        self.samples.push(value);
        true
    }

    // Return None if no samples have been collected.
    pub fn average(&self) -> Option<u16> {
        if self.samples.is_empty() {
            return None;
        }

        let sum: u32 = self.samples.iter().map(|&value| value as u32).sum();

        Some((sum / self.samples.len() as u32) as u16)
    }

    // Clear the logger so its storage can be reused.
    pub fn clear(&mut self) {
        self.samples.clear();
    }

    // Number of items in use
    pub fn count(&self) -> usize {
        self.samples.len()
    }

    // Total capacity
    pub fn capacity(&self) -> usize {
        self.samples.capacity()
    }
}
