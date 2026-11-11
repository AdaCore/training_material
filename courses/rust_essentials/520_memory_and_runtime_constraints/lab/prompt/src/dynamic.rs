use crate::logger_trait::Logger;

// Dynamic will log numeric sensor readings into a "Vec"
// The structure needs to store the vector and any
// information necessary to keep track of how many
// readings are valid.
pub struct Dynamic {
    samples: Vec<u16>,
}

impl Dynamic {
    pub fn new() -> Self {
        todo!()
    }
}

const MAX_SAMPLES: usize = 8;

impl Logger for Dynamic {
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
