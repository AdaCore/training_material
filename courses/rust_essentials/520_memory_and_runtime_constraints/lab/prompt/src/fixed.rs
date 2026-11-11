use crate::logger_trait::Logger;

pub const MAX_SAMPLES: usize = 8;

// Fixed will log numeric sensor readings into an array
// The structure needs to store the array and any
// information necessary to keep track of how many
// readings are valid.
pub struct Fixed {
    samples: [u16; MAX_SAMPLES],
    count: usize,
}

impl Fixed {
    // Return an initialized version of Fixed
    pub fn new() -> Self {
        todo!()
    }
}

impl Logger for Fixed {
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
