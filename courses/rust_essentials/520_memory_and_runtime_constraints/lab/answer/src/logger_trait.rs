//! Lab (answer)
//! Memory and Runtime Constraints
//!
//! Definition of "Logger" trait
//!

pub trait Logger {
    // Return 'false' when no more samples can be accepted
    fn add_sample(&mut self, value: usize) -> bool;

    // Return the average, or None if no values in buffer
    fn average(&self) -> Option<usize>;

    // Empty the buffer
    fn clear(&mut self);

    // Return number of items in the buffer
    fn count(&self) -> usize;

    // Return the capacity of the buffer
    fn capacity(&self) -> usize;
}
