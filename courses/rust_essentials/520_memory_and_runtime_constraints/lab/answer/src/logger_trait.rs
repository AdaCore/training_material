pub trait Logger {
    // Return 'true' if the buffer is full
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
