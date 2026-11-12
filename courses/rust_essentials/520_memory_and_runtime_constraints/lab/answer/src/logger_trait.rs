pub trait Logger {
    fn add_sample(&mut self, value: usize) -> bool;
    fn average(&self) -> Option<usize>;
    fn clear(&mut self);
    fn count(&self) -> usize;
    fn capacity(&self) -> usize;
}
