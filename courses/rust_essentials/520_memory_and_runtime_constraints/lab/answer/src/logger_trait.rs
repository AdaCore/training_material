pub trait Logger {
    fn add_sample(&mut self, value: u16) -> bool;
    fn average(&self) -> Option<u16>;
    fn clear(&mut self);
    fn count(&self) -> usize;
    fn capacity(&self) -> usize;
}
