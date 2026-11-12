use crate::logger_trait::Logger;

pub struct Caller<'a> {
    samples: &'a mut [usize],
    count: usize,
}

impl<'a> Caller<'a> {
    pub fn new(buffer: &'a mut [usize]) -> Self {
        Self {
            samples: buffer,
            count: 0,
        }
    }
}

impl Logger for Caller<'_> {
    fn add_sample(&mut self, value: usize) -> bool {
        false
    }

    fn average(&self) -> Option<usize> {
        None
    }

    fn clear(&mut self) {
    }

    fn count(&self) -> usize {
        0
    }

    fn capacity(&self) -> usize {
        0
    }
}
