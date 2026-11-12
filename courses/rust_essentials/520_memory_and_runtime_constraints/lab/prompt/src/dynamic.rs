use crate::logger_trait::Logger;

pub struct Dynamic {
}

impl Dynamic {
    pub fn new() -> Self {
        todo!()
    }
}

impl Logger for Dynamic {
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
