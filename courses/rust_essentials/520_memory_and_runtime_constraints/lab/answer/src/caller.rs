use crate::logger_trait::Logger;

pub struct Caller<'a> {
    samples: &'a mut [u16],
    count: usize,
}

impl<'a> Caller<'a> {
    pub fn new(buffer: &'a mut [u16]) -> Self {
        Self {
            samples: buffer,
            count: 0,
        }
    }
}

impl Logger for Caller<'_> {
    fn add_sample(&mut self, value: u16) -> bool {
        if self.count >= self.samples.len() {
            false
        } else {
            self.samples[self.count] = value;
            self.count += 1;
            true
        }
    }

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

    fn clear(&mut self) {
        self.count = 0;
    }

    fn count(&self) -> usize {
        self.count
    }

    fn capacity(&self) -> usize {
        self.samples.len()
    }
}
