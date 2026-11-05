pub fn add_sample(samples: &mut [u16], count: &mut usize, value: u16) -> bool {
    if *count >= samples.len() {
        return false;
    }

    samples[*count] = value;
    *count += 1;
    true
}

pub fn average(samples: &[u16], count: usize) -> Option<u16> {
    if count == 0 {
        return None;
    }

    let sum: u32 = samples[..count].iter().map(|&value| value as u32).sum();

    Some((sum / count as u32) as u16)
}

pub fn clear(count: &mut usize) {
    *count = 0;
}
