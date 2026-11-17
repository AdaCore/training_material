//! Mission Control - Testing Lab
//!
//! A rover needs at least 20% battery to launch

pub fn can_launch(battery: u8) -> bool {
    assert!(battery <= 100, "invalid battery level");
    battery > 20 // One mistake is hiding here
}

/// Returns "GO" when the rover can launch, otherwise "WAIT"
///
/// TASK 3: Add a doctest showing mission_status(50) == "GO"
pub fn mission_status(battery: u8) -> &'static str {
    if can_launch(battery) {
        "GO"
    } else {
        "WAIT"
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // TASK 1: Write two unit tests
    // - Check that 19% is too low, but 20% is sufficient
    // - Check that 101% panics with "invalid battery level"
}
