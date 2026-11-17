//! Mission Control - Testing Lab
//!
//! A rover needs at least 20% battery to launch

pub fn can_launch(battery: u8) -> bool {
    assert!(battery <= 100, "invalid battery level");
    battery >= 20
}

/// Returns "GO" when the rover can launch, otherwise "WAIT"
///
/// ```
/// use lab::mission_status;
/// assert_eq!(mission_status(50), "GO");
/// ```
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

    #[test]
    fn battery_boundary() {
        assert!(!can_launch(19));
        assert!(can_launch(20), "20% must be sufficient to launch");
    }

    #[test]
    #[should_panic(expected = "invalid battery level")]
    fn invalid_battery() {
        can_launch(101);
    }
}
