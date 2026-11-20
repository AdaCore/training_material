use lab::mission_status;

#[test]
fn launch_decision() {
    for (battery, expected) in [
        (0, "WAIT"),
        (19, "WAIT"),
        (20, "GO"),
        (100, "GO"),
    ] {
        assert_eq!(
            mission_status(battery),
            expected,
            "battery: {battery}%"
        );
    }
}
