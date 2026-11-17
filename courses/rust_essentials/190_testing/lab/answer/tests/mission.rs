use lab::mission_status;

#[test]
fn launch_decision() {
    assert_eq!(mission_status(19), "WAIT");
    assert_eq!(mission_status(20), "GO");
}
