#[no_mangle]
pub extern "C" fn swap(x: &mut i32, y: &mut i32) {
    let tmp = *x;
    *x = *y;
    *y = tmp;
}
