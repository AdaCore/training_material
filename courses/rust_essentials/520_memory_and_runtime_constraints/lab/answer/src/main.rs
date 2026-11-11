mod caller;
mod dynamic;
mod fixed;
mod logger_trait;

use caller::Caller;
use dynamic::Dynamic;
use fixed::Fixed;

fn common_test<T: logger_trait::Logger>(logger: &mut T) {
    for index in 0..9_u16 {
        if !logger.add_sample(index) {
            println!("Could not add sample {index}")
        }
    }

    let cnt = logger.count();
    assert_eq!(cnt, 8);

    let avg = logger.average().unwrap();
    assert_eq!(avg, 3);

    logger.clear();
    assert_eq!(logger.count(), 0);
    assert_eq!(logger.capacity(), 8);

    println!("Average {avg} over {cnt} values");
}

fn main() {
    println!("Testing a fixed buffer");
    let mut fixed = Fixed::new();
    common_test(&mut fixed);
    println!("");

    println!("Testing a dynamic buffer");
    let mut dynamic = Dynamic::new();
    common_test(&mut dynamic);
    println!("");

    println!("Testing a caller-supplied buffer");
    let mut buffer = [0u16; 8];
    let mut caller = Caller::new(&mut buffer);
    common_test(&mut caller);
}
