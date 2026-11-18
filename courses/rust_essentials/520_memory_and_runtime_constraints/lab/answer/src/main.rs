mod caller;
mod dynamic;
mod fixed;
mod logger_trait;
mod sample_data;

use caller::Caller;
use dynamic::Dynamic;
use fixed::Fixed;
use sample_data::*;

fn common_test<T: logger_trait::Logger>(logger: &mut T, can_exceed: bool) {
    for value in VALUES {
        if !logger.add_sample(value) {
            println!("Should not get here")
        }
    }
    if !logger.add_sample(1234) {
        println!("Exceeded capacity")
    }

    let count = logger.count();
    assert_eq!(count, MAX_SAMPLES);

    let average = logger.average().unwrap();
    assert_eq!(average, AVERAGE, "Actual average: {average}");

    println!("Average {average} over {count} values");

    logger.clear();
    assert_eq!(logger.count(), 0);
    let capacity = logger.capacity();
    println!("Final capacity {capacity}");
    if can_exceed {
        assert!(capacity >= MAX_SAMPLES, "Actual capacity: {capacity}");
    } else {
        assert_eq!(capacity, MAX_SAMPLES, "Actual capacity: {capacity}");
    }
}

fn main() {
    println!("Testing a fixed buffer");
    let mut fixed = Fixed::new();
    common_test(&mut fixed, false);
    println!("");

    println!("Testing a caller-supplied buffer");
    let mut buffer = [0usize; MAX_SAMPLES];
    let mut caller = Caller::new(&mut buffer);
    common_test(&mut caller, false);
    println!("");

    println!("Testing a dynamic buffer");
    let mut dynamic = Dynamic::new();
    common_test(&mut dynamic, true);
}
