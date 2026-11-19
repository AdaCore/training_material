//! Lab (prompt)
//! Memory and Runtime Constraints
//!
//! Main program including incomplete testing function
//!

mod caller;
mod dynamic;
mod fixed;
mod logger_trait;
mod sample_data;

// Import structure implementations of
//   caller-supplied, fixed-size, and dynamic buffers.
// Also need to import the global data module

fn common_test<T: logger_trait::Logger>(logger: &mut T, can_exceed: bool) {
    // for every value in VALUES {
    //     // All buffers should have enough space
    //     assert!(logger.add_sample(value), "Should not get here");
    // }

    // We want to ensure we've limited number of values
    assert(!logger.add_sample(1234));
    println!("Exceeded capacity")

    // Check number of values added
    let count = logger.count();
    assert_eq!(count, MAX_SAMPLES);

    // Check average of values
    let average = logger.average().unwrap();
    assert_eq!(average, AVERAGE, "Actual average: {average}");

    // Print results
    println!("Average {average} over {count} values");

    // Verify clearing the buffer reduces the
    // count and no average exists
    logger.clear();
    assert_eq!(logger.count(), 0);
    assert_eq!(logger.average(), None);

    let capacity = logger.capacity();
    println!("Final capacity {capacity}");
    // Capacity should be exact for Fixed and Caller,
    // but may be greater for Dynamic
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
