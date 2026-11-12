mod logger_trait;
// Add module references for implentations for
//   caller-supplied, fixed-size, and dynamic buffers.
// Also need to reference to the global data module

fn common_test<T: logger_trait::Logger>(logger: &mut T) {
    // for every value in VALUES {
    //     if !logger.add_sample(value) {
    //        All buffers should have enough space
    //        println!("Should not get here")
    //     }
    // }

    // We want to ensure we've limited number of values
    if !logger.add_sample(1234) {
        println!("Exceeded capacity")
    }

    // Check number of values added
    let count = logger.count();
    assert_eq!(count, MAX_SAMPLES);

    // Check average of values
    let average = logger.average().unwrap();
    assert_eq!(average, AVERAGE, "Actual average: {average}");

    // Print results
    println!("Average {average} over {count} values");

    // Verify clearing the buffer reduces the count
    logger.clear();
    assert_eq!(logger.count(), 0);

    // Check that the capacity matches the count
    // NOTE: THIS COULD FAIL IN YOUR DYNAMIC CASE!
    let capacity = logger.capacity();
    assert_eq!(capacity, MAX_SAMPLES, "Actual capacity: {capacity}");
}

fn main() {
    println!("Testing a fixed buffer");
    let mut fixed = Fixed::new();
    common_test(&mut fixed);
    println!("");

    println!("Testing a caller-supplied buffer");
    let mut buffer = [0usize; MAX_SAMPLES];
    let mut caller = Caller::new(&mut buffer);
    common_test(&mut caller);
    println!("");

    println!("Testing a dynamic buffer");
    let mut dynamic = Dynamic::new();
    common_test(&mut dynamic);
}
