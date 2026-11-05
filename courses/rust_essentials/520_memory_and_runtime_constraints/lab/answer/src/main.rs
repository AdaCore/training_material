mod caller;
mod dynamic;
mod fixed;

fn main() {
    let mut logger = fixed::SensorLogger::new();
    for index in 0..fixed::MAX_SAMPLES as u16 {
        if !logger.add_sample(index) {
            println!("Could not add sample {index}")
        } else {
            let cnt = logger.count();
            let cap = logger.capacity();
            println!("{index} => length = {cnt}   capacity = {cap}")
        }
    }
    match logger.average() {
        Some(value) => println!("Average value = {value}"),
        None => println!("No values found"),
    }
    logger.clear();
    let cnt = logger.count();
    let cap = logger.capacity();
    println!("After clear => length = {cnt}   capacity = {cap}");

    let mut logger = dynamic::SensorLogger::new();
    for index in 0..dynamic::MAX_SAMPLES as u16 {
        if !logger.add_sample(index) {
            println!("Could not add sample {index}")
        } else {
            let cnt = logger.count();
            let cap = logger.capacity();
            println!("{index} => length = {cnt}   capacity = {cap}")
        }
    }
    match logger.average() {
        Some(value) => println!("Average value = {value}"),
        None => println!("No values found"),
    }
    logger.clear();
    let cnt = logger.count();
    let cap = logger.capacity();
    println!("After clear => length = {cnt}   capacity = {cap}");

    const MAX_SAMPLES: usize = 8;

    let mut logger = [0_u16; MAX_SAMPLES];
    let mut count = 0;
    for index in 0..MAX_SAMPLES as u16 {
        if !caller::add_sample(&mut logger, &mut count, index) {
            println!("Could not add sample {index}")
        } else {
            let cap = logger.len();
            println!("{index} => length = {count}   capacity = {cap}")
        }
    }
    match caller::average(&logger, count) {
        Some(value) => println!("Average value = {value}"),
        None => println!("No values found"),
    }
    caller::clear(&mut count);
    let cap = logger.len();
    println!("After clear => length = {count}   capacity = {cap}");
}
