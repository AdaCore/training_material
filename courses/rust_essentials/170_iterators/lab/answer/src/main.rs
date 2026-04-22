// Lab (prompt)
//  Iterators

//! Fix all the compile errors below and/or follow the hints provided

#[allow(dead_code)]
#[allow(unused_variables)]


fn main() {
    // TASK 1 - What is an Iterator?
    // Hint: Print ALL values. Check the output to verify you printed all elements  
    let array = [2, 4, 6, 8];
    for idx in 1..4 {
        println!("{}", array[idx]);
    }
    

    // TASK 2 - Using Our Iterator
    // Hint: The 'for' loop calls .next() under the hood
    struct SliceIter<'s> {
        slice: &'s [i32],
        idx: usize,
    }
    impl<'s> Iterator for SliceIter<'s> {
        type Item = &'s i32;
        fn next(&mut self) -> Option<Self::Item> {
            if self.idx == self.slice.len() {
                None
            } else {
                let next = &self.slice[self.idx];
                self.idx += 1;
                Some(next)
            }
        }
    }
    let mut numbers = [10, 20, 30];
    let mut iter = SliceIter {
        slice: &numbers,
        idx: 0,
    };
    for my_iter in iter {
        println!("The number is: {}", my_iter);
    }

    // TASK 3 - Getting an Iterator - 'iter()'
    // Hint: Collections themselves are not iterators
    let numbers2 = vec![10, 20, 30];
        for n in numbers2 {
        println!("{n}");
    }    
       

    // TASK 4 - Common Iterator Adapters
    // Hint: 'map' transforms values during iteration, 'filter' selects values matching condition
    fn double(x: &i32) -> i32 {
        x * 2
    }
    fn is_even(x: &&i32) -> bool {
        **x % 2 == 0
    }
    let flush = vec![10, 11, 12, 13, 14];

    // Multiply all values by 2
    for elem in flush.iter().map(double) {
        println!("Value squared is: {elem}");
    }

    // Print only values divisible by 2
    for elem in flush.iter().filter(is_even) {
        println!("This value is divisible by 2: {elem}");
    }
    

    // TASK 5 - Common consummers
    // Hint: 'sum' adds all values and return a single value, 'any' returns True if any value matches condition
    fn is_freezing(temp: &i32) -> bool {
        *temp < 0
    }
    let hand = [1, 2, 3, 4, 5];
    // Sum all values into 'Total'
    let total: i32 = hand.iter().sum();
    println!("The total is: {total}");
    
    let temperatures = [22, 28, -2, 15, 30];
    // Print the warning if any temperature is below 0
    if temperatures.iter().any(is_freezing){
        println!("Warning: Freezing temperatures detected!");
    } else {
        println!("All temperatures are above freezing.");
    }
    
    // TASK 6 - Chaining
    // Hint: Chaining allows you to create a new set of data before consuming
    fn is_even_owned(x: &i32) -> bool {
        *x % 2 == 0
    }
    fn square (x: i32) -> i32 {
        x * x
    }
    let result: i32 = (1..=10) // Range: 1, 2, 3, ..., 10
        .filter(is_even_owned)                  // Only select even values
        .map(square)                     // Square values
        .sum(); // Total: 220
    println!("Sum of even squares: {}", result);
        

    // TASK 7 - Collecting "Result"
    // Hint: 'collect()' stores results in a collection
    // For any(): receives a reference to the item


fn parse_i32(s: &str) -> Result<i32, std::num::ParseIntError> {
    s.parse::<i32>()
}
    
    let good_strings = vec!["1", "2", "42"];
    let good_numbers: Result<Vec<i32>, _> = good_strings
        .into_iter()
        .map(parse_i32)
        .collect();
    println!("good_numbers: {:?}", good_numbers);
    
}
