// Lab (prompt)
//  Tuples and Arrays

//! Fix all the compile errors below and/or follow the hints provided

#[allow(dead_code)]
#[allow(unused_variables)]


fn main() {
    // TASK 1 - Array Declaration, Intialization and Value Change
    // Hint: First declare type and number of elements of your array, then assign it va;od initial values.
    // Then change only one element using the proper syntax
    let mut numbers_arr: [i8; 1, 2, 3];
    numbers_arr(1) = 5;
    

    // TASK 2 - Out of Bounds
    // Hint: Array indexes start at 0
    let mut bools = [true, true, false];
    bools[3] = true;


    // TASK 3 - Iteration    
    // Hint: Use iteration by index and then by value
    let mut primes = [2, 3, 5, 7, 11, 13, 17];
    for index in 0..primes.len() {
        // Increment elements of 'primes' by 1 using the 'index'
    }
    for prime in X {
        println!("{}", prime);
    }


    // TASK 4 - Tuple Declaration
    // Hint:Tuple declaration syntax differs from arrays. Access to value is done through "." dot notation
    let alien_report: (i8; bool) = [3, false];
    println!("Is it hostile? {}", alien_report);
    

    // TASK 5 - Destructuring
    // Hint: Ignore specific elements that are not needed With the wildcard pattern.
    // Use the variables created to display them
    let person_data = ("Renoir", 33, "Painter");
    let (name, profession) = person_data;
    println!("Name: {person_data}, is a {person_data}");

    // TASK 6 - Destructuring Assignment
    // Hint: Target variables have to be mutable
    let cat_snacks = 1;
    let dog_treats = 42;
    (cat_snacks, dog_treats) = (dog_treats, cat_snacks);


    // TASK 7 - Destructuring Array
    // Hint: destructure the array to print the number of shirts, pants and socks
    let bag: [i32; 3] = [10, 20, 30];
    println!("shirts: {}", shirts);
    println!("pants: {}", pants);
    println!("socks: {}", socks);

     
    // TASK 8 - Ignoring multiple elements
    // Hint: you can ignore multiple elements in destructuring using ".."
    let data = [1, 2, 3, 4, 5, 6];
    // Get the first two elements, ignore the rest
    println!("First: {}", first);
    println!("Second: {}", second);
}
