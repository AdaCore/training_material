//! Lab (answer)
//! Tuples and Arrays
//!
//! Fix all the compile errors below and/or follow the hints provided
//!

#![allow(unused_assignments)]


fn main() {
    // TASK 1 - Array Declaration, Intialization and Value Change
    // Hint: First declare type and number of elements of your array, then assign it valid initial values.
    // Then change only one element using the proper syntax
    let mut numbers_arr: [i8; 3] = [1, 2, 3];
    numbers_arr[1] = 5 ;
    

    // TASK 2 - Out of Bounds
    // Hint: Array indexes start at 0
    numbers_arr[2] = 36;


    // TASK 3 - Iteration    
    // Hint: Use iteration by index and then by value
    let mut primes = [2, 3, 5, 7, 11, 13, 17];
    for ii in 0..primes.len() {
        primes[ii] = primes[ii] + 1 ;
    }
    for prime in primes {
        println!("{}", prime);
    }


    // TASK 4 - Tuple Declaration
    // Hint:Tuple declaration syntax differs from arrays. Access to value is done through "." dot notation
    let alien_report: (i8, bool) = (3, false);
    println!("Is it hostile? {}", alien_report.1);
    

    // TASK 5 - Destructuring
    // Hint: Ignore specific elements that are not needed With the wildcard pattern.
    // Use the variables created to display themm
    let person_data = ("Renoir", 33, "Painter");
    let (name, _, profession) = person_data;
    println!("Name: {name}, is a {profession}");

    // TASK 6 - Destructuring Assignment
    // Hint: Target variables have to be mutable
    let mut cat_snacks = 1;
    let mut dog_treats = 42;
    (cat_snacks, dog_treats) = (dog_treats, cat_snacks);


    // TASK 7 - Destructuring Array
    // Hint: destructure the array to print the number of shirts, pants and socks
    let bag: [i32; 3] = [10, 20, 30];
    let [shirts, pants, socks] = bag;
    println!("shirts: {}", shirts);
    println!("pants: {}", pants);
    println!("socks: {}", socks);

     
    // TASK 8 - Ignoring multiple elements
    // Hint: you can ignore multiple elements in destructuring using ".."
    let data = [1, 2, 3, 4, 5, 6];
    let [first, second, ..] = data;
    println!("First: {}", first);
    println!("Second: {}", second);
}