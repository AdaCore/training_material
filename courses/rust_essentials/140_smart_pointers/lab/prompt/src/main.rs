//! Lab (prompt)
//! Smart Pointers
//!
//! Fix all the compile errors below by following the hints provided
//!

#![allow(dead_code)]
#![allow(unused_variables)]

// TASK 1 - Bring Rc<T> into scope
// Hint: Rc lives in std::rc

enum Doll {
    Inside(Box<Doll>),
    Empty,
}

fn main() {
    // TASK 2 - Create the value "5" on the heap
    // Hint: Replace ??? (remember the "Box" mechanism)

    let my_box = ???;

    // TASK 3 - Print the contents of my_box
    // Hint: Replace ??? with the value inside my_box
    
    println!("Box value is {}", ???);

    // Recursion
    // TASK 4 - Create the two Doll values below
    // Hint: One variant holds a boxed Doll; the other holds no data

    let _a_doll = ???;
    let _last_doll = ???;

    // Dereferencing
    // TASK 5 - Pass the inner value to say_hello
    // Hint: say_hello expects an i32, not a Box<i32>

    fn say_hello(name: i32) {
        println!("Hello, 00{name}!");
    }

    let agent = Box::new(7_i32);

    say_hello(???);

    // Coercing
    // TASK 6 - Call hello_again using my_box
    // Hint: Start by borrowing my_box

    fn hello_again(name: &str) {
        println!("Hello, {name}!");
    }

    let my_box = Box::new(String::from("Rust"));

    hello_again(???);

    // Mutability
    // TASK 7 - Change the boxed value to 10
    // Hint: The Box is mutable, but you still need to reach the inner value

    let mut my_box = Box::new(0);

    ??? = 10;

    // Mutability and Coercion
    // TASK 8 - Make these calls compile
    // Hint: hello only needs shared access, but edit needs mutable access all the way through

    fn hello(name: &str) {
        println!("Hello, {name}!");
    }

    fn edit(name: &mut str) {
        println!("Edited: {name}!");
    }

    let my_box = Box::new(String::from("Rust"));

    hello(???);

    let mut my_box2 = Box::new(String::from("Rust"));
    edit(???);
    hello(???);

    // Counting References

    // TASK 9 - demonstrate shared ownership on the heap    
    // Both 'var_a' and 'var_b' share ownership of the value

    // Create the value "5" on the heap
    // Hint: Replace ??? (what allows shared ownership??)
    let var_a = ???
    println!("Count: {}", Rc::strong_count(&var_a));

    // Ensure the count is what we expect before proceeding
    if Rc::strong_count(&var_a) != 1 {
        unreachable!("Wait, the initial count should be 1!");
    }

    // Make a copy of var_a
    // Hint: Replace ??? (what creates a shallow copy?)
    let var_b = ???
    println!("Count: {}", Rc::strong_count(&var_a));

    // After a clone, the count should have incremented
    if Rc::strong_count(&var_a) != 2 {
        unreachable!("The internal counter did not increment!");
    }

    println!("var_a = {}", var_a);
    println!("var_b = {}", var_b);
}
