//! Lab (answer)
//! Smart Pointers

#![allow(dead_code)]
#![allow(unused_variables)]

// TASK 1 - Bring Rc<T> into scope
use std::rc::Rc;

// A recursive type needs indirection so its size is known at compile time.
enum Doll {
    Inside(Box<Doll>),
    Empty,
}

fn main() {
    // TASK 2 - Create the value "5" on the heap
    // Box::new allocates the value on the heap

    let my_box = Box::new(5);

    // TASK 3 - Print the contents of my_box
    // Dereference the Box to access the inner i32 value
    println!("Box value is {}", *my_box);

    // Recursion

    // TASK 4 - Create the two Doll values below
    // Inside holds a boxed Doll; Empty is the base case
    
    let _a_doll = Doll::Inside(Box::new(Doll::Empty));
    let _last_doll = Doll::Empty;

    // Dereferencing
    // TASK 5 - Pass the inner value to say_hello
    // Hint: say_hello expects an i32, not a Box<i32>

    fn say_hello(name: i32) {
        println!("Hello, 00{name}!");
    }

    let agent = Box::new(7_i32);

    say_hello(*agent);

    // Coercing
    // TASK 6 - Call hello_again using my_box
    // Hint: Start by borrowing my_box

    fn hello_again(name: &str) {
        println!("Hello, {name}!");
    }

    let my_box = Box::new(String::from("Rust"));
    
    hello_again(&my_box);

    // Mutability
    // TASK 7 - Change the boxed value to 10
    // Hint: The Box is mutable, but you still need to reach the inner value

    let mut my_box = Box::new(0);

    *my_box = 10;

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

    hello(&my_box);

    let mut my_box2 = Box::new(String::from("Rust"));
    // edit needs mutable access all the way through
    edit(&mut my_box2);
    // A mutable reference can also be used where shared access is enough
    hello(&mut my_box2);

    // Counting References
    // TASK 9 - demonstrate shared ownership on the heap
    // Create the value "5" on the heap

    let var_a = Rc::new(5);
    println!("Count: {}", Rc::strong_count(&var_a));

    if Rc::strong_count(&var_a) != 1 {
        unreachable!("Wait, the initial count should be 1!");
    }

    // Make a shallow copy of var_a
    let var_b = Rc::clone(&var_a);
    println!("Count: {}", Rc::strong_count(&var_a));

    if Rc::strong_count(&var_a) != 2 {
        unreachable!("The internal counter did not increment!");
    }

    println!("var_a = {}", var_a);
    println!("var_b = {}", var_b);
}
