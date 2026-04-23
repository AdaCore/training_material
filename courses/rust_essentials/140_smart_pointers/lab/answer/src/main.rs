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

    fn say_hello(name: i32) {
        println!("Hello, 00{name}!");
    }

    let agent = Box::new(7_i32);

    // TASK 5 - Pass the inner value to say_hello
    // say_hello expects an i32, so dereference the Box
    say_hello(*agent);

    // Coercing
    fn hello_again(name: &str) {
        println!("Hello, {name}!");
    }

    let my_box = Box::new(String::from("Rust"));

    // TASK 6 - Call hello_again using my_box
    // Borrowing the Box<String> allows deref coercion to &str
    hello_again(&my_box);

    // Mutability
    let mut my_box = Box::new(0);

    // TASK 7 - Change the boxed value to 10
    // Dereference the Box to update the inner value
    *my_box = 10;

    // Mutability and Coercion
    fn hello(name: &str) {
        println!("Hello, {name}!");
    }

    fn edit(name: &mut str) {
        println!("Edited: {name}!");
    }

    let my_box = Box::new(String::from("Rust"));

    // TASK 8 - Make these calls compile
    // hello only needs shared access.
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

    // TASK 10 - Use tic, tac, and toe in a simple way that demonstrates shared ownership
    // All three Rc values refer to the same underlying data
    let tic = Rc::new(5);
    let tac = Rc::clone(&tic);
    let toe = Rc::clone(&tic);

    println!("tic = {}", tic);
    println!("tac = {}", tac);
    println!("toe = {}", toe);
}
