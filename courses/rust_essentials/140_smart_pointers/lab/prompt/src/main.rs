// TASK 1 - Bring Rc<T> into scope
// Hint: Rc lives in std::rc

#[allow(dead_code)]
enum Doll {
    Inside(Box<Doll>),
    Empty,
}

fn main() {
    // TASK 2 - Define a new Box with a value of 5
    // Hint: Replace ??? with something starting with Box::
    let my_box = ???;

    // TASK 3 - Print the contents of my_box
    // Hint: Replace ??? with the value inside my_box
    println!("Box value is {}", ???);

    // Recursion

    // TASK 4 - Create the two Doll values below
    // Hint: One variant holds a boxed Doll; the other holds no data
    let _a_doll = ???;
    let _last_doll = ???;

    fn say_hello(name: i32) {
        println!("Hello, 00{name}!");
    }

    let agent = Box::new(7_i32);

    // TASK 5 - Pass the inner value to say_hello
    // Hint: say_hello expects an i32, not a Box<i32>
    say_hello(???);

    // Coercing
    fn hello_again(name: &str) {
        println!("Hello, {name}!");
    }

    let my_box = Box::new(String::from("Rust"));

    // TASK 6 - Call hello_again using my_box
    // Hint: Start by borrowing my_box
    hello_again(???);

    // Mutability
    let mut my_box = Box::new(0);

    // TASK 7 - Change the boxed value to 10
    // Hint: The Box is mutable, but you still need to reach the inner value
    ??? = 10;

    // Mutability and Coercion
    fn hello(name: &str) {
        println!("Hello, {name}!");
    }

    fn edit(name: &mut str) {
        println!("Edited: {name}!");
    }

    let my_box = Box::new(String::from("Rust"));

    // TASK 8 - Make these calls compile
    // Hint: hello only needs shared access, but edit needs mutable access all the way through
    hello(???);

    let mut my_box2 = Box::new(String::from("Rust"));
    edit(???);
    hello(???);

    // Counting References
    let var_a = Rc::new(5);
    println!("Count: {}", Rc::strong_count(&var_a));
    let var_b = Rc::clone(&var_a);
    println!("Count: {}", Rc::strong_count(&var_a));

    // TASK 9 - Use tic, tac, and toe in a simple way that demonstrates shared ownership
    // Hint: All three variables refer to the same underlying value
    let tic = Rc::new(5);
    let tac = Rc::clone(&tic);
    let toe = Rc::clone(&tic);
}
