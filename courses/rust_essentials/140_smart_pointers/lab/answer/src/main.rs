use std::rc::Rc;

#[allow(dead_code)]
enum Doll {
    Inside(Box<Doll>),
    Empty,
}

fn main() {
    let my_box = Box::new(5);
    println!("Box value is {}", *my_box);

    let _a_doll = Doll::Inside(Box::new(Doll::Empty));
    let _last_doll = Doll::Empty;

    fn say_hello(name: i32) {
        println!("Hello, 00{name}!");
    }

    let agent = Box::new(7_i32);
    say_hello(*agent);

    fn hello_again(name: &str) {
        println!("Hello, {name}!");
    }

    let my_box = Box::new(String::from("Rust"));
    hello_again(&my_box);

    let mut my_box = Box::new(0);
    *my_box = 10;

    fn hello(name: &str) {
        println!("Hello, {name}!");
    }

    fn edit(name: &mut str) {
        println!("Edited: {name}!");
    }

    let my_box = Box::new(String::from("Rust"));
    hello(&my_box);

    let mut my_box2 = Box::new(String::from("Rust"));
    edit(&mut my_box2);
    hello(&mut my_box2);

    let var_a = Rc::new(5);
    println!("Count: {}", Rc::strong_count(&var_a));
    let var_b = Rc::clone(&var_a);
    println!("Count: {}", Rc::strong_count(&var_a));
    println!("var_a = {}", var_a);
    println!("var_b = {}", var_b);

    let tic = Rc::new(5);
    let tac = Rc::clone(&tic);
    let toe = Rc::clone(&tic);
    println!("tic = {}", tic);
    println!("tac = {}", tac);
    println!("toe = {}", toe);
}
