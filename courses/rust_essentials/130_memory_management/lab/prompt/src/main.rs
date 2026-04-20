// Lab (prompt)
//  Memory Management

//! Fix all the compile errors below and/or follow the hints provided

#[allow(dead_code)]
#[allow(unused_variables)]


fn main() {
    // TASK 1 - Scope and Validity
    // Hint: Variables in 'Inner scope' are not available in 'Outer scope'
    struct Point(i32, i32);
    { // Outer scope starts
        { // Inner scope starts
            let pt = Point(3, 4); 
            println!("x: {}", pt.0);
        } 
        println!("y: {}", pt.1); 
    } 
    
    // TASK 2 - Ownership Principles
    // Hint: Every value has precisely one owner at all times
    let poodle = String::from("ball"); 
    let yorkie = poodle; 
    println!("{}", poodle); 


    // TASK 3 - Explicit Duplication   
    // Hint: Modify the code so that 'yorkie' has a copy of the same ball 'poodle' has
    let poodle = String::from("ball");
    let yorkie = poodle;
    println!("{}", poodle); 
    println!("{}", yorkie); 


    // TASK 4 - "Clone Away" Strategy
    // Hint: Modify the function call without creating new variables to have multiple agents smith saying hello!
    fn say_hello(name: String) {
        println!("i'm agent {}", name);
    }
    let agent = String::from("Smith");
    say_hello(agent);
    say_hello(agent);
    say_hello(agent);
    say_hello(agent);
    

    // TASK 5 - Custom "Copy" Types
    // Hint: Copy is not a 'standalone' trait. it has requirements
    #[derive(Copy)]
    struct Point(i32, i32);
    let p1 = Point(3, 4);
    let p2 = p1;

    // TASK 6 - "Copy" Types and Field Constraints
    // Hint: Copy is not always possible
    #[derive(Copy, Clone)]
    struct User(i32, String);
    let user_a = User(42, String::from("Alice"));
    let user_b = user_a;
    println!("We are {} and {}", user_a.1, user_b.1); 


    // TASK 7 - "Copy" and "Drop"
    // Hint: Some traits cannot be implemented together for a type
    #[derive(Copy, Clone)]
    struct Highlander;
    impl Drop for Highlander {
        fn drop(&mut self) {
            println!("There can be only one!");
        }
    }

     
    // TASK 8 - Destructor ("Drop")
    // Hint: Correct implementation to by adding the self reference then add the function call to drop the mic!
    struct Objects {
        object_type: String,
    }
    impl Drop for Objects {
        fn drop(&mut self) {
            println!("Drop the {}!", object_type);
        }
    }
    let shure_SM58 = Objects{object_type: String::from("mic")} ;
    Drop(shure_SM58);
}
