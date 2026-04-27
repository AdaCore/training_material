//! Lab (answer)
//! Memory Management
//!
//! Fix all the compile errors below and/or follow the hints provided
//!

#![allow(unused_assignments)]
#![allow(unused_variables)]
#![allow(dead_code)]

fn main() {
    // TASK 1 - Scope and Validity
    // Hint: Variables in 'Inner scope' are not available in 'Outer scope'
    struct Point(i32, i32);
    { // Outer scope starts
        let pt = Point(3, 4);
        { // Inner scope starts
            println!("x: {}", pt.0);
        } 
        println!("y: {}", pt.1); 
    } 
    
    // TASK 2 - Ownership Principles
    // Hint: Every value has precisely one owner at all times
    let poodle = String::from("ball"); 
    let yorkie = poodle; 
    println!("{}", yorkie); 


    // TASK 3 - Explicit Duplication   
    // Hint: Modify the code so that 'yorkie' has a copy of the same ball 'poodle' has
    let poodle_2 = String::from("ball");
    let yorkie_2 = poodle_2.clone();
    println!("{}", poodle_2); 
    println!("{}", yorkie_2); 


    // TASK 4 - "Clone Away" Strategy
    // Hint: Modify the function call without creating new variables to have multiple agents smith saying hello!
    fn say_hello(name: String) {
        println!("i'm agent {}", name);
    }
    let agent = String::from("Smith");
    say_hello(agent.clone());
    say_hello(agent.clone());
    say_hello(agent.clone());
    say_hello(agent);
    

    // TASK 5 - Custom "Copy" Types
    // Hint: Copy is not a 'standalone' trait. it has requirements
    #[derive(Copy, Clone)]
    struct Point2(i32, i32);
    let p1 = Point2(3, 4);
    let p2 = p1;

    // TASK 6 - "Copy" Types and Field Constraints
    // Hint: Copy is not always possible
    #[derive(Clone)]
    struct User(i32, String);
    let user_a = User(42, String::from("Alice"));
    let user_b = user_a.clone();
    println!("We are {} and {}", user_a.1, user_b.1); 


    // TASK 7 - "Copy" and "Drop"
    // Hint: Some traits cannot be implemented together for a type
    #[derive(Clone)]
    struct Highlander;
    impl Drop for Highlander {
        fn drop(&mut self) {
            println!("There can be only one!");
        }
    }

     
    // TASK 8 - Destructor ("Drop")
    // Hint: Correct implementation by adding the self reference then add the function call to drop the mic!
    struct Objects {
        object_type: String,
    }
    impl Drop for Objects {
        fn drop(&mut self) {
            println!("Drop the {}!", self.object_type);
        }
    }
    let shure_sm58 = Objects{object_type: String::from("mic")} ;
    drop(shure_sm58);
}
