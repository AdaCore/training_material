//! Lab (prompt)
//! Methods and Traits
//!
//! Fix all the compile errors below by following the hints provided
//! 
#[allow(dead_code)]
#[allow(unused_variables)]

fn main() {
    
    // TASK 1 - Method Receiver: Mutable Borrow
    // Hint: The caller must declare the variable mut
    struct Counter { value: i32 }
    impl Counter {
        fn increment(&mut self) { self.value += 1; }
    }
    
    let bad = Counter { value: 0 };
    bad.increment();


    // TASK 2 - Method Receiver: Take Ownership
    // Hint: Object cannot be reused afterward because value is moved into the method
    impl Counter {
        fn finalize(self) -> i32 { self.value }
        fn get(&self) -> i32 { self.value }
    }
    
    let count = Counter { value: 10 };
    let total = count.finalize();
    let current = count.get();


    // TASK 3 - Method Receiver: No Receiver
    // Hint: This is called an associated function, it must be called on the type, not an instance
    impl Counter {
        fn new() -> Self { Counter { value: 0 } }
    }
    
    let new_count = Counter.new();


    // TASK 4 - Implementing a Trait
    // Hint: The syntax requires implementing the trait for the type, not the type for the trait
    trait Friend { fn greet(&self); }
    struct Dog { name: String }
    
    impl Dog for Friend {
        fn greet(&self) { println!("Woof!"); }
    }


    // TASK 5 - Default Trait Methods
    // Hint: Traits can provide default implementations, but types implementing the trait must still define required methods
    trait Speaker {
        fn message(&self) -> String;
        fn speak(&self) { println!("{}", self.message()); }
    }
    struct Cat;
    
    impl Speaker for Cat {
        // Missing implementation for `message`
    }


    // TASK 6 - Deriving in Complex Structures
    // Hint: When a type derives a trait, its included items must also derive the trait
    struct Engine { horsepower: i32 }
    
    #[derive(Clone)]
    struct Car { engine: Engine }


    // TASK 7 - Supertraits
    // Hint: A supertrait is a trait that requires another trait to be implemented
    trait Dance { fn dance(&self); }
    trait PartyAnimal: Dance { fn party(&self); }
    struct Bear;
    
    impl PartyAnimal for Bear {
        fn party(&self) { println!("Bear party!"); }
    }


    // TASK 8 - Associated Types
    // Hint: The implementer must explicitly decide the type of an associated type
    trait Animal {
        type Food; 
        fn consume(&self, food: Self::Food);
    }
    struct Bird;
    struct Seed;
    
    impl Animal for Bird {
        // Missing the associated type assignment
        fn consume(&self, food: Seed) { println!("Peck peck"); }
    }
}
