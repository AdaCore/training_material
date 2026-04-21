//! Lab (answer)
//! Methods and Traits
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
    
    // Fix: Declared `bad` (now `good`) as mutable
    let mut good = Counter { value: 0 };
    good.increment();


    // TASK 2 - Method Receiver: Take Ownership
    // Hint: Object cannot be reused afterward because value is moved into the method
    impl Counter {
        fn finalize(self) -> i32 { self.value }
        fn get(&self) -> i32 { self.value }
    }
    
    let count = Counter { value: 10 };
    // Fix: Move the borrow (`get`) before the ownership is consumed (`finalize`), or avoid calling it entirely
    let current = count.get();
    let total = count.finalize();


    // TASK 3 - Method Receiver: No Receiver
    // Hint: This is called an associated function, it must be called on the type, not an instance
    impl Counter {
        fn new() -> Self { Counter { value: 0 } }
    }
    
    // Fix: Used the path separator `::` to call the associated function on the type
    let new_count = Counter::new();


    // TASK 4 - Implementing a Trait
    // Hint: The syntax requires implementing the trait for the type, not the type for the trait
    trait Friend { fn greet(&self); }
    struct Dog { name: String }
    
    // Fix: Swapped the trait and type names.
    impl Friend for Dog {
        fn greet(&self) { println!("Woof!"); }
    }


    // TASK 5 - Default Trait Methods
    // Hint: Traits can provide default implementations, but types implementing the trait must still define required methods
    trait Speaker {
        fn message(&self) -> String;
        fn speak(&self) { println!("{}", self.message()); }
    }
    struct Cat;
    
    // Fix: Provided the required implementation for the `message` method.
    impl Speaker for Cat {
        fn message(&self) -> String {
            String::from("Meow!")
        }
    }


    // TASK 6 - Deriving in Complex Structures
    // Hint: When a type derives a trait, its included items must also derive the trait
    
    // Fix: Added the `#[derive(Clone)]` attribute to the inner struct
    #[derive(Clone)]
    struct Engine { horsepower: i32 }
    
    #[derive(Clone)]
    struct Car { engine: Engine }


    // TASK 7 - Supertraits
    // Hint: A supertrait is a trait that requires another trait to be implemented
    trait Dance { fn dance(&self); }
    trait PartyAnimal: Dance { fn party(&self); }
    struct Bear;
    
    // Fix: Added the implementation for the base trait `Dance`
    impl Dance for Bear {
        fn dance(&self) { println!("Bear shuffle"); }
    }
    
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
    
    // Fix: Assigned the associated type so the compiler knows what `Self::Food` represents
    impl Animal for Bird {
        type Food = Seed;
        
        fn consume(&self, food: Seed) { println!("Peck peck"); }
    }
}
