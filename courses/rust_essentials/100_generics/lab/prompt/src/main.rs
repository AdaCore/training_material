//! Lab (prompt)
//! Generics
//!
//! Fix all the compile errors below by following the hints provided 
//! 
#[allow(dead_code)]
#[allow(unused_variables)]
fn main() {
    use std::fmt::Debug;

    // TASK 1 - Generic Type Parameter
    // Hint: Generic type parameter must be declared before it is used as a type placeholder
    fn identity(item: T) -> T {
        item
    }

    let a = identity(10);

    // TASK 2 - Trait Bounds
    // Hint: Compiler doesn't know if 'T' can be compared, `PartialOrd` trait bound is needed
    fn smaller<T>(item: T, max_v: T) -> bool {
        item < max_v
    }

    // TASK 3 - Meeting Constraints
    // Hint: Vegetable needs to implement `PartialOrd` and `PartialEq`
    struct Vegetable;
    
    let potato = Vegetable;
    let sweet_potato = Vegetable;
    
    println!("{}", smaller(potato, sweet_potato));

    // TASK 4 - Turbofish Syntax
    // Hint: Compiler needs to know what type the 'Vec' holds, the turbofish syntax `::<>` will remove ambiguity
    let mystery_vec = Vec::new();

    // TASK 5 - Multiple Generic Types
    // Hint: Constructs can have multiple generic data types
    struct Point<T> {
        x: T,
        y: U,
    }

    let p = Point { x: 5, y: 4.0 };

    // TASK 6 - Multiple Traits
    // Hint: You can have multiple trait bounds, the `+` operator combines them
    fn print_and_return<T: Debug Clone>(item: T) -> T {
        println!("{:?}", item);
        item.clone()
    }

    // TASK 7 - "derive" Macro and Generics
    // Hint: The `derive` macro assumes the inner type also implements the trait
    #[derive(Debug)]
    struct BoxContainer<T> {
        content: T,
    }

    struct Secret; 

    let bad_box = BoxContainer { content: Secret };
    println!("{:?}", bad_box); // Fails if 'Secret' doesn't implement `Debug`

    // TASK 8 - Generic Traits
    // Hint: Traits can be made generic to interact with multiple types
    trait Transform<T> {
        fn convert(&self) -> T;
    }

    struct Minutes(i32);

    // Provide the target type parameter (e.g., 'String') for the generic trait Transform
    impl Transform for Minutes {
        fn convert(&self) -> String {
            format!("{} mins", self.0)
        }
    }

    // TASK 9 - Const Generics
    // Hint: Const Generics are generic over a value, not a type
    struct Buffer<N: usize> {
        data: [i32; N],
    }

    let small_buffer = Buffer::<10> { data: [0; 10] };
}
