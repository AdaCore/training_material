//! Lab (answer)
//! Standard Library Traits
//!
#[allow(dead_code)]
#[allow(unused_variables)]
fn main() {
    
    // TASK 1 - Trait Dependencies for Ordering
    // Hint: PartialOrd requires PartialEq to be defined
    
    // Fix: Added PartialEq to the derive macro
    #[derive(PartialEq, PartialOrd)]
    struct Score {
        points: u32,
    }

    let s1 = Score { points: 10 };
    let s2 = Score { points: 20 };
    let is_less = s1 < s2;


    // TASK 2 - Invalid Casting
    // Hint: Casting won't work here, so access the inner field directly
    struct Wrapper(i32);
    let my_wrap = Wrapper(5);
    
    // Fix: Avoided the `as` keyword on a struct and accessed the internal primitive directly
    let raw_int = my_wrap.0;


    // TASK 3 - The `Into` Trait Inference Mystery
    // Hint: Compiler cannot infer the target type so explicit type hints must be provided
    let source: i32 = 42;
    
    // Fix: Provided an explicit type hint (i64) for the target variable
    let target: i64 = source.into();


    // TASK 4 - Operator Overloading Output
    // Hint: When overloading operators the Self::Output return type must be explicitly declared
    struct Point { x: i32 }
    
    impl std::ops::Add for Point {
        // Fix: Defined the associated type Output for the trait
        type Output = Point;

        fn add(self, rhs: Self) -> Self::Output {
            Point { x: self.x + rhs.x }
        }
    }


    // TASK 5 - Enum Defaults
    // Hint: Deriving Default on an enum requires specifying a default variant with the #[default] attribute
    #[derive(Default)]
    enum Status {
        // Fix: Added the #[default] attribute to the desired unit variant
        #[default]
        Pending,
        Active(i32),
        Closed,
    }

    let current_status = Status::default();


    // TASK 6 - Struct Default Initialization
    // Hint: Using struct update syntax (..Default::default()) requires the struct itself to derive or implement the Default trait
    
    // Fix: Added the #[derive(Default)] macro to the struct
    #[derive(Default)]
    struct Config {
        port: u16,
        host: String,
    }

    let my_config = Config { port: 8080, ..Default::default() };


    // TASK 7 - Trait Scope for I/O
    // Hint: Using methods like write_all requires bringing the std::io::Write trait into scope
    
    // Fix: Brought the Write trait into scope
    use std::io::Write;
    
    let mut buffer = Vec::new();
    // Handled the Result returned by write_all
    let _ = buffer.write_all(b"Hello, Rust!");


    // TASK 8 - Safer Conversions
    // Hint: TryFrom returns a Result that must be handled or unwrapped unlike the `as` keyword
    let big_number: i32 = 300;
    
    // Fix: Used a match statement (or unwrap_or) to safely handle the Result returned by TryFrom.
    let tried: u8 = u8::try_from(big_number).unwrap_or(0);


    // TASK 9 - Equality Comparison
    // Hint: Using the == operator requires implementing or deriving PartialEq
    
    // Fix: Derived the PartialEq trait to enable the `==` operator
    #[derive(PartialEq)]
    struct SensorData {
        valid: bool,
    }

    let sensor_1 = SensorData { valid: true };
    let sensor_2 = SensorData { valid: true };
    
    let is_same = sensor_1 == sensor_2;
}
