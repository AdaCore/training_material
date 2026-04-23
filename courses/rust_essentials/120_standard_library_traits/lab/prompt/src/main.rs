//! Lab (prompt)
//! Standard Library Traits
//!
//! Fix all the compile errors below by following the hints provided 
//! 
#[allow(dead_code)]
#[allow(unused_variables)]
fn main() {
    
    // TASK 1 - Trait Dependencies for Ordering
    // Hint: PartialOrd requires PartialEq to be defined
    #[derive(PartialOrd)]
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
    
    let raw_int = my_wrap as i32;


    // TASK 3 - The `Into` Trait Inference Mystery
    // Hint: Compiler cannot infer the target type so explicit type hints must be provided
    let source: i32 = 42;
    
    let target = source.into();


    // TASK 4 - Operator Overloading Output
    // Hint: When overloading operators the Self::Output return type must be explicitly declared
    struct Point { x: i32 }
    
    impl std::ops::Add for Point {
        // Missing the associated type Output
        fn add(self, rhs: Self) -> Self::Output {
            Point { x: self.x + rhs.x }
        }
    }


    // TASK 5 - Enum Defaults
    // Hint: Deriving Default on an enum requires specifying a default variant with the #[default] attribute
    #[derive(Default)]
    enum Status {
        Pending,
        Active(i32),
        Closed,
    }

    let current_status = Status::default();


    // TASK 6 - Struct Default Initialization
    // Hint: Using struct update syntax (..Default::default()) requires the struct itself to derive or implement the Default trait
    struct Config {
        port: u16,
        host: String,
    }

    let my_config = Config { port: 8080, ..Default::default() };


    // TASK 7 - Trait Scope for I/O
    // Hint: Using methods like write_all requires bringing the std::io::Write trait into scope
    let mut buffer = Vec::new();
    
    buffer.write_all(b"Hello, Rust!");


    // TASK 8 - Safer Conversions
    // Hint: TryFrom returns a Result that must be handled or unwrapped unlike the `as` keyword
    let big_number: i32 = 300;
    let tried: u8 = u8::try_from(big_number);


    // TASK 9 - Equality Comparison
    // Hint: Using the == operator requires implementing or deriving PartialEq
    struct SensorData {
        valid: bool,
    }

    let sensor_1 = SensorData { valid: true };
    let sensor_2 = SensorData { valid: true };
    
    let is_same = sensor_1 == sensor_2;
}
