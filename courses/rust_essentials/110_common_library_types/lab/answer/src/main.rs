//! Lab (answer)
//! Common Library Types
//!
#[allow(dead_code)]
#[allow(unused_variables)]
fn main() {
    
    // TASK 1 - Option Type Handling
    // Hint: Option<T> must be explicitly handled
    let maybe_number = Some(42);
    let result = match maybe_number {
        Some(v) => v + 10,
        None => 0,
    };    
    println!("TASK1 => result: {}", result);

    // TASK 2 - Result Variants
    // Hint: Result uses Ok(T) and Err(E)
    fn check_positive(num: i32) -> Result<i32, String> {
        if num > 0 { 
            Ok(num) // Replaced Some with Ok
        } else { 
            Err(String::from("Not a positive number")) // Replaced None with Err
        }
    }
    println!("TASK2 => check: {:?}", check_positive(-5));

    // TASK 3 - String vs &str
    // Hint: &str is a fixed view, an owned and growable String is needed to append text
    let mut greeting = String::from("Hello"); // Converted to an owned String
    greeting.push_str(" world");
    println!("TASK3 => greeting: {}", greeting);

    // TASK 4 - Modifying Strings (push vs push_str)
    // Hint: Which String method appends a slice instead of a single char?
    let mut text = String::from("Rust");
    text.push_str(" is great"); // Changed push to push_str
    println!("TASK4 => text: {}", text);

    // TASK 5 - Vector Creation Macro
    // Hint: How is a macro invoked differently than a function?
    let numbers = vec![1, 2, 3]; // Added ! to properly invoke the macro
    println!("TASK5 => numbers length: {}", numbers.len());

    // TASK 6 - Representing Absence
    // Hint: Rust does not have null pointers, absence is represented by the appropriate Option variant
    fn get_user(id: u32) -> Option<String> {
        None // Replaced null with None
    }
    println!("TASK6 => get_user(1): {:?}", get_user(1));

}
