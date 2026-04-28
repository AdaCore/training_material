//! Lab (prompt)
//! Common Library Types
//!
//! Fix all the compile errors below by following the hints provided 
//! 
#[allow(dead_code)]
#[allow(unused_variables)]
fn main() {

    // TASK 1 - Option Type Handling
    // Hint: Option<T> must be explicitly handled
    let maybe_number = Some(42);
    let result = maybe_number + 10;

    // TASK 2 - Result Variants
    // Hint: Result uses Ok(T) and Err(E)
    fn check_positive(num: i32) -> Result<i32, String> {
        if num > 0 { 
            Some(num) 
        } else { 
            None 
        }
    }

    // TASK 3 - String vs &str
    // Hint: &str is a fixed view, an owned and growable String is needed to append text
    let mut greeting = "Hello";
    greeting.push_str(" world");

    // TASK 4 - Modifying Strings (push vs push_str)
    // Hint: Which String method appends a slice instead of a single char?
    let mut text = String::from("Rust");
    text.push(" is great");

    // TASK 5 - Vector Creation Macro
    // Hint: How is a macro invoked differently than a function?
    let numbers = vec[1, 2, 3];

    // TASK 6 - Representing Absence
    // Hint: Rust does not have null pointers, absence is represented by the appropriate Option variant
    fn get_user(id: u32) -> Option<String> {
        null
    }

}
