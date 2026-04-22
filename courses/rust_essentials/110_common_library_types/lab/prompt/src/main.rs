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
    // Hint: push() appends a single char, to append a string slice (&str), use push_str()
    let mut text = String::from("Rust");
    text.push(" is great");


    // TASK 5 - Vector Creation Macro
    // Hint: The vec macro requires a bang (!) to initialize a Vector with values
    let numbers = vec[1, 2, 3];


    // TASK 6 - Safe Vector Access
    // Hint: v.get(idx) returns an Option<&T>, not the value directly
    let my_vec = vec![10, 20, 30];
    let val: &i32 = my_vec.get(1);


    // TASK 7 - Representing Absence
    // Hint: Rust does not have null pointers, absence is represented by the appropriate Option variant
    fn get_user(id: u32) -> Option<String> {
        null
    }


    // TASK 8 - String Length vs Characters
    // Hint: len() returns bytes, use chars() and count() to get the number of UTF-8 characters
    let fancy = String::from("City");
    let char_count: usize = fancy.length();


    // TASK 9 - Safe Indexing
    // Hint: Use .get() to safely access an element as an Option
    let colors = vec!["Red", "Green", "Blue"];
    let fifth_color: Option<&&str> = colors[5];

}
