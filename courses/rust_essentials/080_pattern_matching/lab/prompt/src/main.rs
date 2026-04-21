//! Lab (prompt)
//! Pattern Matching
//!
//! Fix all the compile errors below by following the hints provided 
//! 
#[allow(dead_code)]
#[allow(unused_variables)]

fn main() {
    
    // TASK 1 - Exhaustiveness in `match`
    // Hint: All possible cases must be handled
    enum Status {
        Online,
        Offline,
        Busy,
    }

    let current_status = Status::Online;
    match current_status {
        Status::Online => println!("User is online"),
        Status::Offline => println!("User is offline"),
    }


    // TASK 2 - Refutable Pattern in a `let` Binding
    // Hint: Every `let` binding uses a pattern, but simple bindings require irrefutable patterns
    let secret_value = Some(42);
    
    let Some(x) = secret_value;
    println!("The secret is {}", x);


    // TASK 3 - Ignoring Fields in Struct Destructuring
    // Hint: Unused fields may be ignored using the rest pattern (..) to capture specific named fields and ignore the rest
    struct Player {
        name: String,
        health: u32,
        mana: u32,
    }

    let p = Player { name: String::from("Hero"), health: 100, mana: 50 };
    
    let Player { name, health } = p;


    // TASK 4 - Destructuring Variant Data
    // Hint: You must keep the parentheses for variants with data, even if you are ignoring the content
    enum Alert {
        Clear,
        Warning(String),
    }

    let a = Alert::Warning(String::from("Low Battery"));
    match a {
        Alert::Warning => println!("Received a warning, ignoring details"),
        Alert::Clear => println!("All clear"),
    }


    // TASK 5 - Match Guards and Exhaustiveness
    // Hint: Guards are dynamic, compiler usually requires a "catch-all" arm (_) when using match guards
    let temperature = 30;
    match temperature {
        t if t > 25 => println!("Hot"),
        t if t <= 25 => println!("Cool"),
    }


    // TASK 6 - Longhand Struct Destructuring and Renaming
    // Hint: Use `field: variable` to rename data as it is extracted
    struct Coordinates {
        x: i32,
        y: i32,
    }

    let c = Coordinates { x: 5, y: 10 };
    let Coordinates { my_x: x, my_y: y } = c;
    
    println!("The coordinates are: {},{}", my_x, my_y);


    // TASK 7 - Destructuring Struct Variants
    // Hint: Struct variants use named fields, patterns must match fields by name using {} instead of ()
    enum Geometry {
        Rectangle { width: u32, height: u32 },
    }

    let geo = Geometry::Rectangle { width: 10, height: 20 };
    match geo {
        Geometry::Rectangle(width, height) => println!("{}x{}", width, height),
    }


    // TASK 8 - "if let" Syntax Order
    // Hint: "if let" follows assignment order: Pattern = value
    let optional_code = Some(777);
    
    if let optional_code = Some(code) {
        println!("Code is {}", code);
    }


    // TASK 9 - Variable Bindings with Ranges
    // Hint: Use the `@` operator to give a name to a value while checking it against a range
    let dice_roll = 4;
    match dice_roll {
        1..=6 @ roll => println!("Rolled a {}", roll),
        _ => println!("Invalid roll"),
    }
}
