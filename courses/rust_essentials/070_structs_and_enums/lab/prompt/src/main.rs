//! Lab (prompt)
//! Structs and Enums
//!
//! Fix all the compile errors below by following the hints provided 
//! Use the command 'cargo run' to verify the solution
//! 
#[allow(dead_code)]
#[allow(unused_variables)]

fn main() {
    
    // TASK 1 - Partial Initialization
    // Hint: full field initialization is mandatory
    {
        struct User {
            active: bool,
            sign_in_count: u64,
            logged_in: bool,
        }

        let user_1 = User {
            active: true,
            sign_in_count: 1,
        };
    }

    // TASK 2 - Field Init Shorthand
    // Hint: shorthand only works if the field and variable have the EXACT same name
    {
        struct User {
            active: bool,
        }
        let activated = true;

        let user_1 = User { activated };
    }

    // TASK 3 - Struct Update Operator
    // Hint: base instance (..) must be at the end of the declaration
    {
        struct Settings {
            font_size: u32,
            active: bool,
        }

        let default_set = Settings { font_size: 18, active: false };
        
        let set_1 = Settings {
            ..default_set,
            active: true,
        }; 
    }

    // TASK 4 - Partial Mutability
    // Hint: mutability applies to the entire instance; no partial application for fields
    {
        struct CatStatus {
            energy_level: u8,
            is_napping: bool,
        }

        let new_cat = CatStatus {
            mut energy_level: 80,
            is_napping: false,
        };

        new_cat.energy_level = 90;
    }

    // TASK 5 - Tuple Struct Indexing
    // Hint: tuple indexing starts at 0 and must stay within bounds
    {
        struct Character(u64, i64, bool);
        let hero = Character(1000, 500, true);

        println!("Power: {}", hero.3);
    }

    // TASK 6 - Type Safety with Tuples
    // Hint: tuple structs with the same definition are different types
    {
        struct Point(i32, i32);
        struct Coordinates(i32, i32);
        let point = Point(10, 20);
        let mut coordinates = Coordinates(30, 40);
        
        coordinates = point;
    }

    // TASK 7 - Enum Pathing
    // Hint: variants are accessed using the path separator notation
    {
        enum Direction {
            Left,
            Right,
        }

        let direction = Left; 
    }

    // TASK 8 - Enum Data Initialization
    // Hint: data must be initialized if the variant holds data
    {
        enum Message {
            Quit,
            Move { x: i32, y: i32 },
        }

        let message = Message::Move;
    }
    
    // SANDBOX - Experiment here!
    {

    }
}
