//! Lab (answer)
//! Structs and Enums
//!
#[allow(dead_code)]
#[allow(unused_variables)]

fn main() {
    // TASK 1 - Partial Initialization
    // Hint: full field initialization is mandatory
    struct User {
        active: bool,
        sign_in_count: u64,
        logged_in: bool,
    }

    let james_bond = User {
        active: true,
        sign_in_count: 1,
        logged_in: true, // Every field defined in a struct must be initialized
    };

    // TASK 2 - Field Init Shorthand
    // Hint: shorthand only works if the field and variable have the EXACT same name
    struct Player {
        active: bool,
    }
    let active = true; // Variable name changed to match the field name for shorthand compatibility

    let mario = Player { active };

    // TASK 3 - Struct Update Operator
    // Hint: base instance (..) must be at the end of the declaration
    struct Settings {
        font_size: u32,
        active: bool,
    }

    let default_set = Settings { font_size: 18, active: false };
    
    let set_1 = Settings {
        active: true,
        ..default_set // The update operator must be the last element in the struct initialization
    }; 

    // TASK 4 - Partial Mutability
    // Hint: mutability applies to the entire instance; no partial application for fields
    struct CatStatus {
        energy_level: u8,
        is_napping: bool,
    }

    // Mutability is an attribute of the variable, not the individual fields
    let mut new_cat = CatStatus {
        energy_level: 80,
        is_napping: false,
    };

    new_cat.energy_level = 90;

    println!("TASK4 => energy_level {}", new_cat.energy_level);

    // TASK 5 - Tuple Struct Indexing
    // Hint: tuple indexing starts at 0 and must stay within bounds
    struct Character(u64, i64, bool);
    let hero = Character(1000, 500, true);

    // Indices are zero-based; the third field is accessed via index 2
    println!("TASK5 => hero is masked: {}", hero.2);


    // TASK 6 - Type Safety with Tuples
    // Hint: tuple structs with the same definition are different types
    #[derive(Debug)]
    struct Coordinates(i32, i32);
    struct Point(i32, i32);

    let point = Point(10, 20);
    let coordinates = Coordinates(point.0, point.1);

    println!("TASK6 => coordinates: {:?}", coordinates);

    // TASK 7 - Enum Pathing
    // Hint: variants are accessed using the path separator notation
    #[derive(Debug)]
    enum Direction {
        Left,
        Right,
    }

    // Enum variants must be accessed through the enum name using the path separator
    let direction = Direction::Left;

    println!("TASK7 => direction: {:?}", direction);
    
    // TASK 8 - Enum Data Initialization
    // Hint: data must be initialized if the variant holds data
    #[derive(Debug)]
    enum Message {
        Quit,
        Move { x: i32, y: i32 },
    }

    // Variants containing data require those values to be provided during instantiation
    let message = Message::Move { x: 10, y: 20};

    println!("TASK7 => message: {:?}", message);
}
