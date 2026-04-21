//! Lab (answers)
//! Lifetimes
//!
//! Fix all the compile errors below by following the hints provided
//!

fn task_1() {
    println!("Task 1 - Lifetime");
    // Task 1 Goals
    //   a) Create an object and a reference
    //   b) Ensure object lives as long as reference
    //   c) Print referenced value
    // Hint:
    //   Object should be in same scope as reference (or higher)
    let treasure_map: &i32;
    {
        let gold = 1000;
        treasure_map = &gold;
        println!("{gold}");
        println!("{treasure_map}");
    }
}

fn task_2() {
    println!("Task 2 - Solving ambiguity");
    // Task 2 Goals
    //   a) Create two String objects
    //   b) Create a function that takes two 'str' references
    //   c) Function should return longer of the two parameters
    //   d) Print the function return value
    // Hint:
    //   Return value needs to live as long as longest-lived string

    fn choose<'a>(left: &'a str, right: &'a str) -> &'a str {
        // Return reference to longer of 'left' or 'right'
        if left.len() > right.len() {
            left
        } else {
            right
        }
    }

    let longer12: &str;
    let longer23: &str;
    let longer13: &str;
    let input1 = String::from("medium");
    {
        let input2 = String::from("short");
        {
            let input3 = String::from("very long");
            longer12 = choose(&input1, &input2);
            longer23 = choose(&input2, &input3);
            longer13 = choose(&input1, &input3);
            println!("longer12 = {}", longer12);
            println!("longer23 = {}", longer23);
            println!("longer13 = {}", longer13);
        }
    }
}

fn task_3() {
    println!("Task 3 - Lifetimes in structs");
    // Task 3 Goals
    //   a) Create a function that returns a reference from within a struct
    //   b) Print the returned data
    // Hint:
    //   Function needs to indicate where the borrowed data comes from
    //   Struct needs to specify that the referenced element lasts as long as the struct

    struct Owner {
        name: String,
    }
    struct User<'a> {
        name: &'a str,
    }

    fn find_user<'a>(owner: &'a Owner, _extra_data: &str) -> User<'a> {
        User { name: &owner.name }
    }

    let my_owner = Owner {
        name: String::from("Permanent"),
    };
    let result_user;
    {
        let short_lived_str = String::from("Temporary");
        result_user = find_user(&my_owner, &short_lived_str);
    }
    println!("{}", result_user.name);
}

fn task_4() {
    println!("Task 4 - Lifetimes for enums");
    // Task 4 Goals
    //   a) Create a function that returns a reference from within an enum variant
    //   b) Print the returned data
    // Hint:
    //   Function needs to indicate where the borrowed data comes from
    //   Enum needs to specify that the referenced variant lasts as long as the enum
    struct Owner {
        name: String,
    }

    enum UserSession<'a> {
        _Admin,
        Guest(&'a str),
    }

    fn get_session<'a>(owner: &'a Owner, _extra: &str) -> UserSession<'a> {
        UserSession::Guest(&owner.name)
    }

    let my_owner = Owner {
        name: String::from("Permanent"),
    };

    let result_session;
    {
        let short_lived_str = String::from("Temporary");
        result_session = get_session(&my_owner, &short_lived_str);
    }

    match result_session {
        UserSession::Guest(name) => println!("Guest name: {}", name),
        UserSession::_Admin => println!("Logged in as Admin"),
    }
}

fn main() {
    task_1();
    task_2();
    task_3();
    task_4();
}
