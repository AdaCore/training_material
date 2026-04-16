//! Lab (prompt)
//! Lifetimes
//!
//! Fix all the compile errors below by following the hints provided
//!

struct Owner {
    name: String,
}
struct User<'a> {
    name: &'a str,
}


fn choose<>(left: &str, right: &str) -> &str {
    // Return reference to longer of 'left' or 'right'
    if left.len() > right.len() {
        left
    } else {
        right
    }
}


fn find_user(owner: &Owner, _extra_data: &str) -> User {
    User { name: &owner.name }

fn main() {

    // TASK 1 - Ensure reference is valid
    // Hint: Cannot point to something that no longer exists
    println!("Task 1");
    let treasure_map: &i32;
    {
        let gold = 1000;
        treasure_map = &gold;
        println!("{gold}");
    }
    println!("{treasure_map}");

    // TASK 2 - Print the longer of two strings
    // Hint: Make sure both borrowed objects live long enough
    println!("Task 2");
    let longer: &str;
    {
        let femur = String::from("the leg bone");
        let ulna = String::from("an arm bone");
        longer = choose(femur, ulna);
    }
    println!("Longer bone is {longer}");



    // TASK 3 - Prevent "use after free" bug
    // Hint: "find_user" returns borrowed data - where does it come from?
    println!("Task 3");
    let my_owner = Owner { name: String::from("Permanent") };
    let result_user;
    {
        let short_lived_str = String::from("Temporary");
        result_user = find_user(my_owner, short_lived_str);
    } 
    println!("{}", result_user.name);

    // TASK 4 - Share the scroll among partners
    // Hint: When you borrow something, ensure what you borrowed lives long enough
    println!("Task 4");
    struct OwnedScroll {
        inscription : String,
    }

    struct BorrowedScroll {
        inscription: &str,
    }

    let orc: BorrowedScroll;
    let elf: BorrowedScroll;

    {
        let leader = OwnedScroll { inscription: String::from("Avada Kedavera"), };
        println!("Leader's scroll: {}", leader.inscription);

        orc = BorrowedScroll { inscription: &leader.inscription, };
        elf = BorrowedScroll { inscription: &leader.inscription, };
    }

    println!("Orc is looking at: {}", orc.inscription);
    println!("Elf is looking at: {}", elf.inscription);

}
