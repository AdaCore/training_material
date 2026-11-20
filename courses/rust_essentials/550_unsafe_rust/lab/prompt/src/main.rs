//! Lab (prompt)
//! Unsafe Rust
//!
//! Fix all the compile errors below by following the hints provided
//!
#[allow(dead_code)]
#[allow(unused_variables)]
fn main() {

    // TASK 1 - Dereferencing Raw Pointers
    // Hint: Dereferencing a raw pointer is an unsafe operation
    let mut ellen_ripley = 0;
    let face_hugger = &raw mut ellen_ripley;
    *face_hugger = 426;
    println!("Ripley status: {}", *face_hugger);

    // TASK 2 - Creating Raw Pointers via Conversion
    // Hint: Cast from a reference using "as"
    let treasure : i32 = 5;
    let reference = &treasure;
    // Change this so the warning disappears
    let clue = reference;
    // Warning: unnecessary `unsafe` block
    unsafe {
        println!("Clue: {}", *clue);
    }

    // TASK 3 - Aliasing and Safety Obligations
    // Hint: Raw pointers may alias the same memory without a compilation error
    let mut coins = 0;
    // Fix the declarations so that they both point to the same address
    let mario = coins;
    let luigi = coins;
    mario = 64;
    luigi = 120;

    // Should be the value of the last assignment
    println!("Number of coins: {}", *coins);

    // TASK 4 - Declaring an Unsafe Function
    // Hint: Calling an unsafe function requires an unsafe context
    // Bonus: Add the safety contracts for the function and caller
    unsafe fn get_some(value: Option<i32>) -> i32 {
        value.unwrap_unchecked()
    }

    let universal_answer =  get_some(Some(42));
    println!("The universal answer: {universal_answer}");

    // TASK 5 - Updating a Mutable Static
    // Hint: Reading or writing requires an unsafe context
    // Bonus: Add the safety contracts for the function and caller
    static RAPTORS_LOOSE: u32 = 0;

    unsafe fn release_raptor() {
        RAPTORS_LOOSE += 1;
    }

    release_raptor();
    // Declare a new variable to handle the value to print
    println!("Raptors loose: {RAPTORS_LOOSE}");

    // TASK 6 - Reading Union Fields
    // Hint: Reading is an unsafe operation
    #[repr(C)]
    union Avenger {
        banner: i8,
        hulk: u8,
    }
    let mut avenger = Avenger { banner: -1, hulk: 254};
    // Write a new value to the other field than the one selected in the declaration
    // Read both union fields separately to interpret the same stored byte
    println!("Hulk: {0}", avenger.hulk);
    println!("Banner: {0}", avenger.banner);

    // TASK 7 - Encapsulating the Unsafe Read
    // Hint: A safe method can perform an unsafe operation
    // Bonus: Add a SAFETY comment justifying the internal unsafe operation
    impl Avenger {
       fn as_unsigned(&self) -> u8 {
            hulk
        }
    }
    let hulk_strength = avenger.as_unsigned();
    println! ("Hulk strength: {hulk_strength}");

    // TASK 8 - Implementing an Unsafe Trait
    // Hint: Implementing an unsafe trait requires an unsafe impl block
    // Bonus: Add SAFETY comments justifying both unsafe operations

    /// # Safety
    /// All-zero byte patterns must represent a valid, initialized value of 'Self'
    unsafe trait Zeroable: Sized {
        fn zeroed() -> Self {
            std::mem::zeroed()
        }
    }

    #[derive(Debug)]
    struct DeathStarPlans {
        sector: u32,
        shield_active: bool,
        exhaust_ports: u8,
    }

    // Implement Zeroable for DeathStarPlans

    let plans = DeathStarPlans::zeroed();
    println!("Loading: {plans:?}");

}
