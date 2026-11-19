//! Lab (answer)
//! Unsafe Rust
//!
#[allow(dead_code)]
#[allow(unused_variables)]
fn main() {

    // TASK 1 - Dereferencing Raw Pointers
    // Hint: Dereferencing a raw pointer is an unsafe operation
    let mut ellen_ripley = 0;
    let face_hugger = &raw mut ellen_ripley;

    unsafe {
        *face_hugger = 426;
        println!("Ripley status: {}", *face_hugger);
    }

    // TASK 2 - Creating Raw Pointers via Conversion
    // Hint: Cast from a reference using "as"
    let treasure: i32 = 5;
    let reference = &treasure;
    let clue = reference as *const i32;

    unsafe {
        println!("clue: {}", *clue);
    }

    // TASK 3 - Aliasing and Safety Obligations
    // Hint: Raw pointers may alias the same memory without a compilation error
    let mut coins = 0;
    let mario = &raw mut coins;
    let luigi = mario;

    unsafe {
        *mario = 64;
        *luigi = 120;
    }

    println!("Number of coins: {}", coins);

    // TASK 4 - Declaring an Unsafe Function
    // Hint: Calling an unsafe function requires an unsafe context
    // Bonus: add the safety contracts for the function and caller
    /// # Safety
    ///
    /// 'value' must be 'Some'
    unsafe fn get_some(value: Option<i32>) -> i32 {
        // SAFETY: Caller guarantees 'value' is 'Some'
        unsafe { value.unwrap_unchecked() }
    }
    // SAFETY: The argument is 'Some', as required
    let universal_answer = unsafe { get_some(Some(42)) };
    println!("The universal answer is {universal_answer}");

    // TASK 5 - Updating a Mutable Static
    // Hint: Reading or writing requires an unsafe context
    // Bonus: add the safety contracts for the function and caller

    static mut RAPTORS_LOOSE: u32 = 0;

    /// # Safety
    /// Access to 'RAPTORS_LOOSE' must be exclusive
    unsafe fn release_raptor() {
        // SAFETY: Required by this function's contract
        unsafe {
            RAPTORS_LOOSE += 1;
        }
    }

    // SAFETY: This example is single-threaded, with no competing access
    // to 'RAPTORS_LOOSE' during the call or subsequent read
    unsafe {
        release_raptor();
        let raptors = RAPTORS_LOOSE;
        println!("Raptors loose: {raptors}");
    }

    // TASK 6 - Reading Union Fields
    // Hint: Reading is an unsafe operation
    #[repr(C)]
    union Avenger {
        banner: i8,
        hulk: u8,
    }
    let mut avenger = Avenger { banner: -1 };

    avenger.hulk = 254;
    let hulk = unsafe { avenger.hulk };
    println!("Hulk: {hulk}");
    let banner = unsafe { avenger.banner };
    println!("Banner: {banner}");


    // TASK 7 - Encapsulating the Unsafe Read
    // Hint: A safe method can perform an unsafe operation
    // Bonus: add the safety contracts for the internal unsafe operation
    impl Avenger {
       fn as_unsigned(&self) -> u8 {
            // SAFETY: Either field initializes the shared byte,
            // and every bit pattern is valid for 'u8'
            unsafe { self.hulk }
       }
    }
    let hulk_strength = avenger.as_unsigned();
    println! ("Hulk strength is : {hulk_strength}");

    // TASK 8 - Implementing an Unsafe Trait
    // Hint: Implementing an unsafe trait requires an unsafe impl block
    // Bonus: Add the safety contract explaining why the implementation is valid
    unsafe trait WarpDrive {
        fn engage(&self);
    }

    struct MillenniumFalcon {
        parsecs: u32,
    }

    // SAFETY: MillenniumFalcon holds valid, initialized fields safe for warp
    unsafe impl WarpDrive for MillenniumFalcon {
        fn engage(&self) {
            println!("Making the Kessel Run in {} parsecs!", self.parsecs);
        }
    }

    let falcon = MillenniumFalcon { parsecs: 12 };
    falcon.engage();


}
