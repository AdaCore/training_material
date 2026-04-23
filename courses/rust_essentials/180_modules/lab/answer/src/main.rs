//! Lab (answer)
//! Modules
//!
#[allow(dead_code)]
#[allow(unused_variables)]
#[allow(unused_imports)]
fn main() {

    // TASK 1 - Private by Default
    // Hint: functions inside modules are private by default, add the correct keyword to make it visible
    mod cleaner {
        // Fix: Added `pub` keyword to make the function visible outside the module
        pub fn perform_cleanup() {
            println!("Cleaning up...");
        }
    }

    cleaner::perform_cleanup();


    // TASK 2 - Visibility at Every Level
    // Hint: making a struct public does not make its fields public, each field needs its own visibility modifier
    mod security {
        pub struct KeyCard {
            // Fix: Added `pub` keyword to the specific field
            pub ident: u32,
        }
    }

    let card = security::KeyCard { ident: 1234 };


    // TASK 3 - Encapsulation
    // Hint: private fields prevent direct struct instantiation, use the provided public constructor
    mod school {
        pub struct Students {
            names: Vec<String>,
        }

        impl Students {
            pub fn new() -> Self {
                Self { names: Vec::new() }
            }
        }
    }

    // Fix: Used the provided public constructor instead of trying to instantiate directly
    let my_class = school::Students::new();


    // TASK 4 - Super Keyword
    // Hint: reaching up to the enclosing module requires the super keyword
    mod parent {
        pub fn hello() {}

        pub mod child {
            pub fn call_parent() {
                // Fix: Used `super::` to reach up to the parent module's scope
                super::hello();
            }
        }
    }

    parent::child::call_parent();


    // TASK 5 - The Use Shortcut
    // Hint: long paths can be shortened by bringing the item into scope with the use keyword
    mod greenhouse {
        pub mod shelf {
            pub struct Cactus;
        }
    }

    // Fix: Brought the item into scope using `use`
    use greenhouse::shelf::Cactus;
    let my_plant: Cactus = Cactus;


    // TASK 6 - Wildcard Import
    // Hint: bring all public items from a module into scope at once using the glob import symbol
    mod networking {
        pub struct TcpStream;
        pub struct UdpSocket;
    }

    // Fix: Used the wildcard `*` to import everything from the networking module
    use networking::*;
    
    let tcp: TcpStream = TcpStream;
    let udp: UdpSocket = UdpSocket;


    // TASK 7 - Renaming With `as`
    // Hint: bringing items with the same name into scope causes a name collision, rename one using the as keyword
    mod system_a {
        pub struct Error;
    }

    mod system_b {
        pub struct Error;
    }

    use system_a::Error;
    // Fix: Used the `as` keyword to rename the conflicting item
    use system_b::Error as SysBError;

    let e1 = Error;
    let e2 = SysBError;

}
