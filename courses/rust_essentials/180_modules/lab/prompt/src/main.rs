//! Lab (prompt)
//! Modules
//!
//! Fix all the compile errors below by following the hints provided
//! 
#[allow(dead_code)]
#[allow(unused_variables)]
#[allow(unused_imports)]
fn main() {

    // TASK 1 - Private by Default
    // Hint: functions inside modules are private by default, add the correct keyword to make it visible
    mod cleaner {
        fn perform_cleanup() {
            println!("Cleaning up...");
        }
    }

    cleaner::perform_cleanup();


    // TASK 2 - Visibility at Every Level
    // Hint: making a struct public does not make its fields public, each field needs its own visibility modifier
    mod security {
        pub struct KeyCard {
            ident: u32,
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

    let my_class = school::Students { names: Vec::new() };


    // TASK 4 - Super Keyword
    // Hint: reaching up to the enclosing module requires the super keyword
    mod parent {
        pub fn hello() {}

        pub mod child {
            pub fn call_parent() {
                hello();
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

    let my_plant: Cactus = greenhouse::shelf::Cactus;


    // TASK 6 - Wildcard Import
    // Hint: bring all public items from a module into scope at once using the glob import symbol
    mod networking {
        pub struct TcpStream;
        pub struct UdpSocket;
    }

    let tcp: TcpStream = networking::TcpStream;
    let udp: UdpSocket = networking::UdpSocket;


    // TASK 7 - Renaming With `as`
    // Hint: bringing items with the same name into scope causes a name collision, rename one using the as keyword
    mod system_a {
        pub struct Error;
    }

    mod system_b {
        pub struct Error;
    }

    use system_a::Error;
    use system_b::Error;

    let e1 = Error;

}
