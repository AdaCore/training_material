========
Option
========

----------------------
Why Use "Option<T>"?
----------------------

* In many languages, a variable can be a value or *unknown*

  * Null pointer
  * Uninitialized

* If we don't handle these cases, we can get bugs

  * Which can be difficult to find

* In Rust, a variable is *always* its type

  * So the value is always valid
  * To allow for *unknown* we wrap the type in :rust:`Option`
  * Requires explicit handling before accessing the value

    * Must detemine if value is :rust:`None` or :rust:`Some`
    * Then extract value if :rust:`Some` 

----------------------
What Is "Option<T>"?
----------------------

* :rust:`Option<T>` is defined as an :rust:`enum` with two variants

  .. code:: rust

    enum Option<T> {
        Some(T),  // Contains a value of type 'T'
        None,     // Represents the absence of a value
    }

* Typical usage

  .. code:: rust

    fn find_user(id: u32) -> Option<String>;

    fn main() {
        let user_id = 1;
    
        match find_user(user_id) {
            Some(name) => println!("Found user: {}", name),
            None => println!("User not found."),
        }
    }

:command:`Found user: Waldo`

.. note::

  :rust:`Option` is useful when there is no resultant value

----------------------
Benefits of "Option"
----------------------

* Safety and clarity

  * No hidden null pointer 
  * Must explicitly handle absence of value

* Type system enforces handling of missing values

  * Cannot just ignore them
  * Fewer runtime surprises

.. note::

  Rust replaces *null* with :rust:`Option` so "nothing" can’t panic behind your back

------------------
Common Use Cases
------------------

* Optional arguments

  * Configuration data where a value might not be specified

  .. code:: rust

    struct Arguments {
        // Always there
        source_file: String,
        // Sometimes there
        output_file: Option<String>,
    }

* Collection lookups

  * Looking for something in a database that might not be there

  .. code:: rust

    fn find_user(id: u32) -> Option<User> {
        // Returns 'None' if 'User' doesn't exist
    }

* Functions that may not return a result

  * Popping from an empty stack

  .. code:: rust

    let value = stack.pop();

