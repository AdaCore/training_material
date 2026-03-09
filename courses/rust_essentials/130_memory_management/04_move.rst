================
Move Semantics
================

------------------------
Transferring Ownership
------------------------

Assigning a value to a new variable transfers ownership

.. code:: rust

    let s1 = String::from("Hello");
    let s2 = s1;

    println!("{}", s1); // Error
    println!("s2: {}", s2);

:error:`error[E0382]: borrow of moved value: 's1'`

Before move to :rust:`s2`:

.. image:: comprehensive_rust_training/review_of_program_memory.svg

After move to :rust:`s2`:

.. image:: comprehensive_rust_training/move_semantics_2.svg


-------------------------
Functions and Ownership
-------------------------

- Passing by value moves the data into the function's scope

.. code:: rust

    fn main() {
        fn say_hello(name: String) {
            println!("Hello {}", name);
        }

        let name = String::from("Alice");
        say_hello(name);
        say_hello(name); // Error
    }

:error:`error[E0382]: use of moved value: 'name'`

.. note::

    The :rust:`name` variable in :rust:`main` is consumed and can no longer be used
