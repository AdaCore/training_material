================
Move Semantics
================

------------------------
Transferring Ownership
------------------------

- Assigning a value to a new variable transfers ownership
  - Compiler treats a *moved* variable as uninitialized
  - To ensure memory safety and prevent "double-free" errors
  - And forbids any further use of it

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

.. note::

    Applies to non-:rust:`Copy` types only: other types remain valid after assignment

-------------------------
Functions and Ownership
-------------------------

**Passing by value moves data into function's scope**

.. code:: rust

    fn say_hello(name: String) {
        println!("Hello {}", name);
    }

    let name = String::from("Alice");
    say_hello(name);
    say_hello(name); // Error

:error:`error[E0382]: use of moved value: 'name'`

.. note::

    :rust:`name` is *consumed* and can no longer be used