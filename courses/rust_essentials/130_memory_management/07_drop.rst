======
Drop
======

---------------------
Destructor ("Drop")
---------------------

- Deterministic cleanup implemented with :rust:`Drop` Trait
  - Occurs *implicitly* and *exactly* at the closing brace :rust:`}`
- Ideal for resource management, e.g, closing files or network sockets
- Internal fields are dropped in the order they are declared
- Variables are dropped in reverse order of their creation

.. code:: rust

    struct Droppable {
        name: String,
    }

    impl Drop for Droppable {
        fn drop(&mut self) {
            println!("Dropping {}", self.name);
        }
    }

    fn main() {
        let _a = Droppable { name: String::from("a") };
        {
            let _b = Droppable { name: String::from("b") };
        } // Dropping '_b'
    } // Dropping '_a'

---------------
Explicit Drop
---------------

- Early clean-up is possible by calling explicitly :rust:`std::mem::drop`
  - :rust:`std::mem::drop` differs from :rust:`std::ops::Drop::drop` 
  - Calling the :rust:`.drop()` method manually results in a compiler error
- :rust:`std::mem::drop` (in *prelude*) is actually an empty function that 
  - Takes any value by value
  - Takes ownership of the passed value
  - Has no logic, and immediately ends
  - Value goes out of scope, and triggers the Drop mechanism automatically

.. code:: rust

  let x = String::from("Early release");
  drop(x); // x is moved here and dropped immediately

  println!("{}", x);

:error:`error[E0382]: borrow of moved value: 'x'`

.. note::

    :rust:`std::mem::drop` is a convenient way to explicitly drop values earlier than their natural scope end

------------------------------
Exclusivity of Copy and Drop
------------------------------

- A type cannot implement both the Copy and Drop traits
- Copy implies a simple bitwise replication on the stack
- Custom cleanup logic (like freeing heap memory) is incompatible with duplication
  - As multiple owners would attempt to free the same memory
- Compiler prevents coexistence of these traits

.. code:: rust

  // This code will not compile
  #[derive(Copy, Clone)] 
  struct Highlander;

  impl Drop for Highlander {
    fn drop(&mut self) {
        println!("There can be only one!");
    }
  }

:error:`error[E0184]: the trait 'Copy' cannot be implemented for this type; the type has a destructor`
