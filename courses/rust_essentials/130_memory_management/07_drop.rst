======
Drop
======

---------------------
Destructor ("Drop")
---------------------

- Deterministic clean-up implemented with :rust:`Drop` Trait
  - Occurs *implicitly* and usually at the closing brace :rust:`}`
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
        let _tic = Droppable { name: String::from("tic") };
        let _tac = Droppable { name: String::from("tac") };
        {
            let _toe = Droppable { name: String::from("toe") };
        } // Dropping '_toe'
    } // Dropping '_tac' 
      // Dropping '_tic'

---------------
Explicit Drop
---------------

- Early clean-up is possible by calling :rust:`std::mem::drop`
  - :rust:`std::mem::drop` differs from :rust:`std::ops::Drop::drop` 
  - Calling :rust:`.drop()` manually results in a compiler error
- :rust:`std::mem::drop` (in *prelude*) is actually an empty function that 
  - Takes any value by value
  - Takes ownership of the passed value
  - Has no logic, and immediately ends
  - Value goes out of scope, and triggers the :rust:`Drop` mechanism automatically

.. code:: rust

  let x = String::from("Early release");
  drop(x); // 'x' is moved here and dropped immediately

  println!("{}", x);

:error:`error[E0382]: borrow of moved value: 'x'`

.. note::

    :rust:`std::mem::drop` is a way to explicitly drop values before end of scope

----------------------------------
Exclusivity of "Copy" and "Drop"
----------------------------------

- A type cannot implement both the :rust:`Copy` and :rust:`Drop` traits
- :rust:`Copy` implies a simple bitwise replication
- If a type implements :rust:`Drop`, Rust must guarantee the destructor runs exactly once
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
