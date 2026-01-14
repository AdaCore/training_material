=========
Methods
=========

-----------------
Methods in Rust
-----------------

* What is a :dfn:`method?`

  * Function *associated* with type via :rust:`impl` block

  * Syntax: :rust:`instance.method(...)`

  * First parameter :dfn:`(receiver)` determines how the method uses the value

* Why use methods?

  * Organize behavior with the data it operates on

  * Makes code more ergonomic and readable

------------------
Method Receivers
------------------

.. list-table::
  :header-rows: 1

  * - Receiver
    - Meaning

  * - :rust:`&self`
    - Shared, read-only borrow

  * - :rust:`&mut self`
    - Unique, mutable borrow

  * - :rust:`self`
    - Takes ownership

  * - :rust:`mut self`
    - Takes and mutably uses ownership

  * - *No receiver*
    - Static method (like a constructor)

--------------------
Example of Methods
--------------------

.. container:: latex_environment footnotesize

  .. container:: columns

    .. container:: column

      **Type Definition**

      .. code:: rust

        // some type
        struct CarRace {
            name: String,
            laps: Vec<i32>,
        }

    .. container:: column

      **Methods**

      .. code:: rust

        // methods for the type
        impl CarRace {
            // no receiver (constructor)
            fn new(name: &str) -> Self { … }
            // mutable borrow
            fn add_lap(&mut self, lap: i32) { … }
            // shared borrow
            fn print_laps(&self) { … }
            // take ownership
            fn finish(self) { … }
        }

  **Usage**

  .. code:: rust

    let mut race = CarRace::new("Monaco Grand Prix");
    race.add_lap(70);
    race.print_laps();
    race.finish();
