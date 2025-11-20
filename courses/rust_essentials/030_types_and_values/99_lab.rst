=================================
Exercise: Calculate Total Price
=================================

-------------
The Problem
-------------

**Goal:** Calculate the total price for an order

- The code below will not compile!

- Read the compiler error and use what you've learned about 
  **Numberic Strictness** and **Casting** to fix it

.. code:: rust

    fn main() {
        let item_count: i32 = 15;
        let price_per_item: f64 = 4.99;

        // ERROR!
        let total_price = item_count * price_per_item;

        println!("Total items: {item_count}");
        println!("Total price: ${total_price}");
    }

--------------
The Solution
--------------

- The error was :error:`cannot multiply 'i32' by 'f64'`

- To fix it, you must **expicitly cast** the :rust:`i32` to an
  :rust:`f64` using the :rust:`as` keyword

.. code:: rust

    fn main() {
        let item_count: i32 = 15;
        let price_per_item: f64 = 4.99;

        // We cast the integer to a float to match the price
        let total_price = (item_count as f64) * price_per_item;

        println!("Total items: {item_count}");
        println!("Total price: ${total_price}");
    }
