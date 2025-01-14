=====================
Procedural language
=====================

--------------------------------
First, a note about philosophy
--------------------------------

* In C/C++, very weak distinction between statements and expressions

    - You can use exprs as statements

* In Ada, strong distinction between statements and expressions

    - Statements are statements, expressions are expressions, not interchangeable
    - Procedures and functions are distinct

* In Rust, everything is an expression (and you generally cannot ignore their value)

    - Simpler than Ada, (much) safer than C/C++
    - But not always obvious what an expression returns
    - Complex type system tricks to make it work (what's the type of a loop?)

-----------
For loops
-----------

.. code:: Rust

   fn main() {
       for i in 1..10 {
           //   ^ Range object (of type Range)
           println!("Hello, World!");
       }
   }

-------------
While loops
-------------

.. code:: Rust

   fn main() {
       let mut i = 1;
       //  ^ Declare a mutable variable (immutable by default)

       // No parens around condition
       while i < 10 {
           println!("Hello, World!");
           i += 1; // increment
       }
   }

----------------
Infinite loops
----------------

.. code:: Rust

   fn main() {
       let mut i = 1;

       loop {
           println!("Hello, World!");
           i += 1; // increment

           if i == 5 {
           //   ^ equality operator
               break;
           }
       }
   }

-------------------------------
Infinite loop w. return value
-------------------------------

.. code:: Rust

   fn main() {
       let mut i = 1;

       let mut a = 0;
       let mut b = 1;

       let res = loop {
           let c = a + b;
           a = b;
           b = c;
           i += 1;
           if i > 12 {
               break a;
           }
       };
    println!("{}", res);
   }

---------
If/else
---------

.. code:: Rust

   fn main() {
       let mut i = 1;
       loop {
           if i == 5 || else i == 12 {
               break;
           } else if i < 5 && i > 2 {
               println!("I = 3 or 4");
           } else {
               println!("Hello, World!");
           }
       }
   }

--------------------------
If/else as an expression
--------------------------

.. code:: Rust

   fn main() {
       let number = if true { 5 } else { 6 };

       let error = if true { 5 } else { "six" };
   }

------------------
Match expression
------------------

.. code:: Rust

   fn main() {
       let mut i = 1;

       loop {
           match i {
               5 | 12 => break,
               1..=4  => println!("i in 1..4"),
               7 | 9 => break,
               _ => println!("Hello, World!")
           }

           i += 1;
       }
   }

