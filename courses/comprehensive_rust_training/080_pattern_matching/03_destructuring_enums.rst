=======
Enums
=======

-------
Enums
-------

Like tuples, enums can also be destructured by matching:

Patterns can also be used to bind variables to parts of your values.
This is how you inspect the structure of your types. Let us start with a
simple :rust:`enum` type:

.. code:: rust

   enum Result {
       Ok(i32),
       Err(String),
   }

   fn divide_in_two(n: i32) -> Result {
       if n % 2 == 0 {
           Result::Ok(n / 2)
       } else {
           Result::Err(format!("cannot divide {n} into two equal parts"))
       }
   }

   fn main() {
       let n = 100;
       match divide_in_two(n) {
           Result::Ok(half) => println!("{n} divided in two is {half}"),
           Result::Err(msg) => println!("sorry, an error happened: {msg}"),
       }
   }

Here we have used the arms to *destructure* the :rust:`Result` value. In the
first arm, :rust:`half` is bound to the value inside the :rust:`Ok` variant. In
the second arm, :rust:`msg` is bound to the error message.

---------
Details
---------

-  The :rust:`if`/:rust:`else` expression is returning an enum that is later
   unpacked with a :rust:`match`.
-  You can try adding a third variant to the enum definition and
   displaying the errors when running the code. Point out the places
   where your code is now inexhaustive and how the compiler tries to
   give you hints.
-  The values in the enum variants can only be accessed after being
   pattern matched.
-  Demonstrate what happens when the search is inexhaustive. Note the
   advantage the Rust compiler provides by confirming when all cases are
   handled.
