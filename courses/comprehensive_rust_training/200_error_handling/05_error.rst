=====================
Dynamic Error Types
=====================

---------------------
Dynamic Error Types
---------------------

Sometimes we want to allow any type of error to be returned without
writing our own enum covering all the different possibilities. The
:rust:`std::error::Error` trait makes it easy to create a trait object that
can contain any error.

.. code:: rust

   use std::error::Error;
   use std::fs;
   use std::io::Read;

   fn read_count(path: &str) -> Result<i32, Box<dyn Error>> {
       let mut count_str = String::new();
       fs::File::open(path)?.read_to_string(&mut count_str)?;
       let count: i32 = count_str.parse()?;
       Ok(count)
   }

   fn main() {
       fs::write("count.dat", "1i3").unwrap();
       match read_count("count.dat") {
           Ok(count) => println!("Count: {count}"),
           Err(err) => println!("Error: {err}"),
       }
   }

.. raw:: html

---------
Details
---------

The :rust:`read_count` function can return :rust:`std::io::Error` (from file
operations) or :rust:`std::num::ParseIntError` (from :rust:`String::parse`).

Boxing errors saves on code, but gives up the ability to cleanly handle
different error cases differently in the program. As such it's generally
not a good idea to use :rust:`Box<dyn Error>` in the public API of a
library, but it can be a good option in a program where you just want to
display the error message somewhere.

Make sure to implement the :rust:`std::error::Error` trait when defining a
custom error type so it can be boxed.

.. raw:: html

