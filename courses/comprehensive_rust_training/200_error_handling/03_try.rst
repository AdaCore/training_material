==============
Try Operator
==============

--------------
Try Operator
--------------

Runtime errors like connection-refused or file-not-found are handled
with the :rust:`Result` type, but matching this type on every call can be
cumbersome. The try-operator :rust:`?` is used to return errors to the
caller. It lets you turn the common

.. code:: rust

   match some_expression {
       Ok(value) => value,
       Err(err) => return Err(err),
   }

into the much simpler

.. code:: rust

   some_expression?

We can use this to simplify our error handling code:

.. code:: rust

   use std::io::Read;
   use std::{fs, io};

   fn read_username(path: &str) -> Result<String, io::Error> {
       let username_file_result = fs::File::open(path);
       let mut username_file = match username_file_result {
           Ok(file) => file,
           Err(err) => return Err(err),
       };

       let mut username = String::new();
       match username_file.read_to_string(&mut username) {
           Ok(_) => Ok(username),
           Err(err) => Err(err),
       }
   }

   fn main() {
       //fs::write("config.dat", "alice").unwrap();
       let username = read_username("config.dat");
       println!("username or error: {username:?}");
   }

.. raw:: html

---------
Details
---------

Simplify the :rust:`read_username` function to use :rust:`?`.

Key points:

-  The :rust:`username` variable can be either :rust:`Ok(string)` or
   :rust:`Err(error)`.
-  Use the :rust:`fs::write` call to test out the different scenarios: no
   file, empty file, file with username.
-  Note that :rust:`main` can return a :rust:`Result<(), E>` as long as it
   implements :rust:`std::process::Termination`. In practice, this means
   that :rust:`E` implements :rust:`Debug`. The executable will print the
   :rust:`Err` variant and return a nonzero exit status on error.

.. raw:: html

