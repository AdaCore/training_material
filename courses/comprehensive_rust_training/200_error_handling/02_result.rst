================
:rust:`Result`
================

----------------
:rust:`Result`
----------------

Our primary mechanism for error handling in Rust is the
:url:`Result <https://doc.rust-lang.org/stable/std/result/enum.Result.html>`
enum, which we briefly saw when discussing standard library types.

.. code:: rust

   use std::fs::File;
   use std::io::Read;

   fn main() {
       let file: Result<File, std::io::Error> = File::open("diary.txt");
       match file {
           Ok(mut file) => {
               let mut contents = String::new();
               if let Ok(bytes) = file.read_to_string(&mut contents) {
                   println!("Dear diary: {contents} ({bytes} bytes)");
               } else {
                   println!("Could not read file content");
               }
           }
           Err(err) => {
               println!("The diary could not be opened: {err}");
           }
       }
   }

-----------------------------
Dealing with :rust:`Result`
-----------------------------

-  :rust:`Result` has two variants: :rust:`Ok` which contains the success value,
   and :rust:`Err` which contains an error value of some kind.

-  Whether or not a function can produce an error is encoded in the
   function's type signature by having the function return a :rust:`Result`
   value.

-  Like with :rust:`Option`, there is no way to forget to handle an error:
   You cannot access either the success value or the error value without
   first pattern matching on the :rust:`Result` to check which variant you
   have. Methods like :rust:`unwrap` make it easier to write quick-and-dirty
   code that doesn't do robust error handling, but means that you can
   always see in your source code where proper error handling is being
   skipped.

-----------------
More to Explore
-----------------

It may be helpful to compare error handling in Rust to error handling
conventions that students may be familiar with from other programming
languages.

------------
Exceptions
------------

-  Many languages use exceptions, e.g. C++, Java, Python.

-  In most languages with exceptions, whether or not a function can
   throw an exception is not visible as part of its type signature. This
   generally means that you can't tell when calling a function if it may
   throw an exception or not.

-  Exceptions generally unwind the call stack, propagating upward until
   a :rust:`try` block is reached. An error originating deep in the call
   stack may impact an unrelated function further up.

---------------
Error Numbers
---------------

-  Some languages have functions return an error number (or some other
   error value) separately from the successful return value of the
   function. Examples include C and Go.

-  Depending on the language it may be possible to forget to check the
   error value, in which case you may be accessing an uninitialized or
   otherwise invalid success value.
