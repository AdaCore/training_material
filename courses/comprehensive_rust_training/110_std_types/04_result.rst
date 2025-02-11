========
Result
========

--------
Result
--------

:rust:`Result` is similar to :rust:`Option`, but indicates the success or
failure of an operation, each with a different enum variant. It is
generic: :rust:`Result<T, E>` where :rust:`T` is used in the :rust:`Ok` variant and
:rust:`E` appears in the :rust:`Err` variant.

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

---------
Details
---------

-  As with :rust:`Option`, the successful value sits inside of :rust:`Result`,
   forcing the developer to explicitly extract it. This encourages error
   checking. In the case where an error should never happen,
   :rust:`unwrap()` or :rust:`expect()` can be called, and this is a signal of
   the developer intent too.
-  :rust:`Result` documentation is a recommended read. Not during the
   course, but it is worth mentioning. It contains a lot of convenience
   methods and functions that help functional-style programming.
-  :rust:`Result` is the standard type to implement error handling as we
   will see on Day 4.
