================
Error handling
================

-------
PANIC
-------

* Rust has no exceptions
* The closest thing it has is unrecoverable errors (via `panic!`)
* Obviously not a solution for robust applications

.. code:: Rust

   fn main() {
       let v = vec![1, 2, 3];

       v[99]; // PANIC
   }

------------
Backtraces
------------

When your program panics, running it with `RUST_BACKTRACE=1` will show you a backtrace:

::

   $ RUST_BACKTRACE=1 cargo run
   thread 'main' panicked at 'index out of bounds: the len is 3 but the index is 99', src/main.rs:4:5
   stack backtrace:
      0: rust_begin_unwind
                at /rustc/3b348d932aa5c9884310d025cf7c516023fd0d9a/library/std/src/panicking.rs:584:5
      1: core::panicking::panic_fmt
                at /rustc/3b348d932aa5c9884310d025cf7c516023fd0d9a/library/core/src/panicking.rs:143:14
      2: core::panicking::panic_bounds_check
                at /rustc/3b348d932aa5c9884310d025cf7c516023fd0d9a/library/core/src/panicking.rs:85:5
      3: <usize as core::slice::index::SliceIndex<[T]>>::index
                at /rustc/3b348d932aa5c9884310d025cf7c516023fd0d9a/library/core/src/slice/index.rs:189:10
      4: core::slice::index::<impl core::ops::index::Index<I> for [T]>::index
                at /rustc/3b348d932aa5c9884310d025cf7c516023fd0d9a/library/core/src/slice/index.rs:15:9
      5: <alloc::vec::Vec<T,A> as core::ops::index::Index<I>>::index
                at /rustc/3b348d932aa5c9884310d025cf7c516023fd0d9a/library/alloc/src/vec/mod.rs:2531:9
      6: test_epita::main
                at ./src/main.rs:4:5
      7: core::ops::function::FnOnce::call_once
                at /rustc/3b348d932aa5c9884310d025cf7c516023fd0d9a/library/core/src/ops/function.rs:227:5

--------
Result
--------

* Proper way to handle errors is via the `Result<T>` type (shown earlier).
* TIP: Main can return a `Result` (but only with () as an OK type):
* Rust provides the `?` operator for easy(er) error handling

.. code:: Rust

   use std::num::ParseIntError;

   fn main() -> Result<(), ParseIntError> {
       let number_str = "10a";

       let n = number_str.parse::<i32>()?;
       //                               ^ Either unwrap, or return the error result
       println!("{}", n);
       Ok(())
   }

------------
Result (2)
------------

You can also use early return for easier error handling

.. code:: Rust

   use std::num::ParseIntError;

   fn main() -> Result<(), ParseIntError> {
       let numbers = ["12", "15", "18a"];
       let mut n = 0;

       for num in numbers {
           match num.parse::<i32>() {
               Ok(val) => { n += val; }
               Err(e) => {
                   return Err(e);
               }
           }
       };

       Ok(())
   }

