============
Arithmetic
============

------------
Arithmetic
------------

.. code:: rust

   fn interproduct(a: i32, b: i32, c: i32) -> i32 {
       return a * b + b * c + c * a;
   }

   fn main() {
       println!("result: {}", interproduct(120, 100, 248));
   }

--------------------
Some Explanations
--------------------

- :rust:`interproduct` is a function - just like :rust:`main`

  - Takes three integers
  - Returns an integer
  - Functions will be covered in more detail later.

- Arithmetic is very similar to other languages, with similar precedence.

- Integer overflow

  - In C and C++ overflow of *signed* integers is undefined

    - Might do unknown things at runtime

  - In Rust, it's defined.

    - Change :rust:`i32` to :rust:`i16` to see an integer overflow
    - Panics (checked) in a debug build, wraps in a release build.
    - Other options (overflowing, saturating, carrying) using method syntax

      .. code:: rust

         (a * b).saturating_add(b * c).saturating_add(c * a)
