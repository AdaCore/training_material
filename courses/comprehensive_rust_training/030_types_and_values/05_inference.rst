================
Type Inference
================

----------------
Type Inference
----------------

Rust will look at how the variable is *used* to determine the type:

.. code:: rust

   fn takes_u32(x: u32) {
       println!("u32: {x}");
   }

   fn takes_i8(y: i8) {
       println!("i8: {y}");
   }

   fn main() {
       let x = 10;
       let y = 20;

       takes_u32(x);
       takes_i8(y);
       // takes_u32(y);
   }

--------------------------
Type Inference Explained
--------------------------

- Compiler infers types based on constraints given by variable declarations and usages.

  - Not some sort of dynamic *any type* that can hold any data.

    - Machine code generated is identical to explicit declaration of a type
    - Compiler does the job for us and helps us write more concise code.

- When nothing constrains the type of an integer literal, Rust defaults to :rust:`i32`.

  - Sometimes appears as :rust:`{integer}` in error messages.
  - Similarly, floating-point literals default to :rust:`f64`.

.. code:: rust

   fn main() {
       let x = 3.14;
       let y = 20;
       assert_eq!(x, y);
       // ERROR: no implementation for `{float} == {integer}`
   }
