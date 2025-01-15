==================
Language Quizzes
==================

----------------------------------------
Quiz 1: Is There a Compilation Error?
----------------------------------------

.. code:: Rust
   :number-lines: 2

   fn main() {
       let a = loop {
           println!("Pouet");
       };

       let b: u32 = a;
   }

.. container:: animate

   :color-green:`No error`

   *But you may get a warning that line 7 is unreachable*

----------------------------------------
Quiz 2: Is There a Compilation Error?
----------------------------------------

.. code:: Rust
   :number-lines: 2

   fn main() {
       let a = for n in 1..11 {
           println!("Pouet");
       };
   }

.. container:: animate

   :color-green:`No error`

----------------------------------------
Quiz 3: Is There a Compilation Error?
----------------------------------------

.. code:: Rust
   :number-lines: 2

   fn main() {
       let a = for n in 1..11 {
           println!("Pouet");
       };

       let b: u32 = a;
   }

.. container:: animate

   :color-red:`error[E0308]: mismatched types --> src/quiz.rs:7:21`

   Types of :rust:`a` and :rust:`b` are not the same

----------------------------------------
Quiz 4: Is There a Compilation Error?
----------------------------------------

.. code:: Rust
   :number-lines: 2

   fn main() {
       let mut i = 1;
       loop {
           println!(
               "{}",
               if i == 5 || i == 12 { "5 or 12" }
               else { "everything else" }
           );

           i += 1;
       };
   }

.. container:: animate

   :color-green:`No error`

----------------------------------------
Quiz 5: Is There a Compilation Error?
----------------------------------------

.. code:: Rust
   :number-lines: 2

   fn main() {
       let mut i = 1;

       loop {
           println!(
               "{}",
               if i == 5 || i == 12 { "5 or 12" }
               else if i == 15 { "15" }
           );

           i += 1;
       };
   }

.. container:: animate

   :color-red:`error[E0317]: 'if' may be missing an 'else' clause --> src/quiz.rs:9:21`

   :rust:`if` expressions without :rust:`else` evaluate to :rust:`()` which is not a valid value for :rust:`println`

----------------------------------------
Quiz 6: Is There a Compilation Error?
----------------------------------------

.. code:: Rust
   :number-lines: 2

   fn main() {
       let mut i = 100;

       while i {
           i -= 1;

           println!("{}", i);
       }

   }

.. container:: animate

   :color-red:`error[E0308]: mismatched types --> src/quiz.rs:5:14`

   :rust:`while` condition expects a boolean value, but :rust:`i` is an integer
