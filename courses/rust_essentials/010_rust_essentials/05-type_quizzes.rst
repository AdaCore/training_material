==============
Type Quizzes
==============

----------------------------------------
Quiz 1: Is There a Compilation Error?
----------------------------------------

.. code:: Rust
   :number-lines: 2

   fn main() {
       let i: (i32, i32) = [1, 2];
   }

.. container:: animate

   :color-red:`error[E0308]: mismatched types --> src/quiz.rs:3:28`

   :rust:`[1, 2]` is an array but :rust:`i` is a defined as a tuple

----------------------------------------
Quiz 2: Is There a Compilation Error?
----------------------------------------

.. code:: Rust
   :number-lines: 2

   fn main() {
       let i = [1, 2, 3, 4, 5.0];
   }

.. container:: animate

   :color-red:`error[E0308]: mismatched types --> src/quiz.rs:3:29`

   :rust:`i` is an array, so all elements must be of the same type

----------------------------------------
Quiz 3: Is There a Compilation Error?
----------------------------------------

.. code:: Rust
   :number-lines: 2

   fn main() {
       let i: [i32; 5] = [1, 2, 3, 4, 5];
   }

.. container:: animate

   :color-green:`No error`

----------------------------------------
Quiz 4: Is There a Compilation Error?
----------------------------------------

.. code:: Rust
   :number-lines: 2

   fn main() {
       let i: [i32] = [1, 2, 3, 4, 5];
   }

.. container:: animate

   :color-red:`error[E0308]: mismatched types --> src/quiz.rs:3:23`

   :color-red:`error[E0277]: the size for values of type '[i32]' cannot be known at compilation time --> src/quiz.rs:3:12`

   TBD

----------------------------------------
Quiz 5: Is There a Compilation Error?
----------------------------------------

.. code:: Rust
   :number-lines: 2

   fn main() {
       let n: i32 = 5;
       let i: [i32; n] = [1, 2, 3, 4, 5];
   }

.. container:: animate

   :color-red:`error[E0435]: attempt to use a non-constant value --> src/quiz.rs:4:21`

   Size of an array must be a constant :rust:`const` instead of a variable :rust:`let`

----------------------------------------
Quiz 6: Is There a Compilation Error?
----------------------------------------

.. code:: Rust
   :number-lines: 2

   fn main() {
       let a = [1, 2, 3, 4, 5];

       println!("{}", a[10]);
   }

.. container:: animate

   :color-red:`error: this operation will panic at runtime --> src/quiz.rs:5:23`

   Index (10) is out of bounds (array length is 5)

----------------------------------------
Quiz 7: Is There a Compilation Error?
----------------------------------------

.. code:: Rust
   :number-lines: 2

   fn main() {
       let s: String = "Hai";
       println!("{}", s);
   }

.. container:: animate

   :color-red:`error[E0308]: mismatched types --> src/quiz.rs:3:24`

   :rust:`String` is similar to a vector; :rust:`s` should probably be :rust:`&str`

----------------------------------------
Quiz 8: Is There a Compilation Error?
----------------------------------------

.. code:: Rust
   :number-lines: 2

   fn main() {
       let s: &str = "Hai";
       let s2: &str = &s[0..2];
       println!("{}", s);
   }

.. container:: animate

   :color-green:`No error`

