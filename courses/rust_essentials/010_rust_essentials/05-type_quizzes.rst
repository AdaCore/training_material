==============
Type Quizzes
==============

----------------------------------------
Quiz 1: Is There a Compilation Error?
----------------------------------------

.. code:: Rust

   fn main() {
       let i: (i32, i32) = [1, 2];
   }

----------------------------------------
Quiz 2: Is There a Compilation Error?
----------------------------------------

.. code:: Rust

   fn main() {
       let i = [1, 2, 3, 4, 5.0];
   }

----------------------------------------
Quiz 3: Is There a Compilation Error?
----------------------------------------

.. code:: Rust

   fn main() {
       let i: [i32; 5] = [1, 2, 3, 4, 5];
   }

----------------------------------------
Quiz 4: Is There a Compilation Error?
----------------------------------------

.. code:: Rust

   fn main() {
       let i: [i32] = [1, 2, 3, 4, 5];
   }

----------------------------------------
Quiz 5: Is There a Compilation Error?
----------------------------------------

.. code:: Rust

   fn main() {
       let n: int = 5;
       let i: [i32; n] = [1, 2, 3, 4, 5];
   }

----------------------------------------
Quiz 6: Is There a Compilation Error?
----------------------------------------

.. code:: Rust

   fn main() {
       let a = [1, 2, 3, 4, 5];

       println!("{}", a[10]);
   }

----------------------------------------
Quiz 7: Is There a Compilation Error?
----------------------------------------

.. code:: Rust

   fn main() {
       let s: String = "Hai";
       println!("{}", s);
   }

----------------------------------------
Quiz 8: Is There a Compilation Error?
----------------------------------------

.. code:: Rust

   fn main() {
       let s: &str = "Hai";
       let s2: &str = &s[0..2];
       println!("{}", s);
   }
