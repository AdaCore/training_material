=================================
Functions and Ownership Quizzes
=================================

----------------------------------------
Quiz 1: Is There a Compilation Error?
----------------------------------------

.. code:: Rust
   :number-lines: 2

   fn factorial(n: i64) -> i64 {
       let mut ret = n;

       for i in 1..n {
           ret = ret * n;
       }

       ret;
   }

.. container:: animate

   :color-red:`error[E0308]: mismatched types --> src/quiz.rs:2:28`

   Function has no return *expression* - need to remove the ";" from line 9

----------------------------------------
Quiz 2: Is There a Compilation Error?
----------------------------------------

.. code:: Rust
   :number-lines: 2

   fn double(v: &mut Vec<i32>) {
       for i in 0..v.len() {
           v[i] = v[i] * 2;
       }
   }

   fn main() {
       let v: Vec<i32> = vec![1, 2, 3, 4];
       double(&v);

       println!("{:?}", v); // :(
   }

.. container:: animate

   :color-red:`error[E0308]: mismatched types --> src/quiz.rs:10:15`

   Actual parameter in call to :rust:`double` has different mutability than formal parameter

----------------------------------------
Quiz 3: Is There a Compilation Error?
----------------------------------------

.. code:: Rust
   :number-lines: 2

   fn double(v: &mut Vec<i32>) {
       for i in 0..v.len() {
           v[i] = v[i] * 2;
       }
   }

   fn main() {
       let mut v: Vec<i32> = vec![1, 2, 3, 4];
       double(&v);

       println!("{:?}", v); // :(
   }

.. container:: animate

   :color-red:`error[E0308]: mismatched types --> src/quiz.rs:10:15`

   TBD

----------------------------------------
Quiz 4: Is There a Compilation Error?
----------------------------------------

.. code:: Rust
   :number-lines: 2

   fn double(v: &mut Vec<i32>) {
       for i in 0..v.len() {
           v[i] = v[i] * 2;
       }
   }

   fn main() {
       let mut v: Vec<i32> = vec![1, 2, 3, 4];

       let v2 = &mut v;
       double(v2);

       let v3 = &mut v;
       double(v3);

       println!("{:?}", v); // :(
   }

.. container:: animate

   :color-green:`No error`

----------------------------------------
Quiz 5: Is There a Compilation Error?
----------------------------------------

.. code:: Rust
   :number-lines: 2

   fn double(v: &mut Vec<i32>) {
       for i in 0..v.len() {
           v[i] = v[i] * 2;
       }
   }

   fn main() {
       let mut v: Vec<i32> = vec![1, 2, 3, 4];

       let v2 = &mut v;
       double(v2);

       let v3 = &mut v;
       double(v3);

       println!("{:?}", v2); // :(
   }

.. container:: animate

   :color-red:`error[E0499]: cannot borrow 'v' as mutable more than once at a time --> src/quiz.rs:14:17`

   :rust:`v2` already took ownership of :rust:`v` so :rust:`v3` cannot also take it
