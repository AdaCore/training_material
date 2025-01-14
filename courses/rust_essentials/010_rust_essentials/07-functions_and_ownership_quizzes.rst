=================================
Functions and Ownership Quizzes
=================================

----------------------------------------
Quiz 1: Is There a Compilation Error?
----------------------------------------

.. code:: Rust

   fn factorial(n: i64) -> i64 {
       let mut ret = n;

       for i in 1..n {
           ret = ret * n;
       }

       ret;
   }

----------------------------------------
Quiz 2: Is There a Compilation Error?
----------------------------------------

.. code:: Rust

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

----------------------------------------
Quiz 3: Is There a Compilation Error?
----------------------------------------

.. code:: Rust

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

----------------------------------------
Quiz 4: Is There a Compilation Error?
----------------------------------------

.. code:: Rust

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

----------------------------------------
Quiz 5: Is There a Compilation Error?
----------------------------------------

.. code:: Rust

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
