=========================
Functions and Ownership
=========================

-----------
Functions
-----------

* Main is always called :rust:`main`
* You can put other functions at the top-level in your main source file
* Order doesn't matter

.. code:: Rust

   fn main() {
       println!("Pouet");
       other_function();
   }

   fn other_function() {
       println("Pouet2");
   }

---------------
Functions (2)
---------------

* Functions contain a (possibly empty) sequence of statements, followed by an optional expression

* Expression is used as the return value

* An expression followed by a semicolon is a :dfn:`statement`

.. code:: Rust

   fn fib() -> i32 {
       let mut i = 1;
   
       let mut a = 0;
       let mut b = 1;

       loop {
           let c = a + b;
           a = b;
           b = c;
           i += 1;
           if i > 12 {
               break a;
           }
       }
   }

-----------
Ownership
-----------

.. code:: Rust

   fn double(v: Vec<i32>) -> Vec<i32> {
       v.iter().map(|i| i * 2).collect()
       //           ^ Lambda function
       //                     ^ Convert back to a vector
   }

   fn main() {
       let v: Vec<i32> = vec![1, 2, 3, 4];
       println!("{:?}", double(v));

       println!("{:?}", v); // :(
}
   
-----------
Ownership
-----------

* Defining concept of Rust. Academic concept: Linear/Affine types
* By default, a value cannot be copied, only moved
* If you want to use it you either move it (as in the above example) or *borrow* it
* Two types of :dfn:`borrow`: Mutable (only one at a time), and immutable (N at a time)

.. code:: Rust

   fn double(v: &Vec<i32>) -> Vec<i32> {
       v.iter().map(|i| i * 2).collect()
   }

   fn main() {
       let v: Vec<i32> = vec![1, 2, 3, 4];
       println!("{:?}", double(&v));

       println!("{:?}", v); // :(
   }

-------------------------------
Ownership: Mutable References
-------------------------------

.. code:: Rust

   fn main() {
       let mut v: Vec<i32> = vec![1, 2, 3, 4];
       let v2 = &mut v[1..3];
       v2[1] = 13;
       println!("{:?}", v);
   }

--------------------------
Ownership Is Complicated
--------------------------

* In many case you want to manipulate your data by reference but you can't use references

* In those cases you want to use a managed pointer type: either :rust:`Box` (owned) or :rust:`Rc` (shared).

* More details later
