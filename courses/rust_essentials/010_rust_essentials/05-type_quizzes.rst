==============
Type Quizzes
==============

----------------------------------------
Quiz 1: Is there a compilation error?
----------------------------------------

.. code:: Rust

   fn main() {
       let i: (i32, i32) = [1, 2];
   }

----------------------------------------
Quiz 2: Is there a compilation error?
----------------------------------------

.. code:: Rust

   fn main() {
       let i = [1, 2, 3, 4, 5.0];
   }

----------------------------------------
Quiz 3: Is there a compilation error?
----------------------------------------

.. code:: Rust

   fn main() {
       let i: [i32; 5] = [1, 2, 3, 4, 5];
   }

----------------------------------------
Quiz 4: Is there a compilation error?
----------------------------------------

.. code:: Rust

   fn main() {
       let i: [i32] = [1, 2, 3, 4, 5];
   }

----------------------------------------
Quiz 5: Is there a compilation error?
----------------------------------------

.. code:: Rust

   fn main() {
       let n: int = 5;
       let i: [i32; n] = [1, 2, 3, 4, 5];
   }

----------------------------------------
Quiz 6: Is there a compilation error?
----------------------------------------

.. code:: Rust

   fn main() {
       let a = [1, 2, 3, 4, 5];

       println!("{}", a[10]);
   }

----------------------------------------
Quiz 7: Is there a compilation error?
----------------------------------------

.. code:: Rust

   fn main() {
       let s: String = "Hai";
       println!("{}", s);
   }

----------------------------------------
Quiz 7: Is there a compilation error?
----------------------------------------

.. code:: Rust

   fn main() {
       let s: &str = "Hai";
       let s2: &str = &s[0..2];
       println!("{}", s);
   }

-----------
Functions
-----------

* Main is always called `main`
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

* An expression followed by a semicolon *is a statement*

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
* Two types of borrows: Mutable (only one at a time), and immutable (N at a time)

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
Ownership: mutable references
-------------------------------

.. code:: Rust

   fn main() {
       let mut v: Vec<i32> = vec![1, 2, 3, 4];
       let v2 = &mut v[1..3];
       v2[1] = 13;
       println!("{:?}", v);
   }

--------------------------
Ownership is complicated
--------------------------

* In many case you want to manipulate your data by reference but you can't use references

* In those cases you want to use a managed pointer type: either `Box` (owned) or `Rc` (shared).

* More details later

----------------------------------------
Quiz 1: Is there a compilation error?
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
Quiz 2: Is there a compilation error?
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
Quiz 3: Is there a compilation error?
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
Quiz 4: Is there a compilation error?
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
Quiz 5: Is there a compilation error?
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

---------
Structs
---------

.. code:: Rust

   #[derive(Debug)]
   // Magic that allows you to print structs
   struct Point {
       x: i32,
       // Component of the struct
       y: i32
   }

   fn main() {
       let p = Point { x: 12, y: 12 };
       println!("{:?}", p);

       println!("{}", p.x);
       //             ^ Access the field x

       // You can define mutable structs
       let mut p2 = Point { x: 12, y: 12 };

       // You can mutate fields of structs via dot notation
       p2.x = 15;

       println!("{:?}", p2);
   }

------------------
Structs: methods
------------------

* Rust is not strictly an OOP language
* No inheritance
* No encapsulation
* BUT: You have method syntax :D

------------------
Structs: methods
------------------

.. code:: Rust

   #[derive(Debug)]
   struct Point {
       x: i32, y: i32
   }

   impl Point {
       fn invert(self: &Point) -> Point {
           Point {x: self.y, y: self.x}
       }

       fn double(&mut self) {
           //    ^ Alias for self: &mut Point
           self.x = self.x * 2;
           self.y = self.y * 2;
       }
   }

   fn main() {
       let mut p = Point {x: 1, y: 2};
       p.double();

       println!("{:?}", p);
       println!("{:?}", p.invert());
   }

-------
Enums
-------

* Enums in Rust are very powerful
* Akin to sum types in functional languages
* But can also be used to model simple stuff
* Can also have methods, like structs!

.. code:: Rust

   enum Color {
       Yellow, Red, Green, Blue
   }

   fn main() {
       let y = Color::Yellow;

       match y {
           Color::Yellow => println!("yellow!"),
           Color::Red => println!("red!");
           _ => println!("Other color!");
       }
   }

---------------------
Complex enums (1/2)
---------------------

.. code:: Rust

   #[derive(Debug)]
   enum Operator {
       Plus, Minus, Divide, Multiply
   }

   #[derive(Debug)]
   enum Expr {
       BinOp {
           l: Box<Expr>,
           op: Operator,
           r: Box<Expr>
       },
       Literal(i32)
   }

---------------------
Complex enums (2/2)
---------------------

.. code:: Rust

   fn main() {
       let e =
           Expr::BinOp {
               l: Box::new(
                   Expr::BinOp {
                       l: Box::new(Expr::Literal(12)),
                       op: Operator::Plus,
                       r: Box::new(Expr::Literal(15))
                   }),
               op: Operator::Plus,
               r: Box::new(Expr::Literal(12))
           };

       println!("{:?}", e);
   }
