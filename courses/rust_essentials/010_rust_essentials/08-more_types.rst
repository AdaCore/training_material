============
More Types
============

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
Structs: Methods
------------------

* Rust is not strictly an OOP language
* No inheritance
* No encapsulation
* BUT: You have method syntax :D

------------------
Structs: Methods
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
Complex Enums (1/2)
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
Complex Enums (2/2)
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
