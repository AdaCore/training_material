=============================
Traits And Generics Quizzes
=============================

---------------------------------------
Quiz 1: Is There a Compilation Error?
---------------------------------------

.. code:: Rust

   fn largest<T>(list: &[T]) -> &T {
       let mut largest = &list[0];

       for item in list {
           if item > largest {
               largest = item;
           }
       }

       largest
   }

---------------------------------------
Quiz 2: Is There a Compilation Error?
---------------------------------------

.. code:: Rust

   fn smallest <'a> (a: &'a str, b: &'a str) -> &'a str {
       if a < b { a } else { b }
   }

   fn main() {
       let a = "hello";
       let c;
       {
           let b = "world";
           c = smallest(b, a);
           println!("{}", c);
           let d = b;
       }
       println!("{}", c);
   }

---------------------------------------
Quiz 3: Is There a Compilation Error?
---------------------------------------

.. code:: Rust

   #[derive(Debug)]
   struct Person<'a> {
       first: &'a str,
       last: &'a str
   }

   fn main() {
       let first = "Raphael".to_string();
       let p;

       {
           let last = "Amiard".to_string();
           p = Person { first: &first, last: &last };
       }
       println!("{:?}", p);
   }

---------------------------------------
Quiz 4: Is There a Compilation Error?
---------------------------------------

.. code:: Rust

   #[derive(Debug)]
   struct Person<'a> {
       first: &'a str,
       last: &'a str
   }

   fn main() {
       let first = "Raphael".to_string();
       let p;

       {
           let last = "Amiard".to_string();
           p = Person { first: &first, last: &last };
           println!("{:?}", p);
       }
   }

