=============================
Traits And Generics Quizzes
=============================

---------------------------------------
Quiz 1: Is There a Compilation Error?
---------------------------------------

.. code:: Rust
   :number-lines: 2

   fn largest<T>(list: &[T]) -> &T {
       let mut largest = &list[0];

       for item in list {
           if item > largest {
               largest = item;
           }
       }

       largest
   }

.. container:: animate

   :color-red:`error[E0369]: binary operation '>' cannot be applied to type '&T' --> src/quiz.rs:6:20`

   TBD

---------------------------------------
Quiz 2: Is There a Compilation Error?
---------------------------------------

.. code:: Rust
   :number-lines: 2

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

.. container:: animate

   :color-green:`No error`

---------------------------------------
Quiz 3: Is There a Compilation Error?
---------------------------------------

.. code:: Rust
   :number-lines: 2

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

.. container:: animate

   :color-red:`error[E0597]: `last` does not live long enough --> src/quiz.rs:14:46`

   TBD

---------------------------------------
Quiz 4: Is There a Compilation Error?
---------------------------------------

.. code:: Rust
   :number-lines: 2

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

.. container:: animate

   :color-green:`No error`
