==================
Language Quizzes
==================

----------------------------------------
Quiz 1: Is there a compilation error?
----------------------------------------

.. code:: Rust

   fn main() {
       let a = loop {
           println!("Pouet");
       };

       let b: u32 = a;
   }

----------------------------------------
Quiz 2: Is there a compilation error?
----------------------------------------

.. code:: Rust

   fn main() {
       let a = for n in 1..11 {
           println!("Pouet");
       };
   }

----------------------------------------
Quiz 3: Is there a compilation error?
----------------------------------------

.. code:: Rust

   fn main() {
       let a = for n in 1..11 {
           println!("Pouet");
       };

       let b: u32 = a;
   }

----------------------------------------
Quiz 4: Is there a compilation error?
----------------------------------------

.. code:: Rust

   fn main() {
       let mut i = 1;

       let a = loop {
           println!("Pouet");

           if i > 12 { break; }

           i +=1;
       };

       let b: u32 = a;
   }

----------------------------------------
Quiz 5: Is there a compilation error?
----------------------------------------

.. code:: Rust

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

----------------------------------------
Quiz 6: Is there a compilation error?
----------------------------------------

.. code:: Rust

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

----------------------------------------
Quiz 7: Is there a compilation error?
----------------------------------------

.. code:: Rust

   fn main() {
       let mut i = 100;

       while i {
           i -= 1;

           println!("{}", i);
       }

   }

----------------------------------------
Quiz 8: Is there a compilation error?
----------------------------------------

.. code:: Rust

   fn main() {
       let mut i = 1;

       loop {
           match i {
               1..=5  => println!("i in 1..=5"),
           //  ^ This is a PATTERN
               5 | 12 => break,
               7 | 9 => break,
           }

           i += 1;
       }
   }

