=====================
Traits and Generics
=====================

----------
Generics
----------

.. code:: Rust

   struct LinkedList<T> {
       item: T,
       next: Box<LinkedList<T>>
   }

* Like Java/C# generics: abstract over types, functions, not packages
* Like Ada (& others): legality checked in the generic form
* Operations need to be made available on types (via traits)

--------------
Generics (2)
--------------

.. code:: Rust

   struct HashTable<T> { ... }

   impl HashTable<T> {
       fn add(&self, item: T) {
           // problem: how do we hash elements?
       }
   }

--------
Traits
--------

* Traits define common behavior
* Very similar to interfaces in Java/C#/etc
* But first and foremost a generic concept

.. code:: Rust

   trait Hashable {
       fn hash() -> i32;
   }

   struct HashTable<T: Hashable> { }
   //                  ^ Trait bound

   impl HashTable<T> {
       fn add(&self, item: T) {
           ...
           let hash = item.hash();
           ...
       }
   }

-----------------------------------------
Shorthand for trait bounds in functions
-----------------------------------------

.. code:: Rust

   fn display_list<T: Display>(list: &[T]) {
       for el in list {
           print!("{el}");
       }
   }

   // Shorthand:

   fn display_list(list: &[impl Display]) ...
   // This function is a GENERIC function

----------------------
Some built-in traits
----------------------

* Rust has a lot of built-in traits that are part of the standard library
* Some of those are derivable: The compiler can provide an implementation for you automatically.

* `Debug`: use to display a value using the `{:?}` formatter
* Ordering traits like `Eq`, `Ord` are used to compare values
* `Copy` and `Clone`, allow different copy semantics for your type.
* `Hash` computes a hash for your type

To derive:

.. code:: Rust

   #[derive(Hash, Debug)]
   struct Point {
       x: i32, y: i32
   }
   // This struct is now hashable and displayable via the Debug trait

--------------
Copy & Clone
--------------

* The `Clone` trait adds a `clone` function on your type, that allows you to clone an instance of it.

* The `Copy` trait, on the other hand, gives full copy semantics to your type (like you have by default on scalar types).

.. code:: Rust

   #[derive(Copy, Debug)]
   struct Point {
       x: i32, y: i32
   }

   fn main() {
       let p = Point { x = 1, y = 2 };
       let p2 = p;

       println!("{:?}", p);
       // WHAT IS THIS SORCERY
   }

-------------------
Dyn trait objects
-------------------

* You can store any object implementing a trait via the `dyn` qualifier, creating a trait object

.. code:: Rust

   use std::fmt::Debug;

   fn main() {
       let a: Vec<Box<dyn Debug>> = vec![
           Box::new(12),
           Box::new("pouet"),
           Box::new((1, 2))
       ];
       println!("{:?}", a);
   }

-----------
Lifetimes
-----------

Ownership is a combination of three things:

* Basic rules of ownership (one owner, N borrowers, etc)
* Lifetimes for every value. For the moment, all lifetimes were infered.
* The borrow checker: checks that borrows don't outlive the lifetime of the value they borrow

Turns out you can actually specify lifetimes yourself, allowing you to express
things that weren't possible before:

.. code:: Rust

   // Won't work: can't return reference without explicit lifetime
   fn smallest (a: &str, b: &str) -> &str {
       if a < b { a } else { b }
   }

   // Works
   fn smallest <'a> (a: &'a str, b: &'a str) -> &'a str {
       if a < b { a } else { b }
   }

---------------
Lifetimes (2)
---------------

.. code:: Rust

   fn smallest <'a> (a: &'a str, b: &'a str) -> &'a str {
       if a < b { a } else { b }
   }

   fn main() {
       let a = String::from("hello");     // <-| Lifetime for a
       let c;                             //   |
       {                                  //   |
           let b = String::from("world"); //   | <-| Lifetime for b (and hence for c)
           c = smallest(&b, &a);          //   |   |
           println!("{}", c);             //   | <-|
       }                                  //   |
       println!("{}", c);                 // <--
   }

---------------
Lifetimes (3)
---------------

* Lifetimes are generic parameters, so functions using lifetimes are actually generic functions
* Structs using lifetimes are also generic types. If you want to use a reference in a struct, you need to annotate lifetimes

.. code:: Rust

   struct Person<'a> {
       first: &'a str,
       last: &'a str
   }

------------------
Lifetime elision
------------------

.. code:: Rust

   // This works thanks to lifetime elision
   fn identity(s: &str) -> &str {
       s
   }

* Each parameter gets its own lifetime (input lifetimes)

* If there is one input lifetime and one output lifetime, the output lifetime gets assigned to the input lifetime

* If there are multiple params, but one of them is &self or &mut self, then the output lifetime gets assigned this lifetime

