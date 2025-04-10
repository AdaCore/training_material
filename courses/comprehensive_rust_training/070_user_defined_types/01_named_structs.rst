===============
Named Structs
===============

---------------
Named Structs
---------------

Like C and C++, Rust has support for custom structs:

.. code:: rust

   struct Person {
       name: String,
       age: u8,
   }

   fn describe(person: &Person) {
       println!("{} is {} years old", person.name, person.age);
   }

   fn main() {
       let mut peter = Person { name: String::from("Peter"), age: 27 };
       describe(&peter);

       peter.age = 28;
       describe(&peter);

       let name = String::from("Avery");
       let age = 39;
       let avery = Person { name, age };
       describe(&avery);

       let jackie = Person { name: String::from("Jackie"), ..avery };
       describe(&jackie);
   }

-----------------
Some Key Points
-----------------

- Structs work like in C or C++.

   - Like in C++, and unlike in C, no typedef is needed to define a
     type.
   - Unlike in C++, there is no inheritance between structs.

- Different types of structs

   - Zero-sized structs (e.g. :rust:`struct Foo;`)

     - Useful for implementing a trait on a type

   - Tuple structs

     - Useful when field names are not important.

.. container:: speakernote

  -  If you already have variables with the right names, then you can
     create the struct using a shorthand.
  -  The syntax :rust:`..avery` allows us to copy the majority of the fields
     from the old struct without having to explicitly type it all out. It
     must always be the last element.
