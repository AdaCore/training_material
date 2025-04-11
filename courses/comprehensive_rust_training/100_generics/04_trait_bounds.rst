==============
Trait Bounds
==============

--------------
Trait Bounds
--------------

When working with generics, you often want to require the types to
implement some trait, so that you can call this trait's methods.

You can do this with :rust:`T: Trait`:

.. code:: rust

   fn duplicate<T: Clone>(a: T) -> (T, T) {
       (a.clone(), a.clone())
   }

   struct NotCloneable;

   fn main() {
       let foo = String::from("foo");
       let pair = duplicate(foo);
       println!("{pair:?}");
   }

------------
Key Points
------------

-  When multiple traits are necessary, use :rust:`+` to join them.

-  As Rust does not (yet) support specialization, given the original :rust:`duplicate`,
   it is invalid to add a specialized :rust:`duplicate(a: u32)`.

.. container:: speakernote

  -  Show a :rust:`where` clause, students will encounter it when reading
     code.

    .. code:: rust

      fn duplicate<T>(a: T) -> (T, T)
      where
          T: Clone,
      {
          (a.clone(), a.clone())
      }

    - It declutters the function signature if you have many parameters.
    - It has additional features making it more powerful.

      -  If someone asks, the extra feature is that the type on the left
         of :rust:`:` can be arbitrary, like :rust:`Option<T>`.

