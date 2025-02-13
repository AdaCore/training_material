====================
The :rust:`Drop` Trait
====================

--------------------
The :rust:`Drop` Trait
--------------------

Values which implement
:url:`Drop <https://doc.rust-lang.org/std/ops/trait.Drop.html>` can
specify code to run when they go out of scope:

.. code:: rust

   struct Droppable {
       name: &'static str,
   }

   impl Drop for Droppable {
       fn drop(&mut self) {
           println!("Dropping {}", self.name);
       }
   }

   fn main() {
       let a = Droppable { name: "a" };
       {
           let b = Droppable { name: "b" };
           {
               let c = Droppable { name: "c" };
               let d = Droppable { name: "d" };
               println!("Exiting block B");
           }
           println!("Exiting block A");
       }
       drop(a);
       println!("Exiting main");
   }

.. raw:: html

---------
Details
---------

-  Note that :rust:`std::mem::drop` is not the same as
   :rust:`std::ops::Drop::drop`.
-  Values are automatically dropped when they go out of scope.
-  When a value is dropped, if it implements :rust:`std::ops::Drop` then its
   :rust:`Drop::drop` implementation will be called.
-  All its fields will then be dropped too, whether or not it implements
   :rust:`Drop`.
-  :rust:`std::mem::drop` is just an empty function that takes any value.
   The significance is that it takes ownership of the value, so at the
   end of its scope it gets dropped. This makes it a convenient way to
   explicitly drop values earlier than they would otherwise go out of
   scope.

   -  This can be useful for objects that do some work on :rust:`drop`:
      releasing locks, closing files, etc.

Discussion points:

-  Why doesn't :rust:`Drop::drop` take :rust:`self`?

   -  Short-answer: If it did, :rust:`std::mem::drop` would be called at the
      end of the block, resulting in another call to :rust:`Drop::drop`, and
      a stack overflow!

-  Try replacing :rust:`drop(a)` with :rust:`a.drop()`.

.. raw:: html

