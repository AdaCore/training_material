============
"Box<T>"
============

------------
"Box<T>"
------------

:url:`Box <https://doc.rust-lang.org/std/boxed/struct.Box.html>` is an
owned pointer to data on the heap:

.. code:: rust

   fn main() {
       let five = Box::new(5);
       println!("five: {}", *five);
   }

.. code:: bob

    Stack                     Heap
   .- - - - - - -.     .- - - - - - -.
   :             :     :             :
   :    five     :     :             :
   :   +-----+   :     :   +-----+   :
   :   | o---|---+-----+-->|  5  |   :
   :   +-----+   :     :   +-----+   :
   :             :     :             :
   :             :     :             :
   `- - - - - - -'     `- - - - - - -'

:rust:`Box<T>` implements :rust:`Deref<Target = T>`, which means that you can
:url:`call methods from T directly on a Box<T> <https://doc.rust-lang.org/std/ops/trait.Deref.html#more-on-deref-coercion>`.

Recursive data types or data types with dynamic sizes cannot be stored
inline without a pointer indirection. :rust:`Box` accomplishes that
indirection:

.. code:: rust

   #[derive(Debug)]
   enum List<T> {
       /// A non-empty list: first element and the rest of the list.
       Element(T, Box<List<T>>),
       /// An empty list.
       Nil,
   }

   fn main() {
       let list: List<i32> =
           List::Element(1, Box::new(List::Element(2, Box::new(List::Nil))));
       println!("{list:?}");
   }

.. code:: bob

    Stack                           Heap
   .- - - - - - - - - - - - - - .     .- - - - - - - - - - - - - - - - - - - - - - - - -.
   :                            :     :                                                 :
   :    list                    :     :                                                 :
   :   +---------+----+----+    :     :    +---------+----+----+    +------+----+----+  :
   :   | Element | 1  | o--+----+-----+--->| Element | 2  | o--+--->| Nil  | // | // |  :
   :   +---------+----+----+    :     :    +---------+----+----+    +------+----+----+  :
   :                            :     :                                                 :
   :                            :     :                                                 :
   '- - - - - - - - - - - - - - '     '- - - - - - - - - - - - - - - - - - - - - - - - -'

.. raw:: html

---------
Details
---------

-  :rust:`Box` is like :rust:`std::unique_ptr` in C++, except that it's
   guaranteed to be not null.

-  A :rust:`Box` can be useful when you:

   -  have a type whose size can't be known at compile time, but the
      Rust compiler wants to know an exact size.
   -  want to transfer ownership of a large amount of data. To avoid
      copying large amounts of data on the stack, instead store the data
      on the heap in a :rust:`Box` so only the pointer is moved.

-  If :rust:`Box` was not used and we attempted to embed a :rust:`List` directly
   into the :rust:`List`, the compiler would not be able to compute a fixed
   size for the struct in memory (the :rust:`List` would be of infinite
   size).

-  :rust:`Box` solves this problem as it has the same size as a regular
   pointer and just points at the next element of the :rust:`List` in the
   heap.

-  Remove the :rust:`Box` in the List definition and show the compiler
   error. We get the message "recursive without indirection", because
   for data recursion, we have to use indirection, a :rust:`Box` or
   reference of some kind, instead of storing the value directly.

-  Though :rust:`Box` looks like :rust:`std::unique_ptr` in C++, it cannot be
   empty/null. This makes :rust:`Box` one of the types that allow the
   compiler to optimize storage of some enums (the "niche
   optimization").

.. raw:: html

