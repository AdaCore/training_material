========
Option
========

--------
Option
--------

We have already seen some use of :rust:`Option<T>`. It stores either a value
of type :rust:`T` or nothing. For example,
`String::find <https://doc.rust-lang.org/stable/std/string/struct.String.html#method.find>`__
returns an :rust:`Option<usize>`.

.. code:: rust

   fn main() {
       let name = "Alexander the Great";
       let mut position: Option<usize> = name.find('e');
       println!("find returned {position:?}");
       assert_eq!(position.unwrap(), 14);
       position = name.find('Z');
       println!("find returned {position:?}");
       assert_eq!(position.expect("Character not found"), 0);
   }

---------
Details
---------

-  :rust:`Option` is widely used, not just in the standard library.
-  :rust:`unwrap` will return the value in an :rust:`Option`, or panic.
   :rust:`expect` is similar but takes an error message.

   -  You can panic on None, but you can't "accidentally" forget to
      check for None.
   -  It's common to :rust:`unwrap`/:rust:`expect` all over the place when
      hacking something together, but production code typically handles
      :rust:`None` in a nicer fashion.

-  The "niche optimization" means that :rust:`Option<T>` often has the same
   size in memory as :rust:`T`, if there is some representation that is not
   a valid value of T. For example, a reference cannot be NULL, so
   :rust:`Option<&T>` automatically uses NULL to represent the :rust:`None`
   variant, and thus can be stored in the same memory as :rust:`&T`.
