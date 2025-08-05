========
String
========

--------
String
--------

:url:`String <https://doc.rust-lang.org/std/string/struct.String.html>`
is a growable UTF-8 encoded string:

.. code:: rust

   fn main() {
       let mut s1 = String::new();
       s1.push_str("Hello");
       println!("s1: len = {}, capacity = {}", s1.len(), s1.capacity());

       let mut s2 = String::with_capacity(s1.len() + 1);
       s2.push_str(&s1);
       s2.push('!');
       println!("s2: len = {}, capacity = {}", s2.len(), s2.capacity());

       let s3 = String::from("a z");
       println!("s3: len = {}, number of chars = {}", s3.len(), s3.chars().count());
   }

:rust:`String` implements
:url:`Deref<Target = str> <https://doc.rust-lang.org/std/string/struct.String.html#deref-methods-str>`,
which means that you can call all :rust:`str` methods on a :rust:`String`.

..
   https://doc.rust-lang.org/std/string/struct.String.html#deref-methods-str

which means that you can call all :rust:`str` methods on a :rust:`String`.

---------
Details
---------

-  :rust:`String::new` returns a new empty string, use
   :rust:`String::with_capacity` when you know how much data you want to
   push to the string.
-  :rust:`String::len` returns the size of the :rust:`String` in bytes (which
   can be different from its length in characters).
-  :rust:`String::chars` returns an iterator over the actual characters.
   Note that a :rust:`char` can be different from what a human will consider
   a "character" due to
   :url:`grapheme clusters <https://docs.rs/unicode-segmentation/latest/unicode_segmentation/struct.Graphemes.html>`.
-  When people refer to strings they could either be talking about
   :rust:`&str` or :rust:`String`.
-  When a type implements :rust:`Deref<Target = T>`, the compiler will let
   you transparently call methods from :rust:`T`.

   -  We haven't discussed the :rust:`Deref` trait yet, so at this point
      this mostly explains the structure of the sidebar in the
      documentation.
   -  :rust:`String` implements :rust:`Deref<Target = str>` which transparently
      gives it access to :rust:`str` methods.
   -  Write and compare :rust:`let s3 = s1.deref();` and :rust:`let s3 = &*s1;`.

-  :rust:`String` is implemented as a wrapper around a vector of bytes, many
   of the operations you see supported on vectors are also supported on
   :rust:`String`, but with some extra guarantees.
-  Compare the different ways to index a :rust:`String`:

   -  To a character by using :rust:`s3.chars().nth(i).unwrap()` where :rust:`i`
      is in-bound, out-of-bounds.
   -  To a substring by using :rust:`s3[0..4]`, where that slice is on
      character boundaries or not.

-  Many types can be converted to a string with the
   :url:`to_string <https://doc.rust-lang.org/std/string/trait.ToString.html#tymethod.to_string>`
   method. This trait is automatically implemented for all types that
   implement :rust:`Display`, so anything that can be formatted can also be
   converted to a string.
