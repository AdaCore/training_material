=========
Strings
=========

---------
Strings
---------

We can now understand the two string types in Rust:

-  :rust:`&str` is a slice of UTF-8 encoded bytes, similar to :rust:`&[u8]`.
-  :rust:`String` is an owned buffer of UTF-8 encoded bytes, similar to
   :rust:`Vec<T>`.

.. code:: rust

   fn main() {
       let s1: &str = "World";
       println!("s1: {s1}");

       let mut s2: String = String::from("Hello ");
       println!("s2: {s2}");
       s2.push_str(s1);
       println!("s2: {s2}");

       let s3: &str = &s2[s2.len() - s1.len()..];
       println!("s3: {s3}");
   }

---------
Details
---------

-  :rust:`&str` introduces a string slice, which is an immutable reference
   to UTF-8 encoded string data stored in a block of memory. String
   literals (:rust:`"Hello"`), are stored in the program's binary.

-  Rust's :rust:`String` type is a wrapper around a vector of bytes. As with
   a :rust:`Vec<T>`, it is owned.

-  As with many other types :rust:`String::from()` creates a string from a
   string literal; :rust:`String::new()` creates a new empty string, to
   which string data can be added using the :rust:`push()` and
   :rust:`push_str()` methods.

-  The :rust:`format!()` macro is a convenient way to generate an owned
   string from dynamic values. It accepts the same format specification
   as :rust:`println!()`.

-  You can borrow :rust:`&str` slices from :rust:`String` via :rust:`&` and
   optionally range selection. If you select a byte range that is not
   aligned to character boundaries, the expression will panic. The
   :rust:`chars` iterator iterates over characters and is preferred over
   trying to get character boundaries right.

-  For C++ programmers: think of :rust:`&str` as :rust:`std::string_view` from
   C++, but the one that always points to a valid string in memory. Rust
   :rust:`String` is a rough equivalent of :rust:`std::string` from C++ (main
   difference: it can only contain UTF-8 encoded bytes and will never
   use a small-string optimization).

-  Byte strings literals allow you to create a :rust:`&[u8]` value directly:

   .. code:: rust

      fn main() {
          println!("{:?}", b"abc");
          println!("{:?}", &[97, 98, 99]);
      }

-  Raw strings allow you to create a :rust:`&str` value with escapes
   disabled: :rust:`r"\n" == "\\n"`. You can embed double-quotes by using an
   equal amount of :rust:`#` on either side of the quotes:

   .. code:: rust

      fn main() {
          println!(r#"<a href="link.html">link</a>"#);
          println!("<a href=\"link.html\">link</a>");
      }
