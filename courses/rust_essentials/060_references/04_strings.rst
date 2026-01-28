=========
Strings
=========

-------------------------
Two main "string" types
-------------------------

- :rust:`&str`: String slice, immutable reference to UTF-8 encoded bytes
  - Similar to :rust:`&[u8]`
- String literals (:rust:`"Hello"`) are :rust:`&str`
  - Stored in the program binary
- :rust:`String`: Buffer of UTF-8 encoded bytes
  - Created from a string literal with :rust:`String::from()`
  - Similar to C++ std::string 

.. code:: rust

   let s1: &str = "World";
   let mut s2 = String::from("Hello ");
   s2.push_str(s1);
   println!("s2: {s2}");
   let s3: &str = &s2[..5];
   println!("s3: {s3}");

* Generates the following output:

:command:`s2: Hello World`

:command:`s3: Hello`
