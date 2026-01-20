===============================
:rust:`From` and :rust:`Into`
===============================

-------------------------------
:rust:`From` and :rust:`Into`
-------------------------------

Types implement
:url:`From <https://doc.rust-lang.org/std/convert/trait.From.html>` and
:url:`Into <https://doc.rust-lang.org/std/convert/trait.Into.html>` to
facilitate type conversions. Unlike :rust:`as`, these traits correspond to
lossless, infallible conversions.

.. code:: rust

   fn main() {
       let s = String::from("hello");
       let addr = std::net::Ipv4Addr::from([127, 0, 0, 1]);
       let one = i16::from(true);
       let bigger = i32::from(123_i16);
       println!("{s}, {addr}, {one}, {bigger}");
   }

:url:`Into <https://doc.rust-lang.org/std/convert/trait.Into.html>` is
automatically implemented when
:url:`From <https://doc.rust-lang.org/std/convert/trait.From.html>` is
implemented:

.. code:: rust

   fn main() {
       let s: String = "hello".into();
       let addr: std::net::Ipv4Addr = [127, 0, 0, 1].into();
       let one: i16 = true.into();
       let bigger: i32 = 123_i16.into();
       println!("{s}, {addr}, {one}, {bigger}");
   }

---------
Details
---------

-  That's why it is common to only implement :rust:`From`, as your type will
   get :rust:`Into` implementation too.
-  When declaring a function argument input type like "anything that can
   be converted into a :rust:`String`", the rule is opposite, you should use
   :rust:`Into`. Your function will accept types that implement :rust:`From` and
   those that *only* implement :rust:`Into`.
