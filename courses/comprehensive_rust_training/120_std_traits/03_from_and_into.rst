=======================
``From`` and ``Into``
=======================

-----------------------
``From`` and ``Into``
-----------------------

Types implement
`From <https://doc.rust-lang.org/std/convert/trait.From.html>`__ and
`Into <https://doc.rust-lang.org/std/convert/trait.Into.html>`__ to
facilitate type conversions. Unlike ``as``, these traits correspond to
lossless, infallible conversions.

.. code:: rust,editable

   fn main() {
       let s = String::from("hello");
       let addr = std::net::Ipv4Addr::from([127, 0, 0, 1]);
       let one = i16::from(true);
       let bigger = i32::from(123_i16);
       println!("{s}, {addr}, {one}, {bigger}");
   }

`Into <https://doc.rust-lang.org/std/convert/trait.Into.html>`__ is
automatically implemented when
`From <https://doc.rust-lang.org/std/convert/trait.From.html>`__ is
implemented:

.. code:: rust,editable

   fn main() {
       let s: String = "hello".into();
       let addr: std::net::Ipv4Addr = [127, 0, 0, 1].into();
       let one: i16 = true.into();
       let bigger: i32 = 123_i16.into();
       println!("{s}, {addr}, {one}, {bigger}");
   }

.. raw:: html

---------
Details
---------

-  That's why it is common to only implement ``From``, as your type will
   get ``Into`` implementation too.
-  When declaring a function argument input type like "anything that can
   be converted into a ``String``", the rule is opposite, you should use
   ``Into``. Your function will accept types that implement ``From`` and
   those that *only* implement ``Into``.

.. raw:: html

