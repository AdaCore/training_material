====================
Reference Validity
====================

--------------------
Reference Validity
--------------------

Rust enforces a number of rules for references that make them always
safe to use. One rule is that references can never be :rust:`null`, making
them safe to use without :rust:`null` checks. The other rule we'll look at
for now is that references can't *outlive* the data they point to.

.. code:: rust

   fn main() {
       let x_ref = {
           let x = 10;
           &x
       };
       println!("x: {x_ref}");
   }

---------
Details
---------

-  This slide gets students thinking about references as not simply
   being pointers, since Rust has different rules for references than
   other languages.

-  We'll look at the rest of Rust's borrowing rules on day 3 when we
   talk about Rust's ownership system.

-----------------
More to Explore
-----------------

-  Rust's equivalent of nullability is the :rust:`Option` type, which can be
   used to make any type "nullable" (not just references/pointers). We
   haven't yet introduced enums or pattern matching, though, so try not
   to go into too much detail about this here.
