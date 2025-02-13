==================
Let Control Flow
==================

------------------
Let Control Flow
------------------

Rust has a few control flow constructs which differ from other
languages. They are used for pattern matching:

-  :rust:`if let` expressions
-  :rust:`let else` expressions
-  :rust:`while let` expressions

========================
"if let" expressions
========================

------------------------
"if let" expressions
------------------------

The
:url:`if let expression <https://doc.rust-lang.org/reference/expressions/if-expr.html#if-let-expressions>`
lets you execute different code depending on whether a value matches a
pattern:

.. code:: rust

   use std::time::Duration;

   fn sleep_for(secs: f32) {
       if let Ok(duration) = Duration::try_from_secs_f32(secs) {
           std::thread::sleep(duration);
           println!("slept for {duration:?}");
       }
   }

   fn main() {
       sleep_for(-10.0);
       sleep_for(0.8);
   }

==========================
"let else" expressions
==========================

--------------------------
"let else" expressions
--------------------------

For the common case of matching a pattern and returning from the
function, use
:url:`let else <https://doc.rust-lang.org/rust-by-example/flow_control/let_else.html>`.
The "else" case must diverge (:rust:`return`, :rust:`break`, or panic - anything
but falling off the end of the block).

.. code:: rust

   fn hex_or_die_trying(maybe_string: Option<String>) -> Result<u32, String> {
       // TODO: The structure of this code is difficult to follow -- rewrite it with let-else!
       if let Some(s) = maybe_string {
           if let Some(first_byte_char) = s.chars().next() {
               if let Some(digit) = first_byte_char.to_digit(16) {
                   Ok(digit)
               } else {
                   return Err(String::from("not a hex digit"));
               }
           } else {
               return Err(String::from("got empty string"));
           }
       } else {
           return Err(String::from("got None"));
       }
   }

   fn main() {
       println!("result: {:?}", hex_or_die_trying(Some(String::from("foo"))));
   }

Like with :rust:`if let`, there is a
:url:`while let <https://doc.rust-lang.org/reference/expressions/loop-expr.html#predicate-pattern-loops>`
variant which repeatedly tests a value against a pattern:

.. raw:: html

   <!-- mdbook-xgettext: skip -->

.. code:: rust

   fn main() {
       let mut name = String::from("Comprehensive Rust");
       while let Some(c) = name.pop() {
           println!("character: {c}");
       }
       // (There are more efficient ways to reverse a string!)
   }

Here
:url:`String::pop <https://doc.rust-lang.org/stable/std/string/struct.String.html#method.pop>`
returns :rust:`Some(c)` until the string is empty, after which it will
return :rust:`None`. The :rust:`while let` lets us keep iterating through all
items.

.. raw:: html

---------
Details
---------

--------
if-let
--------

-  Unlike :rust:`match`, :rust:`if let` does not have to cover all branches.
   This can make it more concise than :rust:`match`.
-  A common usage is handling :rust:`Some` values when working with
   :rust:`Option`.
-  Unlike :rust:`match`, :rust:`if let` does not support guard clauses for
   pattern matching.

----------
let-else
----------

:rust:`if-let`\ s can pile up, as shown. The :rust:`let-else` construct supports
flattening this nested code. Rewrite the awkward version for students,
so they can see the transformation.

The rewritten version is:

.. code:: rust

   fn hex_or_die_trying(maybe_string: Option<String>) -> Result<u32, String> {
       let Some(s) = maybe_string else {
           return Err(String::from("got None"));
       };

       let Some(first_byte_char) = s.chars().next() else {
           return Err(String::from("got empty string"));
       };

       let Some(digit) = first_byte_char.to_digit(16) else {
           return Err(String::from("not a hex digit"));
       };

       return Ok(digit);
   }

===========
while-let
===========

-----------
while-let
-----------

-  Point out that the :rust:`while let` loop will keep going as long as the
   value matches the pattern.
-  You could rewrite the :rust:`while let` loop as an infinite loop with an
   if statement that breaks when there is no value to unwrap for
   :rust:`name.pop()`. The :rust:`while let` provides syntactic sugar for the
   above scenario.

.. raw:: html

