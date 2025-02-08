=======================
"match" Expressions
=======================

-----------------------
"match" Expressions
-----------------------

:rust:`match` can be used to check a value against one or more options:

.. code:: rust

   fn main() {
       let val = 1;
       match val {
           1 => println!("one"),
           10 => println!("ten"),
           100 => println!("one hundred"),
           _ => {
               println!("something else");
           }
       }
   }

Like :rust:`if` expressions, :rust:`match` can also return a value;

.. code:: rust

   fn main() {
       let flag = true;
       let val = match flag {
           true => 1,
           false => 0,
       };
       println!("The value of {flag} is {val}");
   }

---------
Details
---------

-  :rust:`match` arms are evaluated from top to bottom, and the first one
   that matches has its corresponding body executed.

-  There is no fall-through between cases the way that :rust:`switch` works
   in other languages.

-  The body of a :rust:`match` arm can be a single expression or a block.
   Technically this is the same thing, since blocks are also
   expressions, but students may not fully understand that symmetry at
   this point.

-  :rust:`match` expressions need to be exhaustive, meaning they either need
   to cover all possible values or they need to have a default case such
   as :rust:`_`. Exhaustiveness is easiest to demonstrate with enums, but
   enums haven't been introduced yet. Instead we demonstrate matching on
   a :rust:`bool`, which is the simplest primitive type.

-  This slide introduces :rust:`match` without talking about pattern
   matching, giving students a chance to get familiar with the syntax
   without front-loading too much information. We'll be talking about
   pattern matching in more detail tomorrow, so try not to go into too
   much detail here.

-----------------
More to Explore
-----------------

-  To further motivate the usage of :rust:`match`, you can compare the
   examples to their equivalents written with :rust:`if`. In the second case
   matching on a :rust:`bool` an :rust:`if {} else {}` block is pretty similar.
   But in the first example that checks multiple cases, a :rust:`match`
   expression can be more concise than
   :rust:`if {} else if {} else if {} else`.

-  :rust:`match` also supports match guards, which allow you to add an
   arbitrary logical condition that will get evaluated to determine if
   the match arm should be taken. However talking about match guards
   requires explaining about pattern matching, which we're trying to
   avoid on this slide.
