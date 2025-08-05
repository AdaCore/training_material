===========================
:rust:`match` Expressions
===========================

---------------------------
:rust:`match` Expressions
---------------------------

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

-----------------------------------
More Information on :rust:`match`
-----------------------------------

- :rust:`match` arms are evaluated from top to bottom

  - First one that matches has its corresponding body executed.

- No fall-through between cases

  - Unlike :cpp:`switch` in C/C++

- :rust:`match` expressions need to be exhaustive

  - Either cover all possibilities
  - Or have a default case such as :rust:`_`

-----------------
More to Explore
-----------------

- To further motivate the usage of :rust:`match`, you can compare the
  examples to their equivalents written with :rust:`if`. In the second case
  matching on a :rust:`bool` an :rust:`if {} else {}` block is pretty similar.
  But in the first example that checks multiple cases, a :rust:`match`
  expression can be more concise than
  :rust:`if {} else if {} else if {} else`.

- :rust:`match` also supports match guards, which allow you to add an
  arbitrary logical condition that will get evaluated to determine if
  the match arm should be taken. However talking about match guards
  requires explaining about pattern matching, which we're trying to
  avoid on this slide.
