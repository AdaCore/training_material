=================================
Exercise: Expression Evaluation
=================================

---------------------------------
Exercise: Expression Evaluation
---------------------------------

Let's write a simple recursive evaluator for arithmetic expressions.

An example of a small arithmetic expression could be :rust:`10 + 20`, which
evaluates to :rust:`30`. We can represent the expression as a tree:

.. code:: bob

               .-------.
       .------ |   +   | ------.
       |       '-------'       |
       v                       v
   .--------.              .--------.
   |   10   |              |   20   |
   '--------'              '--------'

A bigger and more complex expression would be
:rust:`(10 * 9) + ((3 - 4) * 5)`, which evaluate to :rust:`85`. We represent
this as a much bigger tree:

.. code:: bob

                                 .-----.
               .---------------- |  +  | ----------------.
               |                 '-----'                 |
               v                                         v
            .-----.                                   .-----.
      .---- |  *  | ----.                       .---- |  *  | ----.
      |     '-----'     |                       |     '-----'     |
      v                 v                       v                 v
   .------.          .-----.                 .-----.           .-----.
   |  10  |          |  9  |           .---- |  "-"| ----.     |  5  |
   '------'          '-----'           |     '-----'     |     '-----'
                                       v                 v
                                    .-----.           .-----.
                                    |  3  |           |  4  |
                                    '-----'           '-----'

In code, we will represent the tree with two types:

.. code:: rust

   {{#include exercise.rs:Operation}}

   {{#include exercise.rs:Expression}}

The :rust:`Box` type here is a smart pointer, and will be covered in detail
later in the course. An expression can be "boxed" with :rust:`Box::new` as
seen in the tests. To evaluate a boxed expression, use the deref
operator (:rust:`*`) to "unbox" it: :rust:`eval(*boxed_expr)`.

Copy and paste the code into the Rust playground, and begin implementing
:rust:`eval`. The final product should pass the tests. It may be helpful to
use :rust:`todo!()` and get the tests to pass one-by-one. You can also skip
a test temporarily with :rust:`#[ignore]`:

.. code:: none

   #[test]
   #[ignore]
   fn test_value() { .. }

.. code:: rust

   {{#include exercise.rs:Operation}}

   {{#include exercise.rs:Expression}}

   {{#include exercise.rs:eval}}
       todo!()
   }

   {{#include exercise.rs:tests}}
