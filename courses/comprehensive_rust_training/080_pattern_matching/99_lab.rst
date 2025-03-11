=================================
Exercise: Expression Evaluation
=================================

----------------------------------
Expression Evaluation Background
----------------------------------

Let's write a simple recursive evaluator for arithmetic expressions.

An example of a small arithmetic expression could be :rust:`10 + 20`, which
evaluates to :rust:`30`. We can represent the expression as a tree:

.. image:: comprehensive_rust_training/pattern_matching_exercise_1.svg
   :width: 40%

A bigger and more complex expression would be
:rust:`(10 * 9) + ((3 - 4) * 5)`, which evaluate to :rust:`85`. We represent
this as a much bigger tree:

.. image:: comprehensive_rust_training/pattern_matching_exercise_2.svg

---------------------------------
Expression Evaluation Problem
---------------------------------

In code, we will represent the tree with two types:

.. code:: rust

   /// An operation to perform on two subexpressions.
   #[derive(Debug)]
   enum Operation {
       Add,
       Sub,
       Mul,
       Div,
   }

   /// An expression, in tree form.
   #[derive(Debug)]
   enum Expression {
       /// An operation on two subexpressions.
       Op { op: Operation, left: Box<Expression>, right: Box<Expression> },

       /// A literal value
       Value(i64),
   }

And then create an evaluator:

.. code:: rust

   fn eval(e: Expression) -> i64 {
       todo!()
   }

.. note:: The :rust:`Box` type here is a smart pointer, and will be covered in detail later in the course. To evaluate a boxed expression, use the deref operator (:rust:`*`) to "unbox" it: :rust:`eval(*boxed_expr)`.

Copy and paste the code into the Rust playground, and begin implementing
:rust:`eval`.

---------------------------------
Expression Evaluation Problem
---------------------------------

.. code:: rust

   fn eval(e: Expression) -> i64 {
       match e {
           Expression::Op { op, left, right } => {
               let left = eval(*left);
               let right = eval(*right);
               match op {
                   Operation::Add => left + right,
                   Operation::Sub => left - right,
                   Operation::Mul => left * right,
                   Operation::Div => left / right,
               }
           }
           Expression::Value(v) => v,
       }
   }
