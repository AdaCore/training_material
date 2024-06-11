..
    Coding language

.. role:: ada(code)
    :language: Ada

.. role:: C(code)
    :language: C

.. role:: cpp(code)
    :language: C++

..
    Math symbols

.. |rightarrow| replace:: :math:`\rightarrow`
.. |forall| replace:: :math:`\forall`
.. |exists| replace:: :math:`\exists`
.. |equivalent| replace:: :math:`\iff`
.. |le| replace:: :math:`\le`
.. |ge| replace:: :math:`\ge`
.. |lt| replace:: :math:`<`
.. |gt| replace:: :math:`>`

..
    Miscellaneous symbols

.. |checkmark| replace:: :math:`\checkmark`

======================
Expression Functions
======================

----------------------
Expression Functions
----------------------

* Functions whose implementations are pure expressions

   - No other completion is allowed
   - No :ada:`return` keyword

* May exist only for sake of pre/postconditions

.. code:: Ada

   function function_specification is (expression);

NB: Parentheses around expression are **required**

* Can complete a prior declaration

.. code:: Ada

   function Squared (X : Integer) return Integer;
   function Squared (X : Integer) return Integer is
      (X ** 2);

..
  language_version 2012

------------------------------
Expression Functions Example
------------------------------

* Expression function

.. code:: Ada

   function Square (X : Integer) return Integer is (X ** 2);

* Is equivalent to

.. code:: Ada

   function Square (X : Integer) return Integer is
   begin
      return X ** 2;
   end Square;

..
  language_version 2012

------
Quiz
------

Which statement is True?

   A. Expression functions cannot be nested functions.
   B. Expression functions require a specification and a body.
   C. Expression functions must have at least one "return" statement.
   D. :answer:`Expression functions can have "out" parameters.`

.. container:: animate

   Explanations

   A. False, they can be declared just like regular function
   B. False, an expression function cannot have a body
   C. False, expression functions cannot contain a no :ada:`return`
   D. Correct, but it can assign to :ada:`out` parameters only by calling another function.
