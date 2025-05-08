================
Default Values
================

--------------------------
Component Default Values
--------------------------

.. code:: Ada

   type Complex is
     record
       Real : Float := 0.0;
       Imaginary : Float := 0.0;
     end record;
   -- all components use defaults
   Phasor : Complex;
   -- all components must be specified
   I : constant Complex := (0.0, 1.0);

------------------------------------
Default Component Value Evaluation
------------------------------------

* Occurs when object is elaborated

   - Not when the type is elaborated

* Not evaluated if explicitly overridden

.. code:: Ada

   type Structure is
     record
       A : Integer;
       R : Time := Clock;
     end record;
   -- Clock is called for S1
   S1 : Structure;
   -- Clock is not called for S2
   S2 : Structure := (A => 0, R => Yesterday);

------
Quiz
------

.. code:: Ada

   function Next return Natural;
   -- returns next number starting with 1

   type Record_T is record
      A, B : Integer;
      C    : Integer;
   end record;
   R : Record_T;

What is the value of R?

.. container:: animate

  Trick question!

  Order of evaluation is compiler/runtime dependent, so it could be 
  any combination of 1, 2, and 3
