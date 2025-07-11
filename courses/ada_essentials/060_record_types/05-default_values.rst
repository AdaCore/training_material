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

-----------------------------------
Defaults Within Record Aggregates
-----------------------------------

* Specified via the :dfn:`box` notation
* Value for the component is thus taken as for a stand-alone object declaration

   - So there may or may not be a defined default!

* Can only be used with "named association" form

   - But can mix forms, unlike array aggregates

.. code:: Ada

   type Complex is
     record
       Real : Float := 0.0;
       Imaginary : Float := 0.0;
     end record;
   Phase := (42.0, Imaginary => <>);

..
  language_version 2005

------------------------------------------
Default Initialization Via Aspect Clause
------------------------------------------

* Not definable for entire record type
* Components of scalar types take type's default if no explicit default value specified by record type

.. code:: Ada

   type Toggle_Switch is (Off, On)
       with Default_Value => Off;
   type Controller is record
       -- Off unless specified during object initialization
       Override : Toggle_Switch;
       -- default for this component
       Enable : Toggle_Switch := On;
     end record;
   C : Controller; -- Override => off, Enable => On
   D : Controller := (On, Off); -- All defaults replaced

..
  language_version 2012

------
Quiz
------

.. code:: Ada

   function Next return Natural; -- returns next number starting with 1

   type Record_T is record
      A, B : Integer := Next;
      C    : Integer := Next;
   end record;
   R : Record_T := (C => 100, others => <>);

What is the value of R?

A. (1, 2, 3)
B. (1, 1, 100)
C. :answer:`(1, 2, 100)`
D. (100, 101, 102)

.. container:: animate

 Explanations

 A. :ada:`C => 100`
 B. Multiple declaration calls :ada:`Next` twice
 C. Correct
 D. :ada:`C => 100` has no effect on :ada:`A` and :ada:`B`

..
  language_version 2012

