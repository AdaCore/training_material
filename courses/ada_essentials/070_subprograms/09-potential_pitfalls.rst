====================
Potential Pitfalls
====================

-----------------------------
Mode `out` Risk for Scalars
-----------------------------

* Always assign value to :ada:`out` parameters
* Else "By-copy" mechanism will copy something back

   - May be junk
   - :ada:`Constraint_Error` or unknown behaviour further down

.. code:: Ada

   procedure P
     (A, B : in Some_Type; Result : out Scalar_Type) is
   begin
     if Some_Condition then
       return;  -- Result not set
     end if;
     ...
     Result := Some_Value;
   end P;

----------------
"Side Effects"
----------------

* Any effect upon external objects or external environment

   - Typically alteration of non-local variables or states
   - Can cause hard-to-debug errors
   - Not legal for :ada:`function` in SPARK

* Can be there for historical reasons

   - Or some design patterns

.. code:: Ada

   Global : Integer := 0;

   function F (X : Integer) return Integer is
   begin
      Global := Global + X;
      return Global;
   end F;

---------------------------------------
Order-Dependent Code and Side Effects
---------------------------------------

.. |rightarrow| replace:: :math:`\rightarrow`

.. code:: Ada

   Global : Integer := 0;

   function Inc return Integer is
   begin
     Global := Global + 1;
     return Global;
   end Inc;

   procedure Assert_Equals (X, Y : in Integer);
   ...
   Assert_Equals (Global, Inc);

* Language does **not** specify parameters' order of evaluation
* :ada:`Assert_Equals` could get called with

   - X |rightarrow| 0, Y |rightarrow| 1 (if :ada:`Global` evaluated first)
   - X |rightarrow| 1, Y |rightarrow| 1 (if :ada:`Inc` evaluated first)

--------------------
Parameter Aliasing
--------------------

* :dfn:`Aliasing`: Multiple names for an actual parameter inside a subprogram body
* Possible causes:

   - Global object used is also passed as actual parameter
   - Same actual passed to more than one formal
   - Overlapping :ada:`array` slices
   - One actual is a component of another actual

* Can lead to code dependent on parameter-passing mechanism
* Ada detects some cases and raises :ada:`Program_Error`

.. code:: Ada

   procedure Update (Doubled, Tripled : in out Integer);
   ...
   Update (Doubled => A, Tripled => A);

.. container:: latex_environment footnotesize

   :command:`error: writable actual for "Doubled" overlaps with actual for "Tripled"`

----------------------------
Functions' Parameter Modes
----------------------------

* Can be mode :ada:`in out` and :ada:`out` too
* **Note:** operator functions can only have mode :ada:`in`

   - Including those you overload
   - Keeps readers sane

* Justification for only mode :ada:`in` in earlier versions of the language

   - No side effects: should be like mathematical functions
   - But side effects are still possible via globals
   - So worst possible case: side effects are possible and necessarily hidden!

..
  language_version 2012

----------------------------------
Easy Cases Detected and Not Legal
----------------------------------

.. code:: Ada

   procedure Example (A : in out Positive) is
      function Increment (This : Integer) return Integer is
      begin
         A := A + This;
         return A;
      end Increment;
      X : array (1 .. 10) of Integer;
   begin
      -- order of evaluating A not specified
      X (A) := Increment (A);
   end Example;

