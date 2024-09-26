=================
Call Resolution
=================

-----------------
Call Resolution
-----------------

* Compilers must reject ambiguous calls
* :dfn:`Resolution` is based on the calling context

   - Compiler attempts to find a matching **profile**
   - Based on **Parameter** and **Result** Type

* Overloading is not re-definition, or hiding

   - More than one matching profile is ambiguous

.. code:: Ada

   type Complex is ...
   function "+" (L, R : Complex) return Complex;
   A, B : Complex := some_value;
   C : Complex := A + B;
   D : Float := A + B;  -- illegal!
   E : Float := 1.0 + 2.0;

-------------------------
Profile Components Used
-------------------------

* Significant components appear in the call itself

   - **Number** of parameters
   - **Order** of parameters
   - **Base type** of parameters
   - **Result** type (for functions)

* Insignificant components might not appear at call

   - Formal parameter **names** are optional
   - Formal parameter **modes** never appear
   - Formal parameter **subtypes** never appear
   - **Default** expressions never appear

   .. code:: Ada

      Display (X);
      Display (Foo => X);
      Display (Foo => X, Bar => Y);

-------------------------------
Manually Disambiguating Calls
-------------------------------

* Qualification can be used
* Named parameter association can be used

   - Unless name is ambiguous

.. code:: Ada

   type Stop_Light is (Red, Yellow, Green);
   type Colors is (Red, Blue, Green);
   procedure Put (Light : in Stop_Light);
   procedure Put (Shade : in Colors);

   Put (Red);  -- ambiguous call
   Put (Yellow);  -- not ambiguous: only 1 Yellow
   Put (Colors'(Red)); -- using type to distinguish
   Put (Light => Green); -- using profile to distinguish

---------------------
Overloading Example
---------------------

.. code:: Ada

   function "+" (Left : Position; Right : Offset)
     return Position is
   begin
      return Position'(Left.Row + Right.Row, Left.Column + Right.Col);
   end "+";

   function Acceptable (P : Position) return Boolean;
   type Positions is array (Moves range <>) of Position;

   function Next (Current : Position) return Positions is
     Result : Positions (Moves range 1 .. 4);
     Count  : Moves := 0;
     Test   : Position;
   begin
     for K in Offsets'Range loop
       Test := Current + Offsets (K);
       if Acceptable (Test) then
         Count := Count + 1;
         Result (Count) := Test;
       end if;
     end loop;
     return Result (1 .. Count);
   end Next;

.. container:: speakernote

   If Count is 0, result is a null range

------
Quiz
------

.. code:: Ada

   type Vertical_T is (Top, Middle, Bottom);
   type Horizontal_T is (Left, Middle, Right);
   function "*" (H : Horizontal_T; V : Vertical_T) return Positive;
   function "*" (V : Vertical_T; H : Horizontal_T) return Positive;
   P : Positive;

Which statement(s) is (are) legal?

   A. :answermono:`P := Horizontal_T'(Middle) * Middle;`
   B. :answermono:`P := Top * Right;`
   C. :answermono:`P := "*" (Middle, Top);`
   D. ``P := "*" (H => Middle, V => Top);``

.. container:: animate

   Explanations

   A. Qualifying one parameter resolves ambiguity
   B. No overloaded names
   C. Use of :ada:`Top` resolves ambiguity
   D. When overloading subprogram names, best to not just switch the order of parameters

