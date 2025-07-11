====================
Function Specifics
====================

--------------------------------
Return Statements in Functions
--------------------------------

* Must have at least one

   - Compile-time error otherwise
   - Unless doing machine-code insertions

* Returns a value of the specified (sub)type
* Syntax

   .. code:: Ada

      function defining_designator [formal_part]
           return subtype_mark is
        declarative_part
        begin
           {statements}
           return expression;
        end designator;

---------------------------------------
No Path Analysis Required by Compiler
---------------------------------------

* Running to the end of a function without hitting a :ada:`return` statement raises :ada:`Program_Error`
* Compilers can issue warning if they suspect that a :ada:`return` statement will not be hit

.. code:: Ada

   function Greater (X, Y : Integer) return Boolean is
   begin
     if X > Y then
       return True;
     end if;
   end Greater; -- possible compile warning

----------------------------
Multiple Return Statements
----------------------------

* Allowed
* Sometimes the most clear

.. code:: Ada

   function Truncated (R : Float) return Integer is
     Converted : Integer := Integer (R);
   begin
     if R - Float (Converted) < 0.0 then -- rounded up
       return Converted - 1;
     else -- rounded down
       return Converted;
     end if;
   end Truncated;

---------------------------------------
Multiple Return Statements Versus One
---------------------------------------

* Many can detract from readability
* Can usually be avoided

.. code:: Ada

   function Truncated (R : Float) return Integer is
     Result : Integer := Integer (R);
   begin
     if R - Float (Result) < 0.0 then -- rounded up
       Result := Result - 1;
     end if;
     return Result;
   end Truncated;

-------------------------------
Function Dynamic-Size Results
-------------------------------

.. code:: Ada

    function Char_Mult (C : Character; L : Natural)
      return String is
       R : String (1 .. L) := (others => C);
    begin
       return R;
    end Char_Mult;

    X : String := Char_Mult ('x', 4);

.. code:: Ada

 begin
    -- OK
    pragma Assert (X'Length = 4 and X = "xxxx");

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

