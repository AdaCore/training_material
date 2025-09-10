========
Syntax
========

-------------------------
 Specification and Body
-------------------------

* Subprogram specification is the external (user) **interface**

   - **Declaration** and **specification** are used synonymously

* Specification may be required in some cases

   - eg. recursion

* Subprogram body is the **implementation**

-------------------------------------------
Procedure Specification Syntax (Simplified)
-------------------------------------------

**Syntax**

.. container:: source_include 070_subprograms/syntax.bnf :start-after:procedure_specification_syntax_begin :end-before:procedure_specification_syntax_end :code:bnf

.. container:: source_include 070_subprograms/syntax.bnf :start-after:subprogram_specification_syntax_begin :end-before:subprogram_specification_syntax_end :code:bnf

**Examples**

.. code:: Ada

   procedure Swap (A, B : in out Integer);
   procedure Clean (Force : Boolean := True);

------------------------------------------
Function Specification Syntax (Simplified)
------------------------------------------

**Syntax**

.. container:: source_include 070_subprograms/syntax.bnf :start-after:function_specification_syntax_begin :end-before:function_specification_syntax_end :code:bnf

.. container:: source_include 070_subprograms/syntax.bnf :start-after:subprogram_specification_syntax_begin :end-before:subprogram_specification_syntax_end :code:bnf

**Example**

.. code:: Ada

   function Square (X : Float) return Float;
   function Is_Open return Boolean;

-------------
Body Syntax
-------------

.. code:: Ada

   subprogram_specification is
      [declarations]
   begin
      sequence_of_statements
   end [designator];

.. code:: Ada

   procedure Hello is
   begin
      Ada.Text_IO.Put_Line ("Hello World!");
      Ada.Text_IO.New_Line (2);
   end Hello;

   function F (X : Float) return Float is
      Y : constant Float := X + 3.0;
   begin
      return X * Y;
   end F;

--------------
Completions
--------------

* Bodies **complete** the specification

   - There are **other** ways to complete

* Separate specification is **not required**

   - Body can act as a specification

* A declaration and its body must **fully** conform

   - Mostly **semantic** check
   - But parameters **must** have same name

.. code:: Ada

   procedure P (J, K : Integer)
   procedure P (J : Integer; K : Integer)
   procedure P (J, K : in Integer)
   -- Invalid
   procedure P (A : Integer; B : Integer)

---------------------
Completion Examples
---------------------

* Specifications

   .. code:: Ada

      procedure Swap (A, B : in out Integer);
      function Min (X, Y : Person) return Person;

* Completions

   .. code:: Ada

      procedure Swap (A, B : in out Integer) is
        Temp : Integer := A;
      begin
        A := B;
        B := Temp;
      end Swap;

      -- Completion as specification
      function Less_Than (X, Y : Person) return Boolean is
      begin
         return X.Age < Y.Age;
      end Less_Than;

      function Min (X, Y : Person) return Person is
      begin
         if Less_Than (X, Y) then
            return X;
         else
            return Y;
         end if;
      end Min;

------------------------------------------
Direct Recursion - No Declaration Needed
------------------------------------------

* When :ada:`is` is reached, the subprogram becomes **visible**

    - It can call **itself** without a declaration

.. code:: Ada

   type Vector_T is array (Natural range <>) of Integer;
   Empty_Vector : constant Vector_T (1 .. 0) := (others => 0);

   function Get_Vector return Vector_T is
     Next : Integer;
   begin
     Get (Next);

     if Next = 0 then
       return Empty_Vector;
     else
       return Get_Vector & Next;
     end if;
   end Get_Vector;

----------------------------
Indirect Recursion Example
----------------------------

* Elaboration in **linear order**

.. code:: Ada

   procedure P;

   procedure F is
   begin
     P;
   end F;

   procedure P is
   begin
     F;
   end P;

------
Quiz
------

Which profile is semantically different from the others?

   A. ``procedure P (A : Integer; B : Integer);``
   B. ``procedure P (A, B : Integer);``
   C. :answermono:`procedure P (B : Integer; A : Integer);`
   D. ``procedure P (A : in Integer; B : in Integer);``

.. container:: animate

   Parameter names are important in Ada.  The other selections have
   the names in the same order with the same mode and type.

