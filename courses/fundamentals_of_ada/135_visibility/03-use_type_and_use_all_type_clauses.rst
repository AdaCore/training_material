=======================================
"use type" and "use all type" Clauses
=======================================

-------------------------------
"use type" and "use all type"
-------------------------------

* :ada:`use type` makes **primitive operators** directly visible for specified type

   - Implicit and explicit operator function declarations

   .. code:: Ada

      use type subtype_mark {, subtype_mark};

* :ada:`use all type` makes primitive operators **and all other operations** directly visible for specified type

   - All **enumerated type values** will also be directly visible

   .. code:: Ada

      use all type subtype_mark {, subtype_mark};

* More specific alternatives to :ada:`use` clauses

   - Especially useful when multiple :ada:`use` clauses introduce ambiguity

--------------
Example Code
--------------

.. code:: Ada

   package Types is
     type Distance_T is range 0 .. Integer'Last;

     -- explicit declaration
     -- (we don't want a negative distance)
     function "-" (Left, Right : Distance_T)
                   return Distance_T;

     -- implicit declarations (we get the division operator
     -- for "free", showing it for completeness)
     -- function "/" (Left, Right : Distance_T) return
     --               Distance_T;

     -- primitive operation
     function Min (A, B : Distance_T)
                   return Distance_T;

   end Types;

--------------------------
"use" Clauses Comparison
--------------------------

.. image:: use_clause_comparison.png

-----------------------------
Multiple "use type" Clauses
-----------------------------

* May be necessary
* Only those that mention the type in their profile are made visible

.. code:: Ada

   package P is
     type T1 is range 1 .. 10;
     type T2 is range 1 .. 10;
     -- implicit
     -- function "+"(Left : T2; Right : T2) return T2;
     type T3 is range 1 .. 10;
     -- explicit
     function "+"(Left : T1; Right : T2) return T3;
   end P;

   with P;
   procedure UseType is
     X1 : P.T1;
     X2 : P.T2;
     X3 : P.T3;
     use type P.T1;
   begin
     X3 := X1 + X2; -- operator visible because it uses T1
     X2 := X2 + X2; -- operator not visible
   end UseType;

