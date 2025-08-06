===========================
Loops and Predicted Values
===========================

---------------------------
Recursive Data Structures
---------------------------

* Pointers allow to build recursive data structures like lists

  .. code:: ada

     type List_Cell;
     type List_Acc is access List_Cell;
     type List_Cell is record
        Value : Integer;
        Next  : List_Acc;
     end record;

* Traversing the data structure can use

  - Recursion, typically for specification functions
  - Loops otherwise

------------------------
Pointers and Recursion
------------------------

* No built-in quantified expression for recursive data structures

* Instead, use recursion to traverse the structure

  .. code:: ada

     function All_List_Zero
       (L : access constant List_Cell) return Boolean
     is (L = null or else
          (L.Value = 0 and then All_List_Zero (L.Next)));

* Reminder: :toolname:`GNATprove` protects against non-terminating recursive
  functions

  - No axioms generated for such functions
  - Need to prove termination of recursive functions

* Use special form of structural subprogram variant

  .. code:: ada

     function All_List_Zero ... with
       Subprogram_Variant => (Structural => L);

--------------------
Pointers and Loops
--------------------

* Procedure :ada:`Init_List_Zero` initializes :ada:`L`

  .. code:: ada

     procedure Init_List_Zero (L : access List_Cell)
       with Post => All_List_Zero (L);

* Initialization uses loop to traverse data structure

  .. code:: ada

     procedure Init_List_Zero (L : access List_Cell) is
        B : access List_Cell := L;
     begin
        while B /= null loop
           B.Value := 0;
           B := B.Next;
        end loop;
     end Init_List_Zero;

* Problem: how do we express that previous cells have value zero?

  - Cannot refer to value of :ada:`L` while borrowed

------------------
Predicted Values
------------------

* Special annotation :ada:`At_End_Borrow` on identity function

  - For proof, refers to value of argument at the end of the borrow
  - For execution, is simply the identity function

  .. code:: ada

     function At_End
       (L : access constant List_Cell)
       return access constant List_Cell
     is (L)
     with
       Ghost,
       Annotate => (GNATprove, At_End_Borrow);

* Loop invariant can refer to values at end of the borrow

  - Value of borrower at end of the borrow :ada:`At_End (B)`
  - Value of borrowed at end of the borrow :ada:`At_End (L)`

  .. code:: ada

     pragma Loop_Invariant
       (if All_List_Zero (At_End (B))
        then All_List_Zero (At_End (L)));

* Invariant proved using what is known now about the value at end

  - There is no look ahead
  - Loop invariant proved because values in L and not B are frozen to 0

