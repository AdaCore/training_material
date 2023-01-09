******************
Pointer Programs
******************

..
    Coding language

.. role:: ada(code)
    :language: Ada

..
    Math symbols

.. |rightarrow| replace:: :math:`\rightarrow`
.. |forall| replace:: :math:`\forall`
.. |exists| replace:: :math:`\exists`
.. |equivalent| replace:: :math:`\iff`

..
    Miscellaneous symbols

.. |checkmark| replace:: :math:`\checkmark`

==============
Introduction
==============

--------------------------
Absence of Interferences
--------------------------

* Flow analysis rejects aliasing

  - Between two parameters
  - Between a parameter and a global variable
  - ... when that may lead to interferences

* Interferences when one of the variables is written

* Many features avoid direct use of pointers

  - Array types
  - By-reference parameter passing mode
  - Address specifications :ada:`X : Integer with Address => ...`
  - Generics (avoid C-style :code:`void*` genericity)

* What about pointers?

-----------------------
Pointers and Aliasing
-----------------------

* Pointers introduce aliasing

  - This violates SPARK principle of absence of interferences

* Rust programming language popularized :dfn:`ownership`

  - Only one pointer (the *owner*) at anytime has read-write access
  - Assigning a pointer transfers its ownership

* Work on ownership in SPARK started in 2017

  - First version released in SPARK Pro 20
  - Detection of memory leaks in SPARK Pro 21
  - Support for all access types in SPARK Pro 22
  - SPARK libraries for aliasing in SPARK Pro 23

====================
Ownership Checking
====================

---------------------
Access Types in Ada
---------------------

* Access-to-variable vs access-to-constant types

  .. code:: Ada

     AV : access Integer;
     AC : access constant Integer;

  - :ada:`AV` can be used to modify the integer, :ada:`AC` cannot

* Named vs anonymous access types

  .. code:: Ada

     type Acc is access Integer;
     AN : Acc;
     AA : access Integer;

  - Convenience in Ada to save the introduction of a type name

* Pool-specific vs general access types

  .. code:: Ada

     type PS_Acc is access Integer;
     type G_Acc is access all Integer;

  - Type :ada:`PS_Acc` can only point to the heap, :ada:`GS_Acc` can point to
    the heap and stack.

* Accessibility levels prevent escaping pointers to the stack

* Not null access types forbid use of value :ada:`null`

-----------------------
Access Types in SPARK
-----------------------

* Named pool-specific access-to-variable types: subject to ownership

  .. code:: Ada

     type PS_Int_Acc is access Integer;

* Named access-to-constant types: aliasing allowed, deallocation forbidden

  .. code:: Ada

     type Cst_Int_Acc is access constant Integer;

* Named general access-to-variable types: subject to ownership, deallocation forbidden

  .. code:: Ada

     type Gen_Int_Acc is access all Integer;

* Anonymous access-to-object types: for borrowing and observing

  .. code:: Ada

     X : access Cell := ...
     X : access constant Cell := ...

-------------------------
Memory Ownership Policy
-------------------------

* A chunk of memory has a single *owner*

* Assigning a pointer *moves* its ownership

* Only the owner can both read and write the memory

  .. code:: Ada

     X := new Integer'(1);
     --  X has the ownership of the cell
     Y := X;
     --  The ownership is moved to Y
     Y.all := Y.all + 1;
     --  Y can access and modify the data
     pragma Assert (X.all = 1);
     --  Error: X can no longer access the data

* Ownership policy ensures absence of interferences

----------------------------
Model of Pointers in SPARK
----------------------------

* Pointers are seen as records in analysis

  - Both for flow analysis and proof
  - This is possible thanks to absence of interferences

  .. code:: ada

     type Int_Acc is access Integer;
     X : Int_Acc := new Integer'(42);

  is treated like:

  .. code:: ada

     type Int_Acc (Nul : Boolean := False) is record
       case Nul is
         when True  => null;
         when False => Content : Integer;
       end case;
     end record;
     X : Int_Acc := Int_Acc'(Nul => False, Content => 42);

* Value of pointer itself is not modelled

  - This is an intentional limitation to

    + Allow allocators in expressions
    + Allow dellocation in functions

  - Equality of pointers is not supported (only with :ada:`null`)

-------------------------
Borrowing and Observing
-------------------------

* Borrowing is temporary read-write access

  - either through a declaration

    .. code:: Ada

       X : access Cell := Current_Cell.Next;

  - or through a call (access type can be named or anonymous)

    .. code:: Ada

       procedure Update_Cell (X : access Cell);
       Update_Cell (Current_Cell.Next);

* In-out parameter of access type is *moved* on entry and return

* Observing is temporary read-only access

  - either through a declaration

    .. code:: Ada

       X : access constant Cell := Current_Cell.Next;

  - or through a call

    .. code:: Ada

       procedure Read_Cell (X : access constant Cell);
       Read_Cell (Current_Cell.Next);

-------------------------
Access to Constant Data
-------------------------

* Data is constant all the way down

  - Data designated by the pointer is constant
  - Pointers in that data inherit the same property
  - This is specific to SPARK: in Ada only designated data is constant

* Also applies to constants and input parameters of composite types containing pointers

  - Different from input and constants of access-to-variable type

* Aliasing is allowed

-----------------------------
Access to Data on the Stack
-----------------------------

* Use attribute :ada:`Access` on local variable

  - Not allowed on global variable which would remain visible
  - Result of general access type with :ada:`access all` syntax

* :ada:`Constant'Access` of access-to-constant type

* :ada:`Variable'Access` of access-to-variable type

* Variable is *moved* and cannot be referenced anymore

-----------------------------------------------
Attributes :ada:`Old` and :ada:`Loop_Entry`
-----------------------------------------------

* Attributes :ada:`Old` and :ada:`Loop_Entry` not applicable to pointers

  - Implicit copy on subprogram/loop entry would violate ownership

* Prefix of access type needs to be a call to an *allocating function*

  - Allocating function is a function returing an access-to-variable type

  .. code:: Ada

     function Copy (X : Ptr) return Ptr
       with Post => Copy'Result.all = Ptr.all;

     procedure P (X : in out Ptr)
       with Post => Property (Copy (X)'Old);

-------------
Useful Tips
-------------

* No cycles or sharing inside mutable data structures

* Global objects can also be moved temporarily

  - Procedure must restore some value (or null) before returning

* Allocation function returns a new object of access-to-variable type

  - Similar to initialized allocator with :ada:`new T'(Value)`
  - Some special *traversal functions* give access to part of an object

* Deallocation procedure simply nullifies in-out access parameter

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
       Annotate => (GNATprove, Always_Return),
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

=================
SPARK Libraries
=================

------------------------------
Pointers with Aliasing (1/2)
------------------------------

* SPARK Library defines two generics

  - :ada:`SPARK.Pointers.Pointers_With_Aliasing`
  - :ada:`SPARK.Pointers.Pointers_With_Aliasing_Separate_Memory`
  - Only generic parameter is any type :ada:`Object`

* Both allow aliasing pointers

  - Type :ada:`Pointer` is private

    + User code can copy such pointers freely
    + Ownership policy does not apply

  - All accesses through API check validity of pointer

------------------------------
Pointers with Aliasing (1/2)
------------------------------

* Shared API to create, free, access pointers

  .. code:: ada

     procedure Create (O : Object; P : out Pointer);
     function Deref (P : Pointer) return Object;
     procedure Assign (P : Pointer; O : Object);
     procedure Dealloc (P : in out Pointer);

* Version in :ada:`Pointers_With_Aliasing_Separate_Memory` adds parameter

  .. code:: ada

     Memory : in out Memory_Type

  - To handle separate groups of pointers in different memories

* Use of pointers with aliasing is *possible* but *costly*

  - Need to maintain validity of pointers at all times
  - Need to maintain separation of pointers at all times
  - This comes from free with the ownership policy

=====
Lab
=====

.. include:: labs/12_pointer_programs.lab.rst

=========
Summary
=========

------------------
Pointer Programs
------------------

* Pointers are supported in SPARK

  - All kinds of pointers are supported
  - Access-to-constant is all the way down
  - General access cannot be deallocated

* Ownership policy is key

  - Ensures absence of interferences
  - Constrains code and data structures

    + No cyclic data structures

* Loops require special reasoning

  - So-called promises peek at value after borrow
  - Useful in loop invariants
