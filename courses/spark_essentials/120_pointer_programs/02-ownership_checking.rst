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

|

* Also applies to constants and input parameters of composite types containing pointers

  - Different from constants and input parameters of access-to-variable type

|

* Aliasing is allowed

-----------------------------
Access to Data on the Stack
-----------------------------

* Use attribute :ada:`Access` on local variable

  - Not allowed on global variable which would remain visible
  - Result of general access type with :ada:`access all` syntax

|

* :ada:`Constant'Access` of access-to-constant type

|

* :ada:`Variable'Access` of access-to-variable type

|

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
       with Post => Copy'Result.all = X.all;

     procedure P (X : in out Ptr)
       with Post => Property (Copy (X)'Old);

-------------
Useful Tips
-------------

* No cycles or sharing inside mutable data structures

|

* Global objects can also be moved temporarily

  - Procedure must restore some value (or null) before returning

|

* Allocation function returns a new object of access-to-variable type

  - Similar to initialized allocator with :ada:`new T'(Value)`
  - Some special *traversal functions* give access to part of an object

|

* Deallocation procedure simply nullifies in-out access parameter

