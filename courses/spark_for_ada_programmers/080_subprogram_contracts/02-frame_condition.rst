=================
Frame Condition
=================

----------------------------
Quiz - Stating the Obvious
----------------------------

What is the problem with this postcondition?

.. code:: ada

   type Pair is record
      X, Y : Integer;
   end record;

   procedure Set_X (P : in out Pair; Value : Integer)
     with Post => P.X = Value;

.. container:: animate

   * The postcondition does not say that the value of :ada:`Y` is preserved!
   * As a result, nothing is known about :ada:`Y` after calling :ada:`Set_X`

     .. code:: ada

        P : Pair := Pair'(X => 1, Y => 2);
        P.Set_X (42);
        pragma Assert (P.Y = 2); -- unproved

---------------------------
Frame Condition - Records
---------------------------

* Simpler solution is to state which components are **preserved**

  .. code:: ada

     procedure Set_X (P : in out Pair; Value : Integer)
       with Post => P.X = Value and P.Y = P.Y'Old;

* Or with a **delta aggregate**

  .. code:: ada

     procedure Set_X (P : in out Pair; Value : Integer)
       with Post => P = (P'Old with delta X => Value);

* In both cases, value of :ada:`Y` is known to be preserved

--------------------------
Frame Condition - Arrays
--------------------------

* Use universal quantification to denote components preserved

  .. code:: ada

     procedure Swap_Table (T : in out Table; I, J : Index)
       with Post =>
         (for all K in T'Range =>
           (if K not in I | J then T (K) = T'Old (K)));

* Or with a delta aggregate

   .. code:: ada

     procedure Swap_Table (T : in out Table; I, J : Index)
       with Post =>
         T = (T'Old with delta I => T(J)'Old, J => T(I)'Old);

* In both cases, value of :ada:`T(K)` is known to be preserved for :ada:`K`
  different from :ada:`I` and :ada:`J`

------------------------------
Frame Condition - Conditions
------------------------------

* Any variable may be preserved conditionally

  - That applies also to scalar variables

  .. code:: ada

     procedure Zero_If (X : in out Integer; Cond : Boolean)
       with Post => (if Cond then X = 0);

* The preservation case needs to be **explicited**

  .. code:: ada

     procedure Zero_If (X : in out Integer; Cond : Boolean)
       with Post => (if Cond then X = 0 else X = X'Old);

* :dfn:`Frame condition` is **all** the parts of objects that may be preserved

  - Bounded by user-defined or generated **data dependencies**
  - Anything else needs to be stated **explicitly**

--------------------------------------------
Frame Condition - Bounds and Discriminants
--------------------------------------------

* Some parts of objects **cannot** be changed by a call

  - Array bounds
  - Discriminants of constrained records

* Special handling in :toolname:`GNATprove` to preserve them

  .. code:: ada

     type Rec (Disc : Boolean) is record ...

     procedure Change (T : in out Table; R : in out Rec)
       with Post =>
         T'First = T'First'Old         -- redundant
         and then T'Last = T'Last'Old  -- redundant
         and then R.Disc = R.Disc'Old; -- redundant

---------------------------------
Frame Condition - Private Types
---------------------------------

* Direct access to value or components not possible
* Simpler solution: define **query functions**

  - **Hide** access to value or components

  .. code:: ada

     type Pair is private;
     function Get_Y (P : Pair) return Integer;
     procedure Set_X (P : in out Pair; Value : Integer)
       with Post => P.Get_Y = P.Get_Y'Old;

* More comprehensive solution: define **model functions**

  - Create a visible **model** of the value

  .. code:: ada

     type Pair is private;
     type Pair_Model is record X, Y : Integer; end record;
     function Model (P : Pair) return Pair_Model;
     procedure Set_X (P : in out Pair; Value : Integer)
       with Post => P.Model = (P.Model'Old with delta X => Value);

-----------------------
Attribute :ada:`Old`
-----------------------

* Dynamic semantics is to make a copy at subprogram entry

  - Forbidden on :ada:`limited` types

|

* Evaluation for the copy may raise runtime errors

  - Not allowed by default inside *potentially unevaluated expressions*

    + Unless prefix is a variable

    .. code:: Ada

       procedure Extract (A : in out My_Array;
                          J : Integer;
                          V : out Value)
         with Post =>
           (if J in A'Range then V = A (J)'Old); -- Illegal

  |

  - Use :ada:`pragma Unevaluated_Use_Of_Old (Allow)` to allow

    + :toolname:`GNATprove` **checks** that this is safe

-----------------------------------------
Special Cases for Attribute :ada:`Old`
-----------------------------------------

* Simple component access :ada:`X.C'Old` equivalent to :ada:`X'Old.C`

  - Although one may be more efficient at runtime

|

* Function call in the prefix of :ada:`Old` is evaluated at subprogram entry

  - Value of **globals** is the one at subprogram entry
  - Not the same as calling the function on parameters with :ada:`Old`

    .. code:: Ada

       function F (X : Integer) return Integer
         with Global => Glob;

       procedure P (X : in out Integer)
         with Post =>
           F (X'Old) = 0 and then
           F (X)'Old = 0;

