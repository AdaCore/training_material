===========================
Limited Type Declarations
===========================

---------------------------
Limited Type Declarations
---------------------------

* Syntax

   - Additional keyword limited added to record type declaration

   .. code:: Ada

      type <identifier> is limited record
          <component_list>
      end record;

* Are always record types unless also private

   - More in a moment...

---------------------------
Approximate Analog in C++
---------------------------

.. code:: C++

   class Stack {
   public:
     Stack ();
     void Push (int X);
     void Pop (int& X);
     ...
   private:
     ...
     // assignment operator hidden
     Stack& operator= (const Stack& other);
   }; // Stack

-------------------
Spin Lock Example
-------------------

.. code:: Ada

   with Interfaces;
   package Multiprocessor_Mutex is
     -- prevent copying of a lock
     type Spin_Lock is limited record
       Flag : Interfaces.Unsigned_8;
     end record;
     procedure Lock  (This : in out Spin_Lock);
     procedure Unlock  (This : in out Spin_Lock);
     pragma Inline (Lock, Unlock);
   end Multiprocessor_Mutex;

-----------------------------
Parameter Passing Mechanism
-----------------------------

* Always "by-reference" if explicitly limited

   - Necessary for various reasons (:ada:`task` and :ada:`protected` types, etc)
   - Advantageous when required for proper behavior

* By definition, these subprograms would be called concurrently

   - Cannot operate on copies of parameters!

.. code:: Ada

   procedure Lock  (This : in out Spin_Lock);
   procedure Unlock (This : in out Spin_Lock);

-------------------------------------
Composites with Limited Types
-------------------------------------

* Composite containing a limited type becomes limited as well

   * Example: Array of limited components

      - Array becomes a limited type

   * Prevents assignment and equality loop-holes

.. code:: Ada

   declare
     -- if we can't copy component S, we can't copy User_Type
     type User_Type is record -- limited because S is limited
       S : File;
       ...
     end record;
     A, B : User_Type;
   begin
     A := B;  -- not legal since limited
     ...
   end;

------
Quiz
------

.. include:: ../quiz/limited_syntax/quiz.rst

------
Quiz
------

.. include:: ../quiz/limited_operators/quiz.rst
..
------
Quiz
------

.. code:: Ada

   package P is
      type T is limited null record;
      type R is record
         F1 : Integer;
         F2 : T;
      end record;
   end P;

   with P;
   procedure Main is
      T1, T2 : P.T;
      R1, R2 : P.R;
   begin

Which assignment(s) is (are) legal?

   A. ``T1    := T2;``
   B. ``R1    := R2;``
   C. :answermono:`R1.F1 := R2.F1;`
   D. ``R2.F2 := R2.F2;``

.. container:: animate

   Explanations

   A. :ada:`T1` and :ada:`T2` are :ada:`limited types`
   B. :ada:`R1` and :ada:`R2` contain :ada:`limited` types so they are also :ada:`limited`
   C. Theses components are not :ada:`limited` types
   D. These components are of a :ada:`limited` type

