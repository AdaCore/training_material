=====================================
Combining Limited and Private Views
=====================================

-----------------------
Limited Private Types
-----------------------

* A combination of :ada:`limited` and :ada:`private` views

   - No client compile-time visibility to representation
   - No client assignment or predefined equality

* The typical design idiom for :ada:`limited` types
* Syntax

   - Additional reserved word :ada:`limited` added to :ada:`private` type declaration

   .. code:: Ada

      type defining_identifier is limited private;

------------------------------------
Limited Private Type Rationale (1)
------------------------------------

.. code:: Ada

   package Multiprocessor_Mutex is
     -- copying is prevented
     type Spin_Lock is limited record
       -- but users can see this!
       Flag : Interfaces.Unsigned_8;
     end record;
     procedure Lock (This : in out Spin_Lock);
     procedure Unlock (This : in out Spin_Lock);
     pragma Inline (Lock, Unlock);
   end Multiprocessor_Mutex;

------------------------------------
Limited Private Type Rationale (2)
------------------------------------

.. code:: Ada

   package MultiProcessor_Mutex is
     -- copying is prevented AND users cannot see contents
     type Spin_Lock is limited private;
     procedure Lock (The_Lock : in out Spin_Lock);
     procedure Unlock (The_Lock : in out Spin_Lock);
     pragma Inline (Lock, Unlock);
   private
     type Spin_Lock is ...
   end MultiProcessor_Mutex;

----------------------------------
Limited Private Type Completions
----------------------------------

* Clients have the partial view as :ada:`limited` and :ada:`private`
* The full view completion can be any kind of type
* Not required to be a record type just because the partial view is limited

.. code:: Ada

   package P is
     type Unique_ID_T is limited private;
     ...
   private
     type Unique_ID_T is range 1 .. 10;
   end P;

-----------------------------
Write-Only Register Example
-----------------------------

.. code:: Ada

   package Write_Only is
     type Byte is limited private;
     type Word is limited private;
     type Longword is limited private;
     procedure Assign (Input : in Unsigned_8;
                       To    : in out Byte);
     procedure Assign (Input : in Unsigned_16;
                       To    : in out Word);
     procedure Assign (Input : in Unsigned_32;
                       To    : in out Longword);
   private
     type Byte is new Unsigned_8;
     type Word is new Unsigned_16;
     type Longword is new Unsigned_32;
   end Write_Only;

--------------------------------
Explicitly Limited Completions
--------------------------------

* Completion in Full view includes word :ada:`limited`
* Optional
* Requires a record type as the completion

.. code:: Ada

   package MultiProcessor_Mutex is
     type Spin_Lock is limited private;
     procedure Lock (This : in out Spin_Lock);
     procedure Unlock (This : in out Spin_Lock);
   private
     type Spin_Lock is limited -- full view is limited as well
       record
         Flag : Interfaces.Unsigned_8;
       end record;
   end MultiProcessor_Mutex;

-------------------------------------------
Effects of Explicitly Limited Completions
-------------------------------------------

* Allows no internal copying too
* Forces parameters to be passed by-reference

.. code:: Ada

   package MultiProcessor_Mutex is
     type Spin_Lock is limited private;
     procedure Lock (This : in out Spin_Lock);
     procedure Unlock (This : in out Spin_Lock);
   private
     type Spin_Lock is limited record
       Flag : Interfaces.Unsigned_8;
     end record;
   end MultiProcessor_Mutex;

---------------------------------
Automatically Limited Full View
---------------------------------

* When other limited types are used in the representation
* Recall composite types containing limited types are :ada:`limited` too

.. code:: Ada

   package Foo is
      type Legal is limited private;
      type Also_Legal is limited private;
      type Not_Legal is private;
      type Also_Not_Legal is private;
   private
      type Legal is record
         S : A_Limited_Type;
      end record;
      type Also_Legal is limited record
         S : A_Limited_Type;
      end record;
      type Not_Legal is limited record
         S : A_Limited_Type;
      end record;
      type Also_Not_Legal is record
         S : A_Limited_Type;
      end record;
   end Foo;

.. container:: speakernote

   Also_Legal adds "limited" to the full view
   Not_Legal puts more limitations on full view than partial view
   Also_Not_Legal never shows the client that S is limited

------
Quiz
------

.. include:: ../quiz/limited_private/quiz.rst

------
Quiz
------

.. container:: latex_environment footnotesize

 .. container:: columns

  .. container:: column

   .. code:: Ada

      package P is
         type L1_T is limited private;
         type L2_T is limited private;
         type P1_T is private;
         type P2_T is private;
      private
         type L1_T is limited record
            Field : Integer;
         end record;
         type L2_T is record
            Field : Integer;
         end record;
         type P1_T is limited record
            Field : L1_T;
         end record;
         type P2_T is record
            Field : L2_T;
         end record;
      end P;

  .. container:: column

   What will happen when the above code is compiled?

   A. :answer:`Type P1_T will generate a compile error`
   B. Type P2_T will generate a compile error
   C. Both type P1_T and type P2_T will generate compile errors
   D. The code will compile successfully

   .. container:: animate

      The full definition of type :ada:`P1_T` adds additional
      restrictions, which is not allowed. Although :ada:`P2_T`
      contains a component whose visible view is :ada:`limited`,
      the internal view is not :ada:`limited` so :ada:`P2_T` is
      not :ada:`limited`.

