=========
Example
=========

----------------------------------
Unbounded String Via Access Type
----------------------------------

* Type contains a pointer to a string type
* We want the provider to allocate and free memory "safely"

   - No sharing
   - `Adjust` allocates referenced String
   - `Finalize` frees the referenced String
   - Assignment deallocates target string and assigns copy of source string to target string

------------------------
Unbounded String Usage
------------------------

.. code:: Ada

   with Unbounded_String_Pkg; use Unbounded_String_Pkg;
   procedure Test is
      U1 : Ustring_T;
   begin
      U1 := To_Ustring_T ("Hello");
      declare
         U2 : Ustring_T;
      begin
         U2 := To_Ustring_T ("Goodbye");
         U1 := U2; -- Reclaims U1 memory
      end; -- Reclaims U2 memory
   end Test; -- Reclaims U1 memory

-----------------------------
Unbounded String Definition
-----------------------------

.. code:: Ada

   with Ada.Finalization; use Ada.Finalization;
   package Unbounded_String_Pkg is
      -- Implement unbounded strings
      type Ustring_T is private;
      function "=" (L, R : Ustring_T) return Boolean;
      function To_Ustring_T (Item : String) return Ustring_T;
      function To_String (Item : Ustring_T) return String;
      function Length (Item : Ustring_T) return Natural;
      function "&" (L, R : Ustring_T) return Ustring_T;
   private
      type String_Ref is access String;
      type Ustring_T is new Controlled with record
         Ref : String_Ref := new String (1 .. 0);
      end record;
      procedure Finalize (Object : in out Ustring_T);
      procedure Adjust (Object : in out Ustring_T);
   end Unbounded_String_Pkg;

---------------------------------
Unbounded String Implementation
---------------------------------

.. code:: Ada

   with Ada.Unchecked_Deallocation;
   package body Unbounded_String_Pkg is
      procedure Free_String is new Ada.Unchecked_Deallocation
        (String, String_Ref);

      function "=" (L, R : Ustring_T) return Boolean is
         (L.Ref.all = R.Ref.all);

      function To_Ustring_T (Item : String) return Ustring_T is
         (Controlled with Ref => new String'(Item));

      function To_String (Item : Ustring_T) return String is
         (Item.Ref.all);

      function Length (Item : Ustring_T) return Natural is
         (Item.Ref.all'Length);

      function "&" (L, R : Ustring_T) return Ustring_T is
         (Controlled with Ref => new String'(L.Ref.all & R.Ref.all);

      procedure Finalize (Object : in out Ustring_T) is
      begin
         Free_String (Object.Ref);
      end Finalize;

      procedure Adjust (Object : in out Ustring_T) is
      begin
         Object.Ref := new String'(Object.Ref.all);
      end Adjust;
   end Unbounded_String_Pkg;

------------------
Finalizable Aspect
------------------

* Uses the GNAT-specific :ada:`with Finalizable` aspect

.. code:: Ada

   type Ctrl is record
      Id : Natural := 0;
   end record
     with Finalizable => (Initialize           => Initialize,
                          Adjust               => Adjust,
                          Finalize             => Finalize,
                          Relaxed_Finalization => True);

   procedure Adjust     (Obj : in out Ctrl);
   procedure Finalize   (Obj : in out Ctrl);
   procedure Initialize (Obj : in out Ctrl);

* :ada:`Initialize`, :ada:`Adjust` same definition as previously
* :ada:`Finalize` has the :ada:`No_Raise` aspect: it cannot raise exceptions
* :ada:`Relaxed_Finalization`

    * Performance on-par with C++'s destructor
    * No automatic finalization of **heap-allocated** objects

