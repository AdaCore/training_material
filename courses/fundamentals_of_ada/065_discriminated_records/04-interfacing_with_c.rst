====================
Interfacing with C
====================

-----------------------------------
Passing Records Between Ada and C
-----------------------------------

* Your Ada code needs to call C that looks like this:

   .. code:: C

      struct Struct_T {
         int   Component1;
         char  Component2;
         float Component3;
      };

      int DoSomething (struct Struct_T);

* Ada has mechanisms that will allow you to 

   * Call :C:`DoSomething`
   * Build a record that is binary-compatible to :C:`Struct_T`

--------------------------------
Building a C-Compatible Record
--------------------------------

* To build an Ada record for :C:`Struct_T`, start with a regular record:

   .. code:: Ada

      type Struct_T is record
         Component1 : Interfaces.C.int;
         Component2 : Interfaces.C.char;
         Component3 : Interfaces.C.C_Float;
      end record;

   * We use types from :ada:`Interfaces.C` to map directly to the C types

* But the Ada compiler needs to know that the record layout must match C

   * So we add an aspect to enforce it

   .. code:: Ada

      type Struct_T is record
         Component1 : Interfaces.C.int;
         Component2 : Interfaces.C.char;
         Component3 : Interfaces.C.C_Float;
      end record with Convention => C_Pass_By_Copy;

-------------------------
Mapping Ada to C Unions
-------------------------

* Discriminant records are similar to C's :c:`union`, but with a limitation

   * Only one part of the record is available at any time

* So, you create the equivalent of this C :c:`union`

   .. code:: C

      union Union_T {
         int Component1;
         char Component2;
         float Component3;
      };

* By using a discriminant record and adding aspect :ada:`Unchecked_Union`

   .. code:: Ada

      type C_Union_T (View : natural := 0) is record
         case View is
         when 0 => Component1 : Interfaces.C.int;
         when 1 => Component2 : Interfaces.C.char;
         when 2 => Component3 : Interfaces.C.C_Float;
         when others => null;
         end case;
      end record with Convention => C_Pass_By_Copy,
                      Unchecked_Union;

   * This tells the compiler not to reserve space in the record for the discriminant

------
Quiz
------

.. code:: C

   union Union_T {
      struct Record_T component1;
      char            component2[11];
      float           component3;
   };

.. code:: Ada

    type C_Union_T (Flag : Natural := 1) is record
        case Sign is
        when 1 =>
            One   : Record_T;
        when 2 =>
            Two   : String(1 .. 11);
        when 3 =>
            Three : Float;
        end case;
    end record;

    C_Object : C_Union_T;

Which component does :ada:`C_Object` contain?

   A. :ada:`C_Object.One`
   B. :ada:`C_Object.Two`
   C. :answer:`None: Compilation error`
   D. None: Run-time error

.. container:: animate

    The variant :ada:`case` must cover all the possible values of :ada:`Natural`.

