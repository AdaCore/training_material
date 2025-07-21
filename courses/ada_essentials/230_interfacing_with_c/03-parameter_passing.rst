===================
Parameter Passing
===================

-----------------------------
Parameter Passing to/from C
-----------------------------

* The mechanism used to pass formal subprogram parameters and function results depends on:

   - The type of the parameter
   - The mode of the parameter
   - The Convention applied on the Ada side of the subprogram declaration

* The exact meaning of *Convention C*, for example, is documented in *LRM* B.1 - B.3, and in the *GNAT User's Guide* section 3.11.

-----------------------------------
Passing Scalar Data As Parameters
-----------------------------------

* C types are defined by the Standard
* Ada types are implementation-defined
* GNAT standard types are compatible with C types

   - Implementation choice, use carefully

* At the interface level, scalar types must be either constrained with representation clauses, or coming from Interfaces.C
* Ada view

   .. code:: Ada

      with Interfaces.C;
      function C_Proc (I : Interfaces.C.Int)
          return Interfaces.C.Int;
      pragma Import (C, C_Proc, "c_proc");

* C view

   .. code:: C

     int c_proc (int i) {
       /* some code */
     }

-----------------------------------
Passing Structures As Parameters
-----------------------------------

* An Ada record that is mapping on a C struct must:

   - Be marked as convention C to enforce a C-like memory layout
   - Contain only C-compatible types

* C View

   .. code:: C

     enum Enum {E1, E2, E3};
     struct Rec {
        int A, B;
        Enum C;
     };

* Ada View

   .. code:: Ada
   
     type Enum is (E1, E2, E3) with Convention => C;
     type Rec is record
       A, B : int;
       C : Enum;
     end record with Convention => C;

* This can also be done with pragmas

   .. code:: Ada

     type Enum is (E1, E2, E3);
     Pragma Convention (C, Enum);
     type Rec is record
       A, B : int;
       C : Enum;
     end record;
     Pragma Convention (C, Rec);

..
  language_version 2012

-----------------
Parameter Modes
-----------------

* :ada:`in` scalar parameters passed by copy
* :ada:`out` and :ada:`in out` scalars passed using temporary pointer on C side
* By default, composite types passed by reference on all modes except when the type is marked :ada:`C_Pass_By_Copy`

   - Be very careful with records - some C ABI pass small structures by copy!

* Ada View

   .. code:: Ada

      Type R1 is record
         V : int;
      end record
      with Convention => C;

      type R2 is record
         V : int;
      end record
      with Convention => C_Pass_By_Copy;

* C View

   .. code:: C

      struct R1{
         int V;
      };
      struct R2 {
         int V;
      };
      void f1 (R1 p);
      void f2 (R2 p);

