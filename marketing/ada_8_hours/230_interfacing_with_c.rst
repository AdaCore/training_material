**********************
Interfacing with C
**********************

..
    Coding language

.. role:: ada(code)
    :language: Ada

.. role:: C(code)
    :language: C

.. role:: cpp(code)
    :language: C++

..
    Math symbols

.. |rightarrow| replace:: :math:`\rightarrow`
.. |forall| replace:: :math:`\forall`
.. |exists| replace:: :math:`\exists`
.. |equivalent| replace:: :math:`\iff`
.. |le| replace:: :math:`\le`
.. |ge| replace:: :math:`\ge`
.. |lt| replace:: :math:`<`
.. |gt| replace:: :math:`>`

..
    Miscellaneous symbols

.. |checkmark| replace:: :math:`\checkmark`

=================
Import / Export
=================

-------------------------------
Import / Export Aspects (1/2)
-------------------------------

* Aspect :ada:`Import` allows a C implementation to complete an Ada specification

   - Ada view

      .. code:: Ada

         procedure C_Proc
            with Import,
                 Convention    => C,
                 External_Name => "c_proc";

   - C implementation

       .. code:: C

          void SomeProcedure (void) {
             // some code
          }

* Aspect :ada:`Export` allows an Ada implementation to complete a C specification

   - Ada implementation

       .. code:: Ada

          procedure Some_Procedure
             with Export,
                  Convention    => C,
                  External_Name => "ada_some_procedure");

          procedure Some_Procedure is
          begin
           -- some code
          end Some_Procedure;

   - C view

       .. code:: C

          extern void ada_some_procedure (void);

-------------------------------
Import / Export Aspects (2/2)
-------------------------------

* You can also import/export variables

   - Variables imported won't be initialized
   - Ada view

      .. code:: Ada

         My_Var : Integer_Type
            with Import,
                 Convention    => C,
                 External_Name => "my_var");

   - C implementation

      .. code:: C

         int my_var;

===================
Parameter Passing
===================

-----------------------------------
Passing Scalar Data as Parameters
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
          return Interfaces.C.Int
          with Import,
               Convention    => C,
               External_Name => "c_proc");

* C view

   .. code:: C

     int c_proc (int i) {
       /* some code */
     }

-----------------------------------
Passing Structures as Parameters
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

-----------------
Parameter modes
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
