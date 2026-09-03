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

* The exact meaning of *Convention C*, for example, is documented in *RM* B.1 - B.3, and in the *GNAT User's Guide* section 3.11.

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
     function C_Proc (I : Interfaces.C.int)
         return Interfaces.C.int
         with Import, 
              Convention    => C,
              External_Name => "c_proc";

* C view

  .. code:: C

    int c_proc (int i) {
      /* some code */
    }

----------------------------------
Passing Structures As Parameters
----------------------------------

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

-----------------------------
Parameter Modes for Scalars
-----------------------------

* :ada:`in` scalar parameters passed by copy
* :ada:`out` and :ada:`in out` scalars passed using temporary pointer on C side

* Ada View

  .. code:: Ada

    procedure Use_Scalar
      (Value  : in out Interfaces.C.int;
       Change : in     Interfaces.C.int)
      with Import,
           Convention => C,
           External_Name => "use_scalar";

    The_Value : Interfaces.C.int := 1200;

    Use_Scalar (The_Value, 34);
    Put_Line (The_Value'Image);

* C View

  .. code:: C

    void use_scalar (int *value, int change) {
      *value = *value + change;
    }

.. code:: output

  1234

-------------------------------------------
Parameter Modes for Composite Types (1/2)
-------------------------------------------

* Composite types passed by reference for all modes

  * Except records with the aspect :ada:`C_Pass_By_Copy` passed as :ada:`in`

* Ada View

  .. code:: Ada

   type Reference_T is record
      Sensor_ID : Interfaces.C.int;
      Reading   : Interfaces.C.double;
   end record with Convention => C;

   type Copy_T is record
      Sensor_ID : Interfaces.C.int;
      Reading   : Interfaces.C.double;
   end record with Convention => C_Pass_By_Copy;

   procedure Pass_By_Reference (Data : in Reference_T)
     with Import,
          Convention    => C,
          External_Name => "pass_by_reference";

   procedure Pass_By_Copy (Data : in Copy_T)
     with Import,
          Convention    => C,
          External_Name => "pass_by_copy";

   Reference : Reference_T := (1, 2.3);
   Copy : Copy_T := (4, 5.6);

   Pass_By_Reference (Reference);
   Pass_By_Copy (Copy);

-------------------------------------------
Parameter Modes for Composite Types (2/2)
-------------------------------------------

* C View

  .. code:: C

    //  This type matches both Ada types!
    //  (Identical format, just different passing mechanisms)
    typedef struct {
        int sensor_id;
        double reading;
    } Struct_T;

    // Pass by reference provides a pointer to the data
    void pass_by_reference(const Struct_T *data) {
      printf("ID: %d, Value: %f\n", data->sensor_id, data->reading);
    }

    // Pass by copy provides the actual data
    void pass_by_copy(Struct_T data) {
      printf("ID: %d, Value: %f\n", data.sensor_id, data.reading);
    }

.. code:: output

  ID: 1, Value: 2.300000
  ID: 4, Value: 5.600000

.. warning::

  Be very careful with records - some C ABIs pass small structures by copy!
