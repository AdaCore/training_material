=================
Import / Export
=================

-------------------------------
Import / Export Aspects (1/2)
-------------------------------

* Aspects :ada:`Import` and :ada:`Export` allow Ada and C to interact

   * :ada:`Import` indicates a subprogram imported into Ada
   * :ada:`Export` indicates a subprogram exported from Ada

* Need aspects defining calling convention and external name

  * :ada:`Convention => C` tells linker to use C-style calling convention
  * :ada:`External_Name => "<name>"` defines object name for linker

* Ada implementation

  .. code:: Ada

     procedure Imported_From_C with
        Import,
        Convention    => C, 
        External_Name => "SomeProcedureInC";

     procedure Exported_To_C with
        Export,
        Convention    => C, 
        External_Name => "some_ada_procedure";

* C implementation

  .. code:: C

     void SomeProcedureInC (void) {
        // some code
     }

     extern void some_ada_procedure (void);

-------------------------------
Import / Export Aspects (2/2)
-------------------------------

* Can also import/export variables

   - Variables imported won't be initialized
   - Ada view

     .. code:: Ada

        My_Var : Integer_Type with 
           Import,
           Convention    => C,
           External_Name => "my_var";

  - C implementation

    .. code:: C

        int my_var;
