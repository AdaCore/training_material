=================
Import / Export
=================

-------------------------------
Import / Export Aspects (1/2)
-------------------------------

* Aspects :ada:`Import` and :ada:`Export` allow Ada and C to interact

   * :ada:`Import` indicates a subprogram imported into Ada
   * :ada:`Export` indicates a subprogram exported from Ada

* Need aspects definining calling convention and external name

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
         External_Name => "some_ada_procedure;

* C implementation

    .. code:: C

       void SomeProcedureInC (void) {
          // some code
       }

       extern void ada_some_procedure (void);

..
  language_version 2012

-------------------------------
Import / Export Aspects (2/2)
-------------------------------

* You can also import/export variables

   - Variables imported won't be initialized
   - Ada view

      .. code:: Ada

         My_Var : Integer_Type with 
            Import,
            Convention    => C,
            External_Name => "my_var";
         Pragma Import (C, My_Var, "my_var");

   - C implementation

      .. code:: C

         int my_var;

..
  language_version 2012

------------------------------
Import / Export with Pragmas
------------------------------

* You can also use :ada:`pragma` to import/export entities

   .. code:: Ada

      procedure C_Some_Procedure;
      pragma Import (C, C_Some_Procedure, "SomeProcedure");

      procedure Some_Procedure;
      pragma Export (C, Some_Procedure, "ada_some_procedure");

