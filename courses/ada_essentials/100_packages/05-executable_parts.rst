==================
Executable Parts
==================

--------------------------
Optional Executable Part
--------------------------

.. code:: Ada

   package_body ::=
       package body name is
          declarative_part
       [ begin
          handled_sequence_of_statements ]
       end [ name ];

.. container:: speakernote

   Executable part is optional

---------------------------
Executable Part Semantics
---------------------------

* Executed only once, when package is elaborated
* Ideal when statements are required for initialization

   - Otherwise initial values in variable declarations would suffice

.. code:: Ada

   package body Random is
     Seed1, Seed2 : Integer;
     Call_Count : Natural := 0;
     procedure Initialize (Seed1 : out Integer;
                           Seed2 : out Integer) is ...
     function Number return Float is ...
   begin -- Random
     Initialize (Seed1, Seed2);
   end Random;

.. container:: speakernote

   Maybe initialization requires both values at once, hence two separate initializations (e.g., function calls) won't suffice, unlike CallCount.

------------------------------------------
Requiring/Rejecting Bodies Justification
------------------------------------------

.. container:: columns

 .. container:: column

    * Consider the alternative: an optional package body that becomes obsolete prior to building
    * Builder could silently choose not to include the package in executable

       - Package executable part might do critical initialization!

 .. container:: column

    .. code:: Ada

       package P is
         Data : array (L .. U) of
             Integer;
       end P;

       package body P is
         ...
       begin
         for K in Data'Range loop
           Data (K) := ...
         end loop;
       end P;

---------------------------------------
Forcing a Package Body to Be Required
---------------------------------------

.. container:: columns

 .. container:: column

    * Use :ada:`pragma Elaborate_Body`

       - Says to elaborate body immediately after spec
       - Hence there must be a body!

    * Additional pragmas we will examine later

 .. container:: column

    .. code:: Ada

       package P is
         pragma Elaborate_Body;
         Data : array (L .. U) of
             Integer;
       end P;

       package body P is
         ...
       begin
         for K in Data'Range loop
           Data (K) := ...
         end loop;
       end P;

