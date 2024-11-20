=================
Null Statements
=================

-----------------
Null Statements
-----------------

* Explicit no-op statement
* Constructs with required statement
* Explicit statements help compiler

    - Oversights
    - Editing accidents

.. code:: Ada

   case Today is
     when Monday .. Thursday =>
       Work (9.0);
     when Friday =>
       Work (4.0);
     when Saturday .. Sunday =>
       null;
   end case;

