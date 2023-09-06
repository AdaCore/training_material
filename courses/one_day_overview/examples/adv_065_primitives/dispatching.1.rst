.. code:: Ada

      type T is tagged null record;

      procedure Prim (A : T) is null;
      procedure Not_Prim (A : T'Class) is null;
