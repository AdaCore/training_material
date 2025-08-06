=========
Summary
=========

-------------------------------------
Generic Routines Vs Common Routines
-------------------------------------

.. code:: Ada

   package Helper is
      type Float_T is digits 6;
      generic
         type Type_T is digits <>;
         Min : Type_T;
         Max : Type_T;
      function In_Range_Generic (X : Type_T) return Boolean;
      function In_Range_Common (X   : Float_T;
                                Min : Float_T;
                                Max : Float_T)
                                return Boolean;
   end Helper;

   procedure User is
     type Speed_T is new Float_T range 0.0 .. 100.0;
     B : Boolean;
     function Valid_Speed is new In_Range_Generic
        (Speed_T, Speed_T'First, Speed_T'Last);
   begin
     B := Valid_Speed (12.3);
     B := In_Range_Common (12.3, Speed_T'First, Speed_T'Last);

.. container:: speakernote

   Generics increase code size and readability
   Common functions reduce size, but increase error possibilities

---------
Summary
---------

* Generics are useful for copying code that works the same just for different types

   - Sorting, containers, etc

* Properly written generics only need to be tested once

   - But testing / debugging can be more difficult

* Generic instantiations are best done at compile time

   - At the package level
   - Can be run time expensive when done in subprogram scope
