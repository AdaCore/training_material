========
Idioms
========

---------------------------------------
Effects of Hiding Type Representation
---------------------------------------

* Makes users independent of representation

   - Changes cannot require users to alter their code
   - Software engineering is all about money...

* Makes users dependent upon exported operations

   - Because operations requiring representation info are not available to users

      + Expression of values (aggregates, etc.)
      + Assignment for limited types

* Common idioms are a result

   - :dfn:`Constructor`
   - :dfn:`Selector`

--------------
Constructors
--------------

* Create designer's objects from user's values
* Usually functions

.. code:: Ada

   package Complex is
     type Number is private;
     function Make (Real_Part : Float; Imaginary : Float) return Number;
   private
     type Number is record ...
   end Complex;

   package body Complex is
      function Make (Real_Part : Float; Imaginary_Part : Float)
        return Number is ...
   end Complex:
   ...
   A : Complex.Number :=
       Complex.Make (Real_Part => 2.5, Imaginary => 1.0);

----------------------------
Procedures As Constructors
----------------------------

* Spec

   .. code:: Ada

      package Complex is
        type Number is private;
        procedure Make (This : out Number;  Real_Part, Imaginary : in Float) ;
        ...
      private
        type Number is record
          Real_Part, Imaginary : Float;
        end record;
      end Complex;

* Body (partial)

   .. code:: Ada

      package body Complex is
        procedure Make (This : out Number;
                        Real_Part, Imaginary : in Float) is
          begin
            This.Real_Part := Real_Part;
            This.Imaginary := Imaginary;
          end Make;
      ...

-----------
Selectors
-----------

* Decompose designer's objects into user's values
* Usually functions

.. code:: Ada

   package Complex is
     type Number is private;
     function Real_Part (This: Number) return Float;
     ...
   private
     type Number is record
       Real_Part, Imaginary : Float;
     end record;
   end Complex;

   package body Complex is
     function Real_Part (This : Number) return Float is
     begin
       return This.Real_Part;
     end Real_Part;
     ...
   end Complex;
   ...
   Phase : Complex.Number := Complex.Make (10.0, 5.5);
   Object : Float := Complex.Real_Part (Phase);

