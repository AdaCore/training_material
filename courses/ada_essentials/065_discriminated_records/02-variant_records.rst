=================
Variant Records
=================

---------------------------
What is a Variant Record?
---------------------------

* A :dfn:`variant record` uses the discriminant to determine which components are currently accessible

   .. code:: Ada

      type Category_T is (Employee, Contractor);
      type Employee_T (Kind : Category_T) is record
         Name : String_T;
         DOB  : Date_T;
         case Kind is
            when Employee =>
               Pay_Rate  : Pay_T;
            when Contractor =>
               Hourly_Rate : Contractor_Rate_T;
         end case;
      end record;

      An_Employee     : Employee_T (Employee);
      Some_Contractor : Employee_T (Contractor);

.. note::

   :ada:`case` block must be **last** part of the definition - therefore only **one** per record

* Variant records are considered the same type

   * So you can have

      .. code:: Ada

         procedure Print (Item : Employee_T);

         Print (An_Employee);
         Print (Some_Contractor);

--------------------------
Immutable Variant Record
--------------------------

* In an :dfn:`immutable variant record` the discriminant has no default value

   * It is an :dfn:`indefinite type`, similar to an unconstrained array

     * So you must add a constraint (discriminant) when creating an object
     * But it can be unconstrained when used as a parameter

* For example

   .. code:: Ada
      :number-lines: 24

      Pat     : Employee_T (Employee);
      Sam     : Employee_T :=
         (Kind        => Contractor,
          Name        => From_String ("Sam"),
          DOB         => "2000/01/01",
          Hourly_Rate => 123.45);
      Illegal : Employee_T;  -- indefinite

--------------------------------
Immutable Variant Record Usage
--------------------------------

* Compiler can detect some problems

   .. code:: Ada

      begin
         Pat.Hourly_Rate := 12.3;
      end;

   ``warning: component not present in subtype of "Employee_T" defined at line 24``

* But more often clashes are run-time errors

   .. code:: Ada
     :number-lines: 32

     procedure Print (Item : Employee_T) is
     begin
       Print (Item.Pay_Rate);

   ``raised CONSTRAINT_ERROR : print.adb:34 discriminant check failed``
  
* :ada:`Pat := Sam;` would be a compiler warning because the constraints do not match

------------------------
Mutable Variant Record
------------------------

* To add flexibility, we can make the type :dfn:`mutable` by specifying a default value for the discriminant

   .. code:: Ada

      type Mutable_T (Kind : Category_T := Employee) is record
         Name : String_T;
         DOB  : Date_T;
         case Kind is
            when Employee =>
               Pay_Rate  : Pay_T;
            when Contractor =>
               Hourly_Rate : Contractor_Rate_T;
      end record;

      Pat : Mutable_T;
      Sam : Mutable_T (Contractor);

* Making the variant mutable creates a definite type

   * An object can be created without a constraint (:ada:`Pat`)
   * Or we can create in immutable object where the discriminant cannot change (:ada:`Sam`)
   * And we can create an array whose component is mutable

--------------------------------
Mutable Variant Record Example
--------------------------------

* You can only change the discriminant of :ada:`Pat`, but only via a whole record assignment, e.g:

  .. code:: Ada

    if Pat.Group = Student then
      Pat := (Faculty, Pat.Age, 1);
    else
      Pat := Sam;
    end if;
    Update (Pat);

* But you cannot change the discriminant like a regular component

  .. code:: Ada

    Pat.Kind := Contractor; -- compile error

  ``error: assignment to discriminant not allowed``
    
* And you cannot change the discriminant of :ada:`Sam`

  * :ada:`Sam := Pat;` will give you a run-time error if :ada:`Pat.Kind` is not :ada:`Contractor`

    * And the compiler will not warn about this!

------
Quiz
------

.. code:: Ada

    type Variant_T (Sign : Integer) is record
        case Sign is
        when Integer'First .. -1 =>
            I : Integer;
            B : Boolean;
        when others =>
            N : Natural;
        end case;
    end record;

    Variant_Object : Variant_T (1);

Which component(s) does :ada:`Variant_Object` contain?

A. :ada:`Variant_Object.I, Variant_Object.B`
B. :answermono:`Variant_Object.N`
C. None: Compilation error
D. None: Run-time error

------
Quiz
------

.. container:: columns

  .. container:: column

    .. container:: latex_environment tiny

      .. code:: Ada
         :number-lines: 2

         type Coord_T is record
            X, Y : Float;
         end record;

         type Kind_T is (Circle, Line);
         type Shape_T (Kind : Kind_T := Line) is record
            Origin : Coord_T;
            case Kind is
               when Line =>
                  End_Point : Coord_T;
               when Circle =>
                  End_Point : Coord_T;
            end case;
         end record;

         A_Circle : Shape_T       :=
           (Circle, (1.0, 2.0), (3.0, 4.0));
         A_Line   : Shape_T (Line) :=
           (Circle, (1.0, 2.0), (3.0, 4.0));

  .. container:: column

    .. container:: latex_environment small

      What happens when you try to build and run this code?

      A. Run-time error
      B. Compilation error on an object
      C. :answer:`Compilation error on a type`
      D. No problems

.. container:: animate

  .. container:: latex_environment footnotesize

   * If you fix the compilation error (by changing the name of one of the :ada:`End_Point` components), then

      * You would get a warning on line 20 (because :ada:`A_Line` is constrained to be a :ada:`Line`

         ``incorrect value for discriminant "Kind"``

      * If you then ran the executable, you would get an exception 

         ``CONSTRAINT_ERROR : test.adb:20 discriminant check failed``
   
