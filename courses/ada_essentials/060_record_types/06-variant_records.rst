=================
Variant Records
=================

----------------------
Variant Record Types
----------------------

* :dfn:`Variant record` can use a **discriminant** to specify alternative lists of components

   + Also called :dfn:`discriminated record` type
   + Different **objects** may have **different** components
   + All objects **still** share the same type

* Kind of :dfn:`storage overlay`

   + Similar to :C:`union` in C
   + But preserves **type checking**
   + And object size **is related to** discriminant

* Aggregate assignment is allowed

--------------------------
Immutable Variant Record
--------------------------

* Discriminant must be set at creation time and cannot be modified

.. code:: Ada
   :number-lines: 2

  type Person_Group is (Student, Faculty);
  type Person (Group : Person_Group) is
  record
     --  Components common across all discriminants
     --  (must appear before variant part)
     Age : Positive;
     case Group is --  Variant part of record
        when Student => -- 1st variant
           Gpa  : Float range 0.0 .. 4.0;
        when Faculty => -- 2nd variant
           Pubs : Positive;
     end case;
  end record;

* In a variant record, a discriminant can be used to specify the :dfn:`variant part` (line 8)

   + Similar to case statements (all values must be covered)
   + Components listed will only be visible if choice matches discriminant
   + Component names need to be unique (even across discriminants)
   + Variant part must be end of record (hence only one variant part allowed)

* Discriminant is treated as any other component

  * But is a constant in an immutable variant record

*Note that discriminants can be used for other purposes than the variant part*

----------------------------------
Immutable Variant Record Example
----------------------------------

* Each object of :ada:`Person` has three components, but it depends on :ada:`Group`

    .. code:: Ada

      Pat : Person (Student);
      Sam : Person := (Faculty, 33, 5);

  * :ada:`Pat` has :ada:`Group`, :ada:`Age`, and :ada:`Gpa`
  * :ada:`Sam` has :ada:`Group`, :ada:`Age`, and :ada:`Pubs`
  * Aggregate specifies all components, including the discriminant

* Compiler can detect some problems, but more often clashes are run-time errors

  .. code:: Ada

    procedure Do_Something (Param : in out Person) is
    begin
      Param.Age := Param.Age + 1;
      Param.Pubs := Param.Pubs + 1;
    end Do_Something;

  * :ada:`Pat.Pubs := 3;` would generate a compiler warning because compiler knows :ada:`Pat` is a :ada:`Student`

    * ``warning: Constraint_Error will be raised at run time``

  * :ada:`Do_Something (Pat);` generates a run-time error, because only at runtime is the discriminant for :ada:`Param` known

    * ``raised CONSTRAINT_ERROR : discriminant check failed``

* :ada:`Pat := Sam;` would be a compiler warning because the constraints do not match

------------------------
Mutable Variant Record
------------------------

* Type will become :dfn:`mutable` if its discriminant has a *default value* **and** we instantiate the object without specifying a discriminant

.. code:: Ada
   :number-lines: 2

  type Person_Group is (Student, Faculty);
  type Person (Group : Person_Group := Student) is -- default value
  record
     Age : Positive;
     case Group is
        when Student =>
           Gpa  : Float range 0.0 .. 4.0;
        when Faculty =>
           Pubs : Positive;
     end case;
  end record;

* :ada:`Pat : Person;` is **mutable**
* :ada:`Sam : Person (Faculty);` is **not mutable**

  * Declaring an object with an **explicit** discriminant value (:ada:`Faculty`) makes it immutable

--------------------------------
Mutable Variant Record Example
--------------------------------

* Each object of :ada:`Person` has three components, but it depends on :ada:`Group`

  .. code:: Ada

    Pat : Person := (Student, 19, 3.9);
    Sam : Person (Faculty);

* You can only change the discriminant of :ada:`Pat`, but only via a whole record assignment, e.g:

  .. code:: Ada

    if Pat.Group = Student then
      Pat := (Faculty, Pat.Age, 1);
    else
      Pat := Sam;
    end if;
    Update (Pat);
    
* But you cannot change the discriminant of :ada:`Sam`

  * :ada:`Sam := Pat;` will give you a run-time error if :ada:`Pat.Group` is not :ada:`Faculty`

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

.. code:: Ada

    type Variant_T (Floating : Boolean := False) is record
        case Floating is
            when False =>
                I : Integer;
            when True =>
                F : Float;
        end case;
        Flag : Character;
    end record;

    Variant_Object : Variant_T (True);

Which component does :ada:`Variant_Object` contain?

A. :ada:`Variant_Object.F, Variant_Object.Flag`
B. :ada:`Variant_Object.F`
C. :answer:`None: Compilation error`
D. None: Run-time error

.. container:: animate

    The variant part cannot be followed by a component declaration (:ada:`Flag : Character` here)

