=================
Variant Records
=================

----------------------------
Discriminated Record Types
----------------------------

* A :dfn:`discriminated record` uses a special field (:dfn:`discriminant`) to specify information about the record

  .. code:: ada

     type Discriminated_Record (Discriminant : Natural) is record
        Text : String (1..Discriminant);
     end record;

  * All objects of :ada:`Discriminated_Record` are of the same type, regardless
    of the value of :ada:`Discriminant`

* Discriminant is treated as any other component

  * But is constant in a *constrained* variant record
  * Discriminant is a discrete type

* Aggregate assignment is allowed

----------------
Variant Record
----------------

* A :dfn:`variant record` is a special case of discriminated record

  * Used in a :ada:`case` block to control visibility of components
  * Discriminant can be used to specify the :dfn:`variant part`
  * Components listed will only be visible if choice matches discriminant
  * Component names need to be unique (even across discriminants)

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

.. note::

   :ada:`case` block must be **last** part of definition - so only **one** per record

* Kind of :dfn:`storage overlay`

   + Similar to :C:`union` in C
   + But preserves **type checking**
   + And object size **is related to** discriminant

----------------------------
Constrained Variant Record
----------------------------

* Assigning an initial value to record declaration makes it :dfn:`Constrained`

  * wether *discriminated* or *variant*
  * Discriminant cannot be modified later on 

  .. code:: Ada

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

    Pat : Person (Student);
    Sam : Person := (Faculty, 33, 5);

* Each object of :ada:`Person` has three components, but it depends on :ada:`Group`

  * :ada:`Pat` has :ada:`Group`, :ada:`Age`, and :ada:`Gpa`
  * :ada:`Sam` has :ada:`Group`, :ada:`Age`, and :ada:`Pubs`

* Aggregate specifies all components, including the discriminant

----------------------------
Constrained Record Example
----------------------------

* Compiler can detect some problems, but more often clashes are run-time errors

  .. code:: Ada

    procedure Do_Something (Param : in out Person) is
    begin
      Param.Age := Param.Age + 1;
      Param.Pubs := Param.Pubs + 1;
    end Do_Something;

  * :ada:`Pat.Pubs := 3;` would generate a compiler warning because compiler knows :ada:`Pat` is a :ada:`Student`

    .. code:: error

      warning: Constraint_Error will be raised at run time

  * :ada:`Do_Something (Pat);` generates a run-time error, because only at runtime is the discriminant for :ada:`Param` known

    .. code:: error

      raised CONSTRAINT_ERROR : discriminant check failed

* :ada:`Pat := Sam;` would be a compiler warning because the constraints do not match

----------------------
Unconstrained Record
----------------------

* A record *object* is :dfn:`Unconstrained` if **Both**
  * Discriminant has a *default value* 
  * Object is instantiated without specifying the discriminant
    * thus using the default value at instantiation

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

* :ada:`Pat : Person;` is **Unconstrained**

--------------------------------------
Unconstrained Variant Record Example
--------------------------------------

* Each object of :ada:`Person` has three components, but it depends on :ada:`Group`

  .. code:: Ada

    Pat : Person := (Student, 19, 3.9);
    Sam : Person;

    begin

      Sam := (Faculty, 28, 20);
      if Pat.Group = Student then
        -- Pat.Group := Faculty; -- ILLEGAL
        Pat := (Faculty, Pat.Age, 0);
      else
        Sam := Pat;
      end if;

* Can change the discriminant of :ada:`Pat` and `Sam`

  * But only via a whole record assignment
  * Direct assignment will still result in an error

------
Quiz
------

.. container:: columns

  .. container:: column

    .. container:: latex_environment tiny

      .. code:: Ada
        :number-lines: 2

        type Variant_T (Valid : Integer) is record
            case Valid is
            when Integer'First .. -1 =>
                Value : Integer;
                State : Boolean;
            when others =>
                Number : Natural;
            end case;
        end record;

        Variant_Object : Variant_T (1);

  .. container:: column

    Which of the following components does :ada:`Variant_Object` contain? (Select all that apply)

    A. :ada:`Variant_Object.Value,`
       :ada:`Variant_Object.State`
    B. :answermono:`Variant_Object.Number`
    C. None: Compilation error
    D. None: Run-time error

.. container:: animate

  Explanation

  * Variant block covers all possible values of :ada:`Valid`, so no
    compilation error

  * Discriminant has a value (1) which is in range, so no run-time error

  * :ada:`Valid` is 1, so it enters the :ada:`when others` block on line 7.
    The block only contains component :ada:`Number`.

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
