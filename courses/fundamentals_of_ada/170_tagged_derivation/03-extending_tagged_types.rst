========================
Extending Tagged Types
========================

----------------------------------
How Do You Extend a Tagged Type?
----------------------------------

* Premise of a tagged type is to :dfn:`extend` an existing type

* In general, that means we want to add more fields

  * We can extend a :ada:`tagged` type by adding fields

  .. code:: Ada

    package Animals is
      type Animal_T is tagged record
        Age : Natural;
      end record;
    end Animals;

    with Animals; use Animals;
    package Mammals is
      type Mammal_T is new Animal_T with record
        Number_Of_Legs : Natural;
      end record;
    end Mammals;

    with Mammals; use Mammals;
    package Canines is
      type Canine_T is new Mammal_T with record
        Domesticated : Boolean;
      end record;
    end Canines;

------------------
Tagged Aggregate
------------------

* At initialization, all fields (including **inherited**) must have a **value**

   .. code:: Ada

     Animal : Animal_T := (Age => 1);
     Mammal : Mammal_T := (Age            => 2,
                           Number_Of_Legs => 2);
     Canine : Canine_T := (Age            => 2,
                           Number_Of_Legs => 4,
                           Domesticated   => True);

* But we can also "seed" the aggregate with a parent object

  .. code:: Ada

    Mammal := (Animal with Number_Of_Legs => 4);
    Canine := (Animal with Number_Of_Legs => 4,
                           Domesticated   => False);
    Canine := (Mammal with Domesticated => True);

----------------------
Private Tagged Types
----------------------

* But data hiding says types should be private!

* So we can define our base type as private

  .. container:: latex_environment tiny

    .. code:: Ada

      package Animals is
        type Animal_T is tagged private;
        function Get_Age (P : Animal_T) return Natural;
        procedure Set_Age (P : in out Animal_T; A : Natural);
      private
        type Animal_T is tagged record
           Age : Natural;
        end record;
      end Animals;

* And still allow derivation

  .. container:: latex_environment tiny

    .. code:: Ada

      with Animals;
      package Mammals is
        type Mammal_T is new Animals.Animal_T with record
          Number_Of_Legs : Natural;
        end record;

* But now the only way to get access to :ada:`Age` is with accessor subprograms

--------------------
Private Extensions
--------------------

* In the previous slide, we exposed the fields for :ada:`Mammal_T`!

* Better would be to make the extension itself private

  .. code:: Ada

    package Mammals is
      type Mammal_T is new Animals.Animal_T with private;
    private
      type Mammal_T is new Animals.Animal_T with record
         Number_Of_Legs : Natural;
      end record;
    end Mammals;

--------------------------------------
Aggregates with Private Tagged Types
--------------------------------------

* Remember, an aggregate must specify values for all components

  * But with private types, we can't see all the components!

* So we need to use the "seed" method:

  .. code:: Ada

    procedure Inside_Mammals_Pkg is
      Animal : Animal_T := Animals.Create;
      Mammal : Mammal_T;
    begin
      Mammal := (Animal with Number_Of_Legs => 4);
      Mammal := (Animals.Create with Number_Of_Legs => 4);
    end Inside_Mammals_Pkg;

* Note that we cannot use :ada:`others => <>` for components that are not visible to us

  .. code:: Ada

    Mammal := (Number_Of_Legs => 4,
               others         => <>);  -- Compile Error

-----------------
Null Extensions
-----------------

* To create a new type with no additional fields

  * We still need to "extend" the record - we just do it with an empty record

    .. code:: Ada

      type Dog_T is new Canine_T with null record;


* We still need to specify the "added" fields in an aggregate

  .. code:: Ada

    C    : Canine_T := Canines.Create;
    Dog1 : Dog_T := C; -- Compile Error
    Dog2 : Dog_T := (C with null record);

------
Quiz
------

Given the following code:

  .. code::ada

    package Parents is
      type Parent_T is tagged private;
      function Create return Parent_T;
    private
      type Parent_T is tagged record
         Id : Integer;
      end record;
    end Parents;

    with Parents; use Parents;
    package Children is
      P : Parent_T;
      type Child_T is new Parent_T with record
         Count : Natural;
      end record;
      function Create (C : Natural) return Child_T;
    end Children;

Which completion(s) of Create is (are) valid?

  A. :answermono:`function Create return Child_T is (Parents.Create with Count => 0);`
  B. ``function Create return Child_T is (others => <>);``
  B. ``function Create return Child_T is (0, 0);``
  D.  :answermono:`function Create return Child_T is (P with Count => 0);`

.. container:: animate

   Explanations

   A. Correct - :ada:`Parents.Create` returns :ada:`Parent_T`
   B. Cannot use :ada:`others` to complete private part of an aggregate
   C. Aggregate has no visibility to :ada:`Id` field, so cannot assign
   D. Correct - :ada:`P` is a :ada:`Parent_T`

