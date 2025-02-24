==================
Classes of Types
==================

----------
Classes
----------

* In Ada, a Class denotes an inheritance subtree
* Class of `Root` is the class of `Root` and all its children
* Type :ada:`Root'Class` can designate any object typed after type of class of `Root`

   .. code:: Ada

      type Root is tagged null record;
      type Child1 is new Root with null record;
      type Child2 is new Root with null record;
      type Grand_Child1 is new Child1 with null record;
      -- Root'Class = {Root, Child1, Child2, Grand_Child1}
      -- Child1'Class = {Child1, Grand_Child1}
      -- Child2'Class = {Child2}
      -- Grand_Child1'Class = {Grand_Child1}

* Objects of type :ada:`Root'Class` have at least the properties of `Root`

   - Components of `Root`
   - Primitives of `Root`

-----------------
Indefinite Type
-----------------

* A class-wide type is an indefinite type

   - Just like an unconstrained array or a record with a discriminant

* Properties and constraints of indefinite types apply

   - Can be used for parameter declarations
   - Can be used for variable declaration with initialization

.. code:: Ada

   procedure Main is
      type Animal is tagged null record;
      type Dog is new Animal with null record;
      procedure Handle_Animal (Some_Animal : in out Animal'Class) is null;
      My_Dog     : Dog;
      Pet        : Dog'Class    := My_Dog;
      Pet_Animal : Animal'Class := Pet;
      Pet_Dog    : Animal'Class := My_Dog;
      -- initialization required in class-wide declaration
      Bad_Animal : Animal'Class; -- compile error
      Bad_Dog    : Dog'Class;    -- compile error
   begin
      Handle_Animal (Pet);
      Handle_Animal (My_Dog);
   end Main;

-------------------------------
Testing the Type of an Object
-------------------------------

* The tag of an object denotes its type
* It can be accessed through the `'Tag` attribute
* Applies to both objects and types
* Membership operator is available to check the type against a hierarchy

.. code:: Ada

   type Parent is tagged null record;
   type Child is new Parent with null record;
   Parent_Obj : Parent; -- Parent_Obj'Tag = Parent'Tag
   Child_Obj  : Child;  -- Child_Obj'Tag = Child'Tag
   Parent_Class_1 : Parent'Class := Parent_Obj;
                    -- Parent_Class_1'Tag = Parent'Tag
   Parent_Class_2 : Parent'Class := Child_Obj;
                    -- Parent_Class_2'Tag = Child'Tag
   Child_Class    : Child'Class := Child (Parent_Class_2);
                    -- Child_Class'Tag  = Child'Tag

   B1 : Boolean := Parent_Class_1 in Parent'Class; -- True
   B2 : Boolean := Parent_Class_1'Tag = Child'Tag; -- False
   B3 : Boolean := Child_Class'Tag = Parent'Tag;   -- False
   B4 : Boolean := Child_Class in Child'Class;     -- True

----------------
Abstract Types
----------------

* A tagged type can be declared :ada:`abstract`
* Then, :ada:`abstract tagged` types:

   - cannot be instantiated
   - can have abstract subprograms (with no implementation)
   - Non-abstract derivation of an abstract type must override and implement abstract subprograms

---------------------------
Abstract Types Ada Vs C++
---------------------------

* Ada

    .. code:: Ada

       type Animal is abstract tagged record
          Number_Of_Eyes : Integer;
       end record;
       procedure Feed (The_Animal : Animal) is abstract;
       procedure Pet (The_Animal : Animal);
       type Dog is abstract new Animal with null record;
       type Bulldog is new Dog with null record;

       overriding  -- Ada 2005 and later
       procedure Feed (The_Animal : Bulldog);

* C++

    .. code:: Ada

       class Animal {
          public:
             int Number_Of_Eyes;
             virtual void Feed (void) = 0;
             virtual void Pet (void);
       };
       class Dog : public Animal {
       };
       class Bulldog {
          public:
             virtual void Feed (void);
       };

.. container:: speakernote

   "overriding" keyword is optional

------------------------
Relation to Primitives
------------------------

Warning: Subprograms with parameter of type `Root'Class` are not primitives of `Root`

      .. code:: Ada

         type Root is tagged null record;
         procedure Not_A_Primitive (Param : Root'Class);
         type Child is new Root with null record;
         -- This does not override Not_A_Primitive!
         overriding procedure Not_A_Primitive (Param : Child'Class);

----------------------------
'Class and Prefix Notation
----------------------------

Prefix notation rules apply when the first parameter is of a class-wide type

      .. code:: Ada

         type Animal is tagged null record;
         procedure Handle_Animal (Some_Animal : Animal'Class);
         type Cat is new Animal with null record;

         Stray_Animal : Animal;
         Pet_Animal   : Animal'Class := Animal'(others => <>);
         ...
         Handle_Animal (Stray_Animal);
         Handle_Animal (Pet_Animal);
         Stray_Animal.Handle_Animal;
         Pet_Animal.Handle_Animal;

..
  language_version 2005

