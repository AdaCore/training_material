===============================
Dispatching and Redispatching
===============================

---------------------------------
Calls on Class-Wide Types (1/3)
---------------------------------

* Any subprogram expecting a :ada:`Root` object can be called with a :ada:`Animal'Class` object

.. code:: Ada

   type Animal is tagged null record;
   procedure Feed (The_Animal : Animal);

   type Dog is new Animal with null record;
   procedure Feed (The_Dog : Dog);

      Stray_Dog : Animal'Class := [...]
      My_Dog    : Dog'Class := [...]
   begin
      Feed (Stray_Dog);
      Feed (My_Dog);

---------------------------------
Calls on Class-Wide Types (2/3)
---------------------------------

* The *actual* type of the object is not known at compile time
* The *right* type will be selected at run-time

.. container:: columns

 .. container:: column

   *Ada*

      .. code:: Ada

         declare
           Stray : Animal'Class :=
                Animal'(others => <>);
           My_Dog : Animal'Class :=
                Dog'(others => <>);
         begin
           Stray.Feed;  -- calls Feed of Animal
           My_Dog.Feed; -- calls Feed of Dog

 .. container:: column

   *C++*

      .. code:: C++

         Animal * Stray = 
                     new Animal ();
         Animal * My_Dog = new Dog ();
         Stray->Feed ();
         My_Dog->Feed ();

---------------------------------
Calls on Class-Wide Types (3/3)
---------------------------------

* It is still possible to force a call to be static using a conversion of view

.. container:: columns

 .. container:: column

   *Ada*

   .. code:: Ada

      declare
        Stray : Animal'Class :=
             Animal'(others => <>);
        My_Dog : Animal'Class :=
             Dog'(others => <>);
      begin
        Animal (Stray).Feed;  -- calls Feed of Animal
        Animal (My_Dog).Feed; -- calls Feed of Animal

 .. container:: column

   *C++*

   .. code:: C++

      Animal * Stray = 
                  new Animal ();
      Animal * My_Dog = new Dog ();
      ((Animal) *Stray).Feed ();
      ((Animal) *My_Dog).Feed ();

-------------------------------
Definite and Class-Wide Views
-------------------------------

* In C++, dispatching occurs only on pointers
* In Ada, dispatching occurs only on class-wide views

.. code:: Ada

   type Animal is tagged null record;
   procedure Groom (The_Animal : Animal);
   procedure Give_Treat (The_Animal : Animal);
   type Dog is new Animal with null record;
   overriding procedure Give_Treat (The_Dog : Dog);
   procedure Groom (The_Animal : Animal) is
   begin
      Give_Treat (The_Animal); -- always calls Give_Treat from Animal
   end Groom;
   procedure Main is
      My_Dog : Animal'Class :=
           Dog'(others => <>);
   begin
      -- Calls Groom from the implicitly overridden subprogram
      -- Calls Give_Treat from Animal!
      My_Dog.Groom;

.. container:: speakernote

   Groom operates on Animal, not Animal'Class

---------------
Redispatching
---------------

* :ada:`tagged` types are always passed by reference

   - The original object is not copied

* Therefore, it is possible to convert them to different views

.. code:: Ada

   type Animal is tagged null record;
   procedure Feed (An_Animal : Animal);
   procedure Pet (An_Animal : Animal);
   type Cat is new Animal with null record;
   overriding procedure Pet (A_Cat : Cat);

-----------------------
Redispatching Example
-----------------------

.. code:: Ada

   procedure Feed (Anml : Animal) is
      Fish : Animal'Class renames
                Animal'Class (Anml); -- naming of a view
   begin
      Pet (Anml); -- static: uses the definite view
      Pet (Animal'Class (Anml)); -- dynamic: (redispatching)
      Pet (Fish);                -- dynamic: (redispatching)

      -- Ada 2005 "distinguished receiver" syntax
      Anml.Pet; -- static: uses the definite view
      Animal'Class (Anml).Pet; -- dynamic: (redispatching)
      Fish.Pet;                -- dynamic: (redispatching)
   end Feed;

------
Quiz
------

.. code::Ada

   package Robots is
      type Robot is tagged null record;
      function Service_Code (The_Bot : Robot) return Integer is (101);
      type Appliance_Robot is new Robot with null record;
      function Service_Code (The_Bot : Appliance_Robot) return Integer is (201);
      type Vacuum_Robot is new Appliance_Robot with null record;
      function Service_Code (The_Bot : Vacuum_Robot) return Integer is (301);
   end Robots;

   with Robots; use Robots;
   procedure Main is
      Robot_Object : Robot'Class := Vacuum_Robot'(others => <>);

What is the value returned by :ada:`Service_Code (Appliance_Robot'Class (Robot_Object));`?

   A. :answer:`301`
   B. 201
   C. 101
   D. Compilation error

.. container:: animate

   Explanations

   A. Correct
   B. Would be correct if :ada:`Robot_Object` was a :ada:`Appliance_Robot` - :ada:`Appliance_Robot'Class` leaves the object as :ada:`Vacuum_Robot`
   C. Object is initialized to something in :ada:`Robot'Class`, but it doesn't have to be :ada:`Robot`
   D. Would be correct if function parameter types were :ada:`'Class`

