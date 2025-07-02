===============================
Exotic Dispatching Operations
===============================

-------------------------------
Multiple Dispatching Operands
-------------------------------

* Primitives with multiple dispatching operands are allowed if all operands are of the same type

   .. code:: Ada

      type Animal is tagged null record;
      procedure Interact (Left : Animal; Right : Animal);
      type Dog is new Animal with null record;
      overriding procedure Interact (Left : Dog; Right : Dog);

* At call time, all actual parameters' tags have to match, either statically or dynamically

   .. code:: Ada

      Animal_1, Animal_2   : Animal;
      Dog_1, Dog_2 : Dog;
      Any_Animal_1 : Animal'Class := Animal_1;
      Any_Animal_2 : Animal'Class := Animal_2;
      Dog_Animal : Animal'Class := Dog_1;
      ...
      Interact (Animal_1, Animal_2);                    -- static:  ok
      Interact (Animal_1, Dog_1);                       -- static:  error
      Interact (Any_Animal_1, Any_Animal_2);            -- dynamic: ok
      Interact (Any_Animal_1, Dog_Animal);              -- dynamic: error
      Interact (Animal_1, Any_Animal_1);                -- static:  error
      Interact (Animal'Class (Animal_1), Any_Animal_1); -- dynamic: ok

---------------------------
Special Case for Equality
---------------------------

* Overriding the default equality for a :ada:`tagged` type involves the use of a function with multiple controlling operands
* As in general case, static types of operands have to be the same
* If dynamic types differ, equality returns false instead of raising exception

.. code:: Ada

   type Animal is tagged null record;
   function "=" (Left : Animal; Right : Animal) return Boolean;
   type Dog is new Animal with null record;
   overriding function "=" (Left : Dog; Right : Dog) return Boolean;
   Animal_1, Animal_2 : Animal;
   Dog_1, Dog_2 : Dog;
   Any_Animal_1 : Animal'Class := Animal_1;
   Any_Animal_2 : Animal'Class := Animal_2;
   Dog_Animal   : Animal'Class := Dog_1;
   ...
   -- overridden "=" called via dispatching
   if Any_Animal_1 = Any_Animal_2 then [...]
   if Any_Animal_1 = Dog_Animal then [...] -- returns false

--------------------------
Controlling Result (1/2)
--------------------------

* The controlling operand may be the return type

   - This is known as the constructor pattern

      .. code:: Ada

         type Animal is tagged null record;
         function Feed_Treats (Number_Of_Treats : Integer) return Animal;

* If the child adds components, all such subprograms have to be overridden

      .. code:: Ada

         type Animal is tagged null record;
         function Feed_Treats (Number_Of_Treats : Integer) return Animal;

         type Dog is new Animal with null record;
         --  OK, Feed_Treats is implicitly inherited

         type Bulldog is new Animal with record
            Has_Underbite : Boolean;
         end record;
         --  ERROR no implicitly inherited function Feed_Treats

* Primitives returning abstract types have to be abstract

      .. code:: Ada

         type Animal is abstract tagged null record;
         function Feed_Treats (Number_Of_Treats : Integer) return Animal is abstract;

--------------------------
Controlling Result (2/2)
--------------------------

* Primitives returning :ada:`tagged` types can be used in a static context

   .. code:: Ada

      type Animal is tagged null record;
      function Feed return Animal;
      type Dog is new Animal with null record;
      function Feed return Dog;
      Fed_Animal : Animal := Feed;

* In a dynamic context, the type has to be known to correctly dispatch

   .. code:: Ada
     
     Fed_Animal : Animal'Class := 
                           Animal'(Feed);    -- Static call to Animal primitive
     Another_Fed_Animal : Animal'Class := Fed_Animal;
     Fed_Dog : Animal'Class := Dog'(Feed);   -- Static call to Dog primitive
     Starving_Animal : Animal'Class := Feed; -- Error - ambiguous expression
     ...
     Fed_Animal := Feed;         -- Dispatching call to Animal primitive
     Another_Fed_Animal := Feed; -- Dispatching call to Animal primitive
     Fed_Dog := Feed;            -- Dispatching call to Dog primitive

* No dispatching is possible when returning access types

