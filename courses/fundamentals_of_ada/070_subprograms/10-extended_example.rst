===================
Extended Example
===================

-----------------------------
Implementing a Simple "Set"
-----------------------------

* We want to indicate which colors of the rainbow are in a **set**

   * If you remember from the *Basic Types* module, a type is made up of values and primitive operations

* Our values will be

   * Type indicating colors of the rainbow
   * Type to group colors
   * Mechanism to indicate which color is in our set

* Our primitive operations will be

   * Create a set
   * Add a color to the set
   * Remove a color from the set
   * Check if color is in set

--------------------
Values for the Set
--------------------

* Colors of the rainbow

   .. code:: Ada

      type Color_T is (Red, Orange, Yellow, Green,
                       Blue, Indigo, Violet,
                       White, Black);

* Group of colors

   .. code:: Ada

      type Group_Of_Colors_T is
         array (Positive range <>) of Color_T;

* Mechanism indicating which color is in the set

   .. code:: Ada

      type Set_T is array (Color_T) of Boolean;
      --  if array component at Color is True,
      --  the color is in the set

----------------------------------
Primitive Operations for the Set
----------------------------------

* Create a set

   .. code:: Ada

      function Make (Colors : Group_Of_Colors_T) return Set_T;

* Add a color to the set

   .. code:: Ada

      procedure Add (Set    : in out Set_T;
                     Color  :        Color_T);

* Remove a color from the set

   .. code:: Ada

      procedure Remove (Set    : in out Set_T;
                        Color  :        Color_T);

* Check if color is in set

   .. code:: Ada

      function Contains (Set   : Set_T;
                         Color : Color_T)
                         return Boolean;

--------------------------------------------
Implementation of the Primitive Operations
--------------------------------------------

* Implementation of the primitives is easy

   * We could do operations directly on :ada:`Set_T`, but that's not flexible

.. code:: Ada

   function Make (Colors : Group_Of_Colors_T) return Set_T is
      Set : Set_T := (others => False);
   begin
      for Color of Colors loop
         Set (Color) := True;
      end loop;
      return Set;
   end Make;

   procedure Add (Set   : in out Set_T;
                  Color :        Color_T) is
   begin
      Set (Color) := True;
   end Add;

   procedure Remove (Set   : in out Set_T;
                     Color :        Color_T) is
   begin
      Set (Color) := False;
   end Remove;

   function Contains (Set   : Set_T; Color : Color_T) return Boolean is
      (Set (Color));

-------------------------
Using our Set Construct
-------------------------

.. code:: Ada

   Rgb   : Set_T := Make ((Red, Green, Blue));
   Light : Set_T := Make ((Red, Yellow, Green));

.. code:: Ada

   if Contains (Rgb, Black) then
      Remove (Rgb, Black);
   else
      Add (Rgb, Black);
   end if;

* In addition, because of the operations available to arrays of Boolean,
we can easily implement union and intersection

.. code:: Ada

   Union         : Set_T := Rgb or Light;
   Intersection  : Set_T := Rgb and Light;
   Difference    : Set_T := Rgb xor Light;
