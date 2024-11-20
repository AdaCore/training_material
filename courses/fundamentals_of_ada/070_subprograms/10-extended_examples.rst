===================
Extended Examples
===================

------------------------------------
Tic-Tac-Toe Winners Example (Spec)
------------------------------------

.. container:: columns

 .. container:: column

    .. code:: Ada

       package TicTacToe is
         type Players is (Nobody, X, O);
         type Move is range 1 .. 9;
         type Game is array (Move) of
           Players;
         function Winner (This : Game)
           return Players;
         ...
       end TicTacToe;

 .. container:: column

    .. list-table::

      * - :subscript:`1` N

        - :subscript:`2` N
        - :subscript:`3` N

      * - :subscript:`4` N

        - :subscript:`5` N
        - :subscript:`6` N

      * - :subscript:`7` N

        - :subscript:`8` N
        - :subscript:`9` N

------------------------------------
Tic-Tac-Toe Winners Example (Body)
------------------------------------

.. code:: Ada

   function Winner (This : Game) return Players is
     type Winning_Combinations is range 1 .. 8;
     type Required_Positions   is range 1 .. 3;
     Winning : constant array
       (Winning_Combinations, Required_Positions)
         of Move := (-- rows
                     (1, 2, 3), (4, 5, 6), (7, 8, 9),
                     -- columns
                     (1, 4, 7), (2, 5, 8), (3, 6, 9),
                     -- diagonals
                     (1, 5, 9), (3, 5, 7));

   begin
     for K in Winning_Combinations loop
       if This (Winning (K, 1)) /= Nobody and then
         (This (Winning (K, 1)) = This (Winning (K, 2)) and
          This (Winning (K, 2)) = This (Winning (K, 3)))
       then
         return This (Winning (K, 1));
       end if;
     end loop;
     return Nobody;
   end Winner;

-------------
Set Example
-------------

.. code:: Ada

   -- some colors
   type Color is (Red, Orange, Yellow, Green, Blue, Violet);
   -- truth table for each color
   type Set is array (Color) of Boolean;
   -- unconstrained array of colors
   type Set_Literal is array (Positive range <>) of Color;

   -- Take an array of colors and set table value to True
   -- for each color in the array
   function Make (Values : Set_Literal) return Set;
   -- Take a color and return table with color value set to true
   function Make (Base : Color) return Set;
   -- Return True if the color has the truth value set
   function Is_Member (C : Color; Of_Set: Set) return Boolean;

   Null_Set : constant Set := (Set'Range => False);
   RGB      : Set := Make (
              Set_Literal'(Red, Blue, Green));
   Domain   : Set := Make (Green);

   if Is_Member (Red, Of_Set => RGB) then ...

   -- Type supports operations via Boolean operations,
   -- as Set is a one-dimensional array of Boolean
   S1, S2 : Set := Make (....);
   Union : Set := S1 or S2;
   Intersection : Set := S1 and S2;
   Difference : Set := S1 xor S2;

------------------------------
Set Example (Implementation)
------------------------------

.. code:: Ada

   function Make (Base : Color) return Set is
     Result : Set := Null_Set;
   begin
      Result (Base) := True;
      return Result;
   end Make;

   function Make (Values : Set_Literal) return Set is
     Result : Set := Null_Set;
   begin
     for K in Values'Range loop
       Result (Values (K)) := True;
     end loop;
     return Result;
   end Make;

   function Is_Member (C: Color;
                        Of_Set: Set)
                        return Boolean is
   begin
     return Of_Set (C);
   end Is_Member;

