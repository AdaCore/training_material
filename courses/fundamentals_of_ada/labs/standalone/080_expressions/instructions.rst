-----------------
Expressions Lab
-----------------

* Requirements

   - Allow the user to fill a list with dates
   - After the list is created, use *quantified expressions* to print True/False

      * If any date is not legal (taking into account leap years!)
      * If all dates are in the same calendar year

   - Use *expression functions* for all validation routines

* Hints

   - Use subtype membership for range validation
   - You will need *conditional expressions* in your functions
   - You *can* use component-based iterations for some checks

      * But you *must* use indexed-based iterations for others

-----------------------------------
Expressions Lab Solution - Checks
-----------------------------------

.. code:: Ada

      subtype Year_T is Positive range 1_900 .. 2_099;
      subtype Month_T is Positive range 1 .. 12;
      subtype Day_T is Positive range 1 .. 31;

      type Date_T is record
         Year  : Positive;
         Month : Positive;
         Day   : Positive;
      end record;

      function Is_Leap_Year (Year : Positive)
                             return Boolean is
        (Year mod 400 = 0 or else (Year mod 4 = 0 and Year mod 100 /= 0));

      function Days_In_Month (Month : Positive;
                              Year  : Positive)
                              return Positive is
        (case Month is when 4 | 6 | 9 | 11 => 30,
           when 2 => (if Is_Leap_Year (Year) then 29 else 28), when others => 31);

      function Is_Valid (Date : Date_T)
                         return Boolean is
        (Date.Year in Year_T and then Date.Month in Month_T
         and then Date.Day in 1 .. Days_In_Month (Date.Month, Date.Year));

---------------------------------
Expressions Lab Solution - Main
---------------------------------

.. code:: Ada

      function Number (Prompt : String)
                       return Positive is
      begin
         Put (Prompt & "> ");
         return Positive'Value (Get_Line);
      end Number;

      List : array (1 .. 5) of Date_T;
      Item : Date_T;

   begin

      for I in List'Range loop
         Item.Year  := Number ("Year");
         Item.Month := Number ("Month");
         Item.Day   := Number ("Day");
         List (I)   := Item;
      end loop;

      Put_Line ("Any invalid: " &
                Boolean'Image (for some Date of List => not Is_Valid (Date)));
      Put_Line ("Same Year: " &
                Boolean'Image
                   (for all I in List'Range =>
                      I = List'First or else List (I).Year = List (I - 1).Year));
