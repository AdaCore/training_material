----------------
Visibility Lab
----------------

* Requirements

   - Create a types package for calculating speed in miles per hour

      + At least two different distance measurements (e.g. feet, kilometers)
      + At least two different time measurements (e.g. seconds, minutes)
      + Overloaded operators and/or primitives to handle calculations

   - Create a types child package for converting distance, time, and mph into a string

      + Use :code:`Ada.Text_IO.Float_IO` package to convert floating point to string
      + Create visible global objects to set **Fore** and **Aft** parameters for :code:`Put`

   - Create a main program to enter distance and time and then print speed value

* Hints

   - :code:`use` to get full visibility to `Ada.Text_IO`
   - :code:`use type` to get access to calculations

      + :code:` use all type` if calculations are primitives

   - :code:`renames` to make using **Fore** and **Aft** easier

----------------------------------------
Visibility Lab Solution - Types (Spec)
----------------------------------------

.. code:: Ada

   package Types is

      type Mph_T is digits 6;

      type Feet_T is digits 6;
      type Miles_T is digits 6;
      type Kilometers_T is digits 6;

      type Seconds_T is digits 6;
      type Minutes_T is digits 6;
      type Hours_T is digits 6;

      function "/" (Distance : Feet_T; Time : Seconds_T) return Mph_T;
      function "/" (Distance : Kilometers_T; Time : Minutes_T) return Mph_T;
      function "/" (Distance : Miles_T; Time : Hours_T) return Mph_T;

      function Convert (Distance : Feet_T) return Miles_T;
      function Convert (Distance : Kilometers_T) return Miles_T;
      function Convert (Time : Seconds_T) return Hours_T;
      function Convert (Time : Minutes_T) return Hours_T;

   end Types;

----------------------------------------
Visibility Lab Solution - Types (Body)
----------------------------------------

.. code:: Ada

   package body Types is

      function "/" (Distance : Feet_T; Time : Seconds_T) return Mph_T is
         (Convert (Distance) / Convert (Time));

      function "/" (Distance : Kilometers_T; Time : Minutes_T) return Mph_T is
         (Convert (Distance) / Convert (Time));

      function "/" (Distance : Miles_T; Time : Hours_T) return Mph_T is
         (Mph_T (Distance) / Mph_T (Time));

      function Convert (Distance : Feet_T) return Miles_T is
         (Miles_T (Distance) / 5_280.0);
      function Convert (Distance : Kilometers_T) return Miles_T is
         (Miles_T (Distance) / 1.6);
      function Convert (Time : Seconds_T) return Hours_T is
         (Hours_T (Time) / (60.0 * 60.0));
      function Convert (Time : Minutes_T) return Hours_T is
         (Hours_T (Time) / 60.0);

   end Types;

------------------------------------------------
Visibility Lab Solution - Types.Strings (Spec)
------------------------------------------------

.. code:: Ada

   package Types.Strings is

      Exponent_Digits      : Natural := 2;
      Digits_After_Decimal : Natural := 3;

      function To_String (Value : Mph_T) return String;

      function To_String (Value : Feet_T) return String;
      function To_String (Value : Miles_T) return String;
      function To_String (Value : Kilometers_T) return String;

      function To_String (Value : Seconds_T) return String;
      function To_String (Value : Minutes_T) return String;
      function To_String (Value : Hours_T) return String;

   end Types.Strings;

------------------------------------------------
Visibility Lab Solution - Types.Strings (Body)
------------------------------------------------

.. code:: Ada

   with Ada.Text_IO; use Ada.Text_IO;
   package body Types.Strings is

      package Io is new Ada.Text_IO.Float_IO (Float);
      function To_String (Value : Float) return String is
         Ret_Val : String (1 .. 30);
      begin
         Io.Put (To   => Ret_Val,
                 Item => Value,
                 Aft  => Digits_After_Decimal,
                 Exp  => Exponent_Digits);
         for I in reverse Ret_Val'Range loop
            if Ret_Val (I) = ' ' then
               return Ret_Val
                   (I + 1 .. Ret_Val'Last);
            end if;
         end loop;
         return Ret_Val;
      end To_String;

      function To_String (Value : Mph_T) return String is
         (To_String (Float (Value)));

      function To_String (Value : Feet_T) return String is
         (To_String (Float (Value)));
      function To_String (Value : Miles_T) return String is
         (To_String (Float (Value)));
      function To_String (Value : Kilometers_T) return String is
         (To_String (Float (Value)));

      function To_String (Value : Seconds_T) return String is
         (To_String (Float (Value)));
      function To_String (Value : Minutes_T) return String is
         (To_String (Float (Value)));
      function To_String (Value : Hours_T) return String is
         (To_String (Float (Value)));

   end Types.Strings;

--------------------------------
Visibility Lab Solution - Main
--------------------------------

.. code:: Ada

   with Ada.Text_IO; use Ada.Text_IO;
   with Types;       use Types;
   with Types.Strings;

   procedure Main is

      Aft : Integer renames Types.Strings.Digits_After_Decimal;
      Exp : Integer renames Types.Strings.Exponent_Digits;

      Feet       : Feet_T;
      Kilometers : Kilometers_T;

      Seconds : Seconds_T;
      Minutes : Minutes_T;

      Mph : Mph_T;

      function Get (Prompt : String) return String is
      begin
         Put (Prompt & "> ");
         return Get_Line;
      end Get;

   begin
      Feet       := Feet_T'Value (Get ("Feet"));
      Kilometers := Kilometers_T'Value (Get ("Kilometers"));

      Seconds := Seconds_T'Value (Get ("Seconds"));
      Minutes := Minutes_T'Value (Get ("Minutes"));

      Aft := 2;
      Exp := 2;
      Mph := Feet / Seconds;
      Put_Line
        (Strings.To_String (Feet) & " feet / " & Strings.To_String (Seconds) &
         " seconds = " & Strings.To_String (Mph) & " mph");
      Aft := Aft + 1;
      Exp := Exp + 1;
      Mph := Kilometers / Minutes;
      Put_Line
        (Strings.To_String (Kilometers) & " km / " & Strings.To_String (Minutes) &
         " minute = " & Strings.To_String (Mph) & " mph");

   end Main;

