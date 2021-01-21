-------------
Tasking Lab
-------------

* Requirements

   - Create a datastore to set/inspect multiple "registers"

      + Individual registers can be read/written by multiple tasks

   - Create a "monitor" capability that will periodically update each register

      + Each register has it's own update frequency

   - Main program should print register values on request

* Hints

   - Datastore needs to control access to its contents
   - One task per register is easier than one task trying to maintain multiple update frequencies

----------------------------------
Tasking Lab Solution - Datastore
----------------------------------
.. code:: Ada

   package Datastore is
     type Register_T is (One, Two, Three);
     function Read (Register : Register_T) return Integer;
     procedure Write (Register : Register_T;
                      Value    : Integer);
   end Datastore;

   package body Datastore is
     type Register_Data_T is array (Register_T) of Integer;

     protected Registers is
       function Read (Register : Register_T) return Integer;
       procedure Write (Register : Register_T;
                        Value    : Integer);
     private
       Register_Data : Register_Data_T;
     end Registers;

     protected body Registers is
       function Read (Register : Register_T) return Integer is
         (Register_Data (Register));
       procedure Write (Register : Register_T;
                        Value    : Integer) is
       begin
         Register_Data (Register) := Value;
       end Write;
     end Registers;

     function Read (Register : Register_T) return Integer is
       (Registers.Read (Register));
     procedure Write (Register : Register_T;
                      Value    : Integer) is
     begin
       Registers.Write (Register, Value);
     end Write;

   end Datastore;

-------------------------------------------
Tasking Lab Solution - Monitor Tasks Body
-------------------------------------------
.. code:: Ada

   package body Counter is

     task body Counter_T is
       O_Register  : Datastore.Register_T;
       O_Increment : Integer;
       O_Delay     : Duration;
       Initialized : Boolean := False;
     begin

       loop
         select
           accept Initialize (Register   : Datastore.Register_T;
                              Value      : Integer;
                              Increment  : Integer;
                              Delay_Time : Duration) do
             O_Register  := Register;
             O_Increment := Increment;
             O_Delay     := Delay_Time;
             Datastore.Write (Register => O_Register,
                              Value    => Value);
             Initialized := True;
           end Initialize;
         or
           delay O_Delay;
           if Initialized then
             Datastore.Write (Register => O_Register,
                              Value    => Datastore.Read (O_Register) + O_Increment);
           end if;
         end select;
       end loop;
     end Counter_T;

   end Counter;

----------------------------
Tasking Lab Solution - Main
----------------------------
.. code:: Ada

   procedure Main is
     Counters : array (Register_T) of Counter_T;

     function Get (Prompt : String) return Integer is ...
     procedure Print is ...

   begin
     for Register in Register_T loop
       Put_Line ("Register " & Register'Image);
       declare
         V : Integer := Get ("Initial value");
         I : Integer := Get ("Increment");
         D : Integer := Get ("Delay in tenths");
       begin
         Counters (Register).Initialize (Register   => Register,
                                         Value      => V,
                                         Increment  => I,
                                         Delay_Time => Duration (D) / 10.0);
       end;
     end loop;

     loop
       Put_Line ("Enter Q to quit, any other value to print registers");
       declare
         Str : constant String := Get_Line;
       begin
         exit when Str'Length > 0 and then (Str (Str'First) in 'Q' | 'q');
         Print;
       end;
     end loop;

     for Register in Register_T loop
       abort Counters (Register);
     end loop;
   end Main;
