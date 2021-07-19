-----------------------------------------------------------------------
--                              Ada Labs                             --
--                                                                   --
--                 Copyright (C) 2008-2009, AdaCore                  --
--                                                                   --
-- Labs is free  software; you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Libm_Single;                 use Libm_Single;
with Ada.Real_Time;               use Ada.Real_Time;
with Ada.Real_Time.Timing_Events; use Ada.Real_Time.Timing_Events;
with HAL;                         use HAL;
with STM32.RNG.Interrupts;        use STM32.RNG.Interrupts;

package body Solar_System is

   Random_Color_Event_Span : constant Time_Span := Seconds (2);

   type Random_Color_Timing_Event is new Timing_Event with null record;

   ----------------------------------
   -- Random_Color_Timing_Event_PO --
   ----------------------------------

   protected Random_Color_Timing_Event_PO is
      pragma Interrupt_Priority;

      entry Wait_On_Random_Color_Timing_Event;

      procedure Random_Color_Timing_Event_Handler
        (Event : in out Timing_Event);

   private

      Time_Is_Up : Boolean := False;

   end Random_Color_Timing_Event_PO;

   Random_Color_Timer         : Random_Color_Timing_Event;
   Random_Color_Timer_Handler : constant Timing_Event_Handler :=
     Random_Color_Timing_Event_PO.Random_Color_Timing_Event_Handler'Access;

   ----------------------------------
   -- Random_Color_Timing_Event_PO --
   ----------------------------------

   protected body Random_Color_Timing_Event_PO is

      entry Wait_On_Random_Color_Timing_Event when Time_Is_Up is
      begin
         Time_Is_Up := False;
      end Wait_On_Random_Color_Timing_Event;

      procedure Random_Color_Timing_Event_Handler
        (Event : in out Timing_Event)
      is
         Now : constant Time := Clock;
      begin
         Time_Is_Up := True;
         Set_Handler
           (Event   => Event,
            At_Time => Now + Random_Color_Event_Span,
            Handler => Random_Color_Timer_Handler);
      end Random_Color_Timing_Event_Handler;

   end Random_Color_Timing_Event_PO;

   ------------------------------
   -- T_Change_Color_To_Random --
   ------------------------------

   task body T_Change_Color_To_Random is
      Body_Data : Body_Type;
   begin
      loop
         Random_Color_Timing_Event_PO.Wait_On_Random_Color_Timing_Event;

         for P_Body of Bodies loop
            Body_Data       := P_Body.Get_Data;
            Body_Data.Color :=
              (R => Color_Component_T (Random mod 255),
               G => Color_Component_T (Random mod 255),
               B => Color_Component_T (Random mod 255),
               A => 255);
            P_Body.Set_Data (Body_Data);
         end loop;
      end loop;
   end T_Change_Color_To_Random;

   ------------------------------
   -- Enable_Random_Color_Mode --
   ------------------------------

   procedure Enable_Random_Color_Mode is
      Now : constant Time := Clock;
   begin
      Initialize_RNG;
      Set_Handler
        (Event   => Random_Color_Timer,
         At_Time => Now + Random_Color_Event_Span,
         Handler => Random_Color_Timer_Handler);
   end Enable_Random_Color_Mode;

   ---------------
   -- Init_Body --
   ---------------

   procedure Init_Body
     (B            : Bodies_Enum;
      Radius       : Float;
      Color        : RGBA_T;
      Distance     : Float;
      Speed        : Float;
      Turns_Around : Bodies_Enum;
      Angle        : Float   := 0.0;
      Tail         : Boolean := False;
      Visible      : Boolean := True)
   is
   begin

      Bodies (B).Set_Data
        ((Distance     => Distance,
          Speed        => Speed,
          Angle        => Angle,
          Turns_Around => Turns_Around,
          Visible      => Visible,
          Color        => Color,
          Radius       => Radius,
          Pos          => (0.0, 0.0),
          With_Tail    => Tail,
          Tail         => (others => (0.0, 0.0))));
   end Init_Body;

   -- implement a function to compute the X coordinate
   -- x of the reference + distance * cos(angle)
   function Compute_X
     (Body_To_Move : Body_Type;
      Turns_Around : Body_Type) return Float;

   -- implement a function to compute the Y coordinate
   -- y of the reference + distance * sin(angle)
   function Compute_Y
     (Body_To_Move : Body_Type;
      Turns_Around : Body_Type) return Float;

   ---------------
   -- Compute_X --
   ---------------

   function Compute_X
     (Body_To_Move : Body_Type;
      Turns_Around : Body_Type) return Float
   is
   begin
      return Turns_Around.Pos.X +
        Body_To_Move.Distance * Cos (Body_To_Move.Angle);
   end Compute_X;

   ---------------
   -- Compute_X --
   ---------------

   function Compute_X
     (X_Ref    : Float;
      Angle    : Float;
      Distance : Float) return Float
   is
   begin
      return X_Ref + Distance * Cos (Angle);
   end Compute_X;

   ---------------
   -- Compute_Y --
   ---------------

   function Compute_Y
     (Body_To_Move : Body_Type;
      Turns_Around : Body_Type) return Float
   is
   begin
      return Turns_Around.Pos.Y +
        Body_To_Move.Distance * Sin (Body_To_Move.Angle);
   end Compute_Y;

   ---------------
   -- Compute_Y --
   ---------------

   function Compute_Y
     (Y_Ref    : Float;
      Angle    : Float;
      Distance : Float) return Float
   is
   begin
      return Y_Ref + Distance * Sin (Angle);
   end Compute_Y;

   ----------
   -- Move --
   ----------

   procedure Move
     (Body_To_Move : in out Body_Type;
      Turns_Around :        Body_Type)
   is
   begin

      Body_To_Move.Pos.X := Compute_X (Body_To_Move, Turns_Around);

      Body_To_Move.Pos.Y := Compute_Y (Body_To_Move, Turns_Around);

      Body_To_Move.Angle := Body_To_Move.Angle + Body_To_Move.Speed;

      if Body_To_Move.With_Tail then
         for I in Body_To_Move.Tail'First .. Body_To_Move.Tail'Last - 1 loop
            Body_To_Move.Tail (I) := Body_To_Move.Tail (I + 1);
         end loop;
         Body_To_Move.Tail (T_Tail'Last) := Body_To_Move.Pos;
      end if;

   end Move;

   ------------
   -- P_Body --
   ------------

   protected body P_Body is

      --------------
      -- Get_Data --
      --------------

      function Get_Data return Body_Type is
      begin
         return Data;
      end Get_Data;

      --------------
      -- Set_Data --
      --------------

      procedure Set_Data (B : Body_Type) is
      begin
         Data := B;
      end Set_Data;

   end P_Body;

   protected body Dispatch_Tasks is

      -------------------
      -- Get_Next_Body --
      -------------------

      procedure Get_Next_Body (B : out Bodies_Enum) is
      begin
         B := Current;
         if Current /= Bodies_Enum'Last then
            Current := Bodies_Enum'Succ (Current);
         end if;
      end Get_Next_Body;

   end Dispatch_Tasks;

   -----------------
   -- T_Move_Body --
   -----------------

   task body T_Move_Body is
      -- declare a variable Now of type Time to record current time
      Now : Time;
      -- declare a constant Period of 40 milliseconds of type Time_Span defining the loop period
      Period       : constant Time_Span := Milliseconds (20);
      Current      : Body_Type;
      Turns_Around : Body_Type;
      B            : Bodies_Enum;
   begin
      Dispatch_Tasks.Get_Next_Body (B);

      loop
         Now          := Clock;
         Current      := Bodies (B).Get_Data;
         Turns_Around := Bodies (Current.Turns_Around).Get_Data;
         Move (Current, Turns_Around);
         Bodies (B).Set_Data (Current);

         delay until Now + Period;
      end loop;
   end T_Move_Body;

end Solar_System;
