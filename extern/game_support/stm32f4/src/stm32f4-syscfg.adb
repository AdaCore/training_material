------------------------------------------------------------------------------
--                                                                          --
--                Hardware Abstraction Layer for STM32 Targets              --
--                                                                          --
--             Copyright (C) 2014, Free Software Foundation, Inc.           --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This file provides register definitions for the STM32F4 (ARM Cortex M4F)
--  microcontrollers from ST Microelectronics.

package body STM32F4.SYSCFG is

   --------------------------------
   -- Connect_External_Interrupt --
   --------------------------------

   procedure Connect_External_Interrupt
     (Port : GPIO_Port;
      Pin  : GPIO_Pin)
   is
      CR_Index   : Integer range EXTI_Control_Registers'Range;
      EXTI_Index : Integer range EXTI_n_List'Range;
      Port_Name  : constant GPIO_Port_Id := As_GPIO_Port_Id (Port);
   begin
      --  First we find the control register, of the four possible, that
      --  contains the EXTI_n value for pin 'n' specified by the Pin parameter.
      --  In effect, this is what we are doing for the EXTICR index:
      --    case GPIO_Pin'Pos (Pin) is
      --       when  0 .. 3  => CR_Index := 0;
      --       when  4 .. 7  => CR_Index := 1;
      --       when  8 .. 11 => CR_Index := 2;
      --       when 12 .. 15 => CR_Index := 3;
      --    end case;
      --  Note that that means we are dependent upon the order of the Pin
      --  declarations because we require GPIO_Pin'Pos(Pin_n) to be 'n', ie
      --  Pin_0 should be at position 0, Pin_1 at position 1, and so forth.
      CR_Index := GPIO_Pin'Pos (Pin) / 4;

      --  Now we must find which EXTI_n value to use, of the four possible,
      --  within the control register. We are depending on the GPIO_Port type
      --  being an enumeration and that the enumeral order is alphabetical on
      --  the Port letter, such that in effect GPIO_A'Pos = 0, GPIO_B'Pos = 1,
      --  and so on.
      EXTI_Index := GPIO_Port_Id'Pos (Port_Name) mod 4;  -- ie 0 .. 3

      --  Finally we assign the port 'number' to the EXTI_n value within the
      --  control register. We depend upon the Port enumerals' underlying
      --  numeric representation values matching what the hardware expects,
      --  that is, the values 0 .. n-1, which we get automatically unless
      --  overridden.

      SYSCFG.EXTICR (CR_Index).EXTI (EXTI_Index) := Port_Name;
   end Connect_External_Interrupt;

   --------------------------------
   -- Connect_External_Interrupt --
   --------------------------------

   procedure Connect_External_Interrupt
     (Port : GPIO_Port;
      Pins : GPIO_Pins)
   is
   begin
      for Pin of Pins loop
         Connect_External_Interrupt (Port, Pin);
      end loop;
   end Connect_External_Interrupt;

   --------------------------
   -- Set_External_Trigger --
   --------------------------

   procedure Set_External_Trigger
     (Pin     : GPIO_Pin;
      Trigger : External_Triggers)
   is
      This_Pin : constant Integer range 0 .. 15 := GPIO_Pin'Pos (Pin);
   begin
      EXTI.IMR (This_Pin) := Trigger in Interrupt_Triggers;
      EXTI.EMR (This_Pin) := Trigger in Event_Triggers;
   end Set_External_Trigger;

   --------------------------
   -- Set_External_Trigger --
   --------------------------

   procedure Set_External_Trigger
     (Pins    : GPIO_Pins;
      Trigger : External_Triggers)
   is
   begin
      for Pin of Pins loop
         Set_External_Trigger (Pin, Trigger);
      end loop;
   end Set_External_Trigger;

   -------------------------
   -- Select_Trigger_Edge --
   -------------------------

   procedure Select_Trigger_Edge
     (Pin     : GPIO_Pin;
      Trigger : External_Triggers)
   is
      This_Pin : constant Integer range 0 .. 15 := GPIO_Pin'Pos (Pin);
   begin
      --  all those that are/include rising edge
      EXTI.RTSR (This_Pin) := Trigger in Interrupt_Rising_Edge  |
                                         Interrupt_Rising_Falling_Edge |
                                         Event_Rising_Edge  |
                                         Event_Rising_Falling_Edge;

      -- all those that are/include falling edge
      EXTI.FTSR (This_Pin) := Trigger in Interrupt_Falling_Edge |
                                         Interrupt_Rising_Falling_Edge |
                                         Event_Falling_Edge |
                                         Event_Rising_Falling_Edge;
   end Select_Trigger_Edge;

   -------------------------
   -- Select_Trigger_Edge --
   -------------------------

   procedure Select_Trigger_Edge
     (Pins    : GPIO_Pins;
      Trigger : External_Triggers)
   is
   begin
      for Pin of Pins loop
         Select_Trigger_Edge (Pin, Trigger);
      end loop;
   end Select_Trigger_Edge;

   ------------------------------
   -- Clear_External_Interrupt --
   ------------------------------

   procedure Clear_External_Interrupt (Pin : GPIO_Pin) is
   begin
      EXTI.PR (GPIO_Pin'Pos (Pin)) := 1;  -- yes, value is one to clear it
   end Clear_External_Interrupt;

   ---------------------
   -- As_GPIO_Port_Id --
   ---------------------

   function As_GPIO_Port_Id (Port : GPIO_Port) return GPIO_Port_Id is
      use System;
   begin
      -- TODO: rather ugly to have this board-specific range here
      if Port'Address = System'To_Address (GPIOA_Base) then
         return GPIO_Port_A;
      elsif Port'Address = System'To_Address (GPIOB_Base) then
         return GPIO_Port_B;
      elsif Port'Address = System'To_Address (GPIOC_Base) then
         return GPIO_Port_C;
      elsif Port'Address = System'To_Address (GPIOD_Base) then
         return GPIO_Port_D;
      elsif Port'Address = System'To_Address (GPIOE_Base) then
         return GPIO_Port_E;
      elsif Port'Address = System'To_Address (GPIOF_Base) then
         return GPIO_Port_F;
      elsif Port'Address = System'To_Address (GPIOG_Base) then
         return GPIO_Port_G;
      elsif Port'Address = System'To_Address (GPIOH_Base) then
         return GPIO_Port_H;
      elsif Port'Address = System'To_Address (GPIOI_Base) then
         return GPIO_Port_I;
      elsif Port'Address = System'To_Address (GPIOJ_Base) then
         return GPIO_Port_J;
      elsif Port'Address = System'To_Address (GPIOK_Base) then
         return GPIO_Port_K;
      else
         raise Program_Error;
      end if;
   end As_GPIO_Port_Id;

end STM32F4.SYSCFG;
