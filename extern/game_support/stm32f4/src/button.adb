------------------------------------------------------------------------------
--                                                                          --
--                             GNAT EXAMPLE                                 --
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

with Ada.Interrupts.Names;
with Ada.Real_Time;  use Ada.Real_Time;
with STM32F4;        use STM32F4;
with STM32F4.GPIO;   use STM32F4.GPIO;
with STM32F4.RCC;    use STM32F4.RCC;
with STM32F4.SYSCFG; use STM32F4.SYSCFG;

with STM32F429_Discovery;  use STM32F429_Discovery;

package body Button is

   protected Button is
      pragma Interrupt_Priority;

      function Current_Direction return Directions;

   private
      procedure Interrupt_Handler;
      pragma Attach_Handler
         (Interrupt_Handler,
          Ada.Interrupts.Names.EXTI0_Interrupt);

      Direction : Directions := Clockwise;  -- arbitrary
      Last_Time : Time := Clock;
   end Button;

   Debounce_Time : constant Time_Span := Milliseconds (500);

   protected body Button is

      function Current_Direction return Directions is
      begin
         return Direction;
      end Current_Direction;

      procedure Interrupt_Handler is
         Now : constant Time := Clock;
      begin
         Clear_External_Interrupt (Pin_0);

         --  Debouncing
         if Now - Last_Time >= Debounce_Time then
            if Direction = Counterclockwise then
               Direction := Clockwise;
            else
               Direction := Counterclockwise;
            end if;
            Last_Time := Now;
         end if;
      end Interrupt_Handler;

   end Button;

   function Current_Direction return Directions is
   begin
      return Button.Current_Direction;
   end Current_Direction;

   procedure Initialize is
      Configuration : GPIO_Port_Configuration;
   begin
      Enable_Clock (GPIO_A);

      Configuration.Mode := Mode_In;
      Configuration.Resistors := Floating;
      Configure_IO (Port   => GPIO_A,
                    Pin    => Pin_0,
                    Config => Configuration);

      Connect_External_Interrupt (GPIO_A, Pin_0);

      Configure_Trigger (GPIO_A, Pin_0, Interrupt_Rising_Edge);
   end Initialize;

begin
   Initialize;
end Button;
