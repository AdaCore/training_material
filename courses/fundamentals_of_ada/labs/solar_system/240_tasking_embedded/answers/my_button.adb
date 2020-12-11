------------------------------------------------------------------------------
--                                                                          --
--                             GNAT EXAMPLE                                 --
--                                                                          --
--          Copyright (C) 2014-2017, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
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

with STM32F4;             use STM32F4;
with STM32F4.GPIO;        use STM32F4.GPIO;

with STM32F4.RCC;         use STM32F4.RCC;
with STM32F4.SYSCFG;      use STM32F4.SYSCFG;
with STM32F429_Discovery; use STM32F429_Discovery;

package body My_Button is

   Debounce_Time : constant Time_Span := Milliseconds (100);

   protected body Button is

      entry Wait_Press when Is_Pressed is
      begin
         Is_Pressed := False;
      end Wait_Press;

      procedure Interrupt_Handler is
         Now : constant Time := Clock;
      begin
         --  Clear interrupt
         Clear_External_Interrupt (Pin_0);

         --  Debouncing
         if Now - Last_Time >= Debounce_Time then
            Last_Time  := Now;
            Is_Pressed := True;
         end if;
      end Interrupt_Handler;

   end Button;

   procedure Initialize is
      Configuration : GPIO_Port_Configuration;
   begin
      Enable_Clock (GPIO_A);

      Configuration.Mode      := Mode_In;
      Configuration.Resistors := Floating;
      Configure_IO (Port => GPIO_A, Pin => Pin_0, Config => Configuration);

      Connect_External_Interrupt (GPIO_A, Pin_0);

      Configure_Trigger (GPIO_A, Pin_0, Interrupt_Rising_Edge);
   end Initialize;

begin
   Initialize;
end My_Button;
