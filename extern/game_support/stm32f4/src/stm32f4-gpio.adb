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

with STM32F4.RCC;
with STM32F4.SYSCFG;
with System.Machine_Code;

package body STM32F4.GPIO is

   -------------
   -- Any_Set --
   -------------

   function Any_Set (Port : GPIO_Port;  Pins : GPIO_Pins) return Boolean is
      These_Pins : Half_Word := 0;
   begin
      for Pin of Pins loop
         These_Pins := These_Pins or Pin'Enum_Rep;
      end loop;
      return (Port.IDR and These_Pins) /= 0;
   end Any_Set;

   ---------
   -- Set --
   ---------

   function Set (Port : GPIO_Port;  Pin : GPIO_Pin) return Boolean is
   begin
      return (Port.IDR and Pin'Enum_Rep) = Pin'Enum_Rep;
   end Set;

   -------------
   -- All_Set --
   -------------

   function All_Set (Port : GPIO_Port;  Pins : GPIO_Pins) return Boolean is
      These_Pins : Half_Word := 0;
   begin
      for Pin of Pins loop
         These_Pins := These_Pins or Pin'Enum_Rep;
      end loop;
      return (Port.IDR and These_Pins) = These_Pins;
   end All_Set;

   ---------
   -- Set --
   ---------

   procedure Set (Port : in out GPIO_Port;  Pin : GPIO_Pin) is
   begin
      Port.BSRR_Set := Pin'Enum_Rep;
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set (Port : in out GPIO_Port;  Pins : GPIO_Pins) is
      These_Pins : Half_Word := 0;
   begin
      for Pin of Pins loop
         These_Pins := These_Pins or Pin'Enum_Rep;
      end loop;
      Port.BSRR_Set := These_Pins'Enum_Rep;
   end Set;

   -----------
   -- Clear --
   -----------

   procedure Clear (Port : in out GPIO_Port;  Pin : GPIO_Pin) is
   begin
      Port.BSRR_Reset := Pin'Enum_Rep;
   end Clear;

   -----------
   -- Clear --
   -----------

   procedure Clear (Port : in out GPIO_Port;  Pins : GPIO_Pins) is
      These_Pins : Half_Word := 0;
   begin
      for Pin of Pins loop
         These_Pins := These_Pins or Pin'Enum_Rep;
      end loop;
      Port.BSRR_Reset := These_Pins;
   end Clear;

   ------------
   -- Toggle --
   ------------

   procedure Toggle (Port : in out GPIO_Port;  Pin : GPIO_Pin) is
      pragma Annotate (CodePeer,
                       False_Positive,
                       "subp always fails",
                       "hardware-specific interaction for IDR and ODR");
   begin
      Port.ODR := Port.ODR xor Pin'Enum_Rep;
   end Toggle;
   pragma Annotate (CodePeer,
                    False_Positive,
                    "postcondition",
                    "hardware-specific semantics");

   ------------
   -- Toggle --
   ------------

   procedure Toggle (Port : in out GPIO_Port;  Pins : GPIO_Pins) is
      These_Pins : Half_Word := 0;
   begin
      for Pin of Pins loop
         These_Pins := These_Pins or Pin'Enum_Rep;
      end loop;
      Port.ODR := Port.ODR xor These_Pins;
   end Toggle;

   ------------
   -- Locked --
   ------------

   function Locked (Port : GPIO_Port;  Pin : GPIO_Pin) return Boolean is
      Temp : Word;
   begin
      Temp := Port.LCKR;
      return (Temp and Pin'Enum_Rep) = Pin'Enum_Rep;
   end Locked;

   ----------
   -- Lock --
   ----------

   procedure Lock (Port : in out GPIO_Port;  Pin : GPIO_Pin) is
      Temp : Word;
      pragma Volatile (Temp);

      use System.Machine_Code;
      use ASCII;
      use System;
   begin
      --  As per the Reference Manual (RM0090; Doc ID 018909 Rev 6) pg 282,
      --  a specific sequence is required to set the Lock bit. Throughout the
      --  sequence the same value for the lower 15 bits of the word must be
      --  used (ie the pin number). The lock bit is referred to as LCKK in
      --  the doc.

--        Temp := LCCK or Pin'Enum_Rep;
--
--        --  set the lock bit
--        Port.LCKR := Temp;
--
--        --  clear the lock bit
--        Port.LCKR := Pin'Enum_Rep;
--
--        --  set the lock bit again
--        Port.LCKR := Temp;
--
--        --  read the lock bit
--        Temp := Port.LCKR;
--
--        --  read the lock bit again
--        Temp := Port.LCKR;

      --  We use the following assembly language sequence because the above
      --  high-level version in Ada works only if the optimizer is enabled.
      --  This is not an issue specific to Ada. If you need a specific sequence
      --  of instructions you should really specify those instructions.
      --  We don't want the functionality to depend on the switches, and we
      --  don't want to preclude debugging, hence the following:

      Asm ("orr  r3, %2, #65536"  & LF & HT &
           "str  r3, %0"          & LF & HT &
           "ldr  r3, %0"          & LF & HT &  -- temp <- pin or LCCK  line 164
           "str  r3, [%1, #28]"   & LF & HT &  -- temp -> lckr         line 167
           "str  %2, [%1, #28]"   & LF & HT &  -- pin -> lckr          line 170
           "ldr  r3, %0"          & LF & HT &
           "str  r3, [%1, #28]"   & LF & HT &  -- temp -> lckr         line 173
           "ldr  r3, [%1, #28]"   & LF & HT &
           "str  r3, %0"          & LF & HT &  -- temp <- lckr         line 176
           "ldr  r3, [%1, #28]"   & LF & HT &
           "str  r3, %0"          & LF & HT,   -- temp <- lckr         line 179
           Inputs => (Address'Asm_Input ("r", Port'Address), -- %1
                     (GPIO_Pin'Asm_Input ("r", Pin))),            -- %2
           Outputs => (Word'Asm_Output ("=m", Temp)),  -- %0
           Volatile => True,
           Clobber => ("r2, r3"));
   end Lock;

   ----------
   -- Lock --
   ----------

   procedure Lock (Port : in out GPIO_Port;  Pins : GPIO_Pins) is
   begin
      for Pin of Pins loop
         Lock (Port, Pin);
      end loop;
   end Lock;

   ------------------
   -- Configure_IO --
   ------------------

   procedure Configure_IO
     (Port   : in out GPIO_Port;
      Pins   : GPIO_Pins;
      Config : GPIO_Port_Configuration)
   is
      MODER   : Pin_Modes_Register := Port.MODER;
      OTYPER  : Output_Types_Register := Port.OTYPER;
      OSPEEDR : Output_Speeds_Register := Port.OSPEEDR;
      PUPDR   : Resistors_Register := Port.PUPDR;
   begin
      for Pin of Pins loop
         declare
            Index : constant Integer := GPIO_Pin'Pos (Pin); -- 0 .. 15
         begin
            MODER (Index)   := Config.Mode;
            OTYPER (Index)  := Config.Output_Type;
            OSPEEDR (Index) := Config.Speed;
            PUPDR (Index)   := Config.Resistors;
         end;
      end loop;
      Port.MODER   := MODER;
      Port.OTYPER  := OTYPER;
      Port.OSPEEDR := OSPEEDR;
      Port.PUPDR   := PUPDR;

      if Config.Locked then
         for Pin of Pins loop
            Lock (Port, Pin);
         end loop;
      end if;
   end Configure_IO;

   ------------------
   -- Configure_IO --
   ------------------

   procedure Configure_IO
     (Port   : in out GPIO_Port;
      Pin    : GPIO_Pin;
      Config : GPIO_Port_Configuration)
   is
      MODER   : Pin_Modes_Register := Port.MODER;
      OTYPER  : Output_Types_Register := Port.OTYPER;
      OSPEEDR : Output_Speeds_Register := Port.OSPEEDR;
      PUPDR   : Resistors_Register := Port.PUPDR;

      Index : constant Integer := GPIO_Pin'Pos (Pin);
   begin
      MODER (Index)   := Config.Mode;
      OTYPER (Index)  := Config.Output_Type;
      OSPEEDR (Index) := Config.Speed;
      PUPDR (Index)   := Config.Resistors;

      Port.MODER   := MODER;
      Port.OTYPER  := OTYPER;
      Port.OSPEEDR := OSPEEDR;
      Port.PUPDR   := PUPDR;

      if Config.Locked then
         Lock (Port, Pin);
      end if;
   end Configure_IO;

   -------------------
   -- Current_Input --
   -------------------

   function Current_Input (Port : GPIO_Port) return Half_Word is
   begin
      return Port.IDR;
   end Current_Input;

   --------------------
   -- Current_Output --
   --------------------

   function Current_Output (Port : GPIO_Port) return Half_Word is
   begin
      return Port.ODR;
   end Current_Output;

   ------------------
   -- Write_Output --
   ------------------

   procedure Write_Output (Port : in out GPIO_Port; Data : Half_Word) is
   begin
      Port.ODR := Data;
   end Write_Output;

   ----------------------------------
   -- Configure_Alternate_Function --
   ----------------------------------

   procedure Configure_Alternate_Function
     (Port : in out GPIO_Port;
      Pin  : GPIO_Pin;
      AF   : GPIO_Alternate_Function)
   is
      Index : constant Integer := GPIO_Pin'Pos (Pin);
   begin
      Port.AFR (Index) := Bits_4 (AF);
   end Configure_Alternate_Function;

   ----------------------------------
   -- Configure_Alternate_Function --
   ----------------------------------

   procedure Configure_Alternate_Function
     (Port : in out GPIO_Port;
      Pins : GPIO_Pins;
      AF   : GPIO_Alternate_Function)
   is
   begin
      for Pin of Pins loop
         Configure_Alternate_Function (Port, Pin, AF);
      end loop;
   end Configure_Alternate_Function;

   -----------------------
   -- Configure_Trigger --
   -----------------------

   procedure Configure_Trigger
     (Port    : in out GPIO_Port;
      Pin     : GPIO_Pin;
      Trigger : External_Triggers)
   is
      use STM32F4.SYSCFG, STM32F4.RCC;
   begin
      SYSCFG_Clock_Enable;

      Connect_External_Interrupt (Port, Pin);
      Set_External_Trigger (Pin, Trigger);
      Select_Trigger_Edge (Pin, Trigger);
   end Configure_Trigger;

   -----------------------
   -- Configure_Trigger --
   -----------------------

   procedure Configure_Trigger
     (Port    : in out GPIO_Port;
      Pins    : GPIO_Pins;
      Trigger : External_Triggers)
   is
   begin
      for Pin of Pins loop
         Configure_Trigger (Port, Pin, Trigger);
      end loop;
   end Configure_Trigger;

end STM32F4.GPIO;
