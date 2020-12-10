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

with Memory_Barriers;

package body STM32F4.NVIC is

   -----------------------
   -- Priority_Grouping --
   -----------------------

   function Priority_Grouping return Word is
   begin
      return Shift_Right (SCB.AIRCR and SCB_AIRCR_PRIGROUP_Mask,
                          SCB_AIRCR_PRIGROUP_Pos);
   end Priority_Grouping;

   ---------------------------
   -- Set_Priority_Grouping --
   ---------------------------

   procedure Set_Priority_Grouping (Priority_Group : Word) is
      Reg_Value : Word;
      PriorityGroupTmp : constant Word := Priority_Group and 16#07#;
      Key              : constant := 16#5FA#;
   begin
      Reg_Value := SCB.AIRCR;
      Reg_Value := Reg_Value and (not (SCB_AIRCR_VECTKEY_Mask or SCB_AIRCR_PRIGROUP_Mask));
      Reg_Value := Reg_Value or
        Shift_Left (Key, SCB_AIRCR_VECTKEY_Pos) or
        Shift_Left (PriorityGroupTmp, 8);
      SCB.AIRCR := Reg_Value;
   end Set_Priority_Grouping;

   ------------------
   -- Set_Priority --
   ------------------

   procedure Set_Priority
     (IRQn     : Interrupt_Id;
      Priority : Word)
   is
      Index : constant Natural := Integer (IRQn);
      Value : constant Word := Shift_Left (Priority, 8 - NVIC_PRIO_BITS) and 16#FF#;
   begin
      --  IRQ numbers are never less than 0 in the current definition, hence the code is different from that in the CMSIS.
      NVIC.IP (Index) := Byte (Value);
   end Set_Priority;

   ----------------------
   -- Encoded_Priority --
   ----------------------

   function Encoded_Priority
     (Priority_Group : Word;  Preempt_Priority : Word;  Subpriority : Word)
      return Word
   is
      PriorityGroupTmp    : constant Word := Priority_Group and 16#07#;
      PreemptPriorityBits : Word;
      SubPriorityBits     : Word;
      Temp1 : Word;
      Temp2 : Word;
      Temp3 : Word;
      Temp4 : Word;
      Temp5 : Word;
   begin
      if (7 - PriorityGroupTmp) > NVIC_PRIO_BITS then
         PreemptPriorityBits := NVIC_PRIO_BITS;
      else
         PreemptPriorityBits := 7 - PriorityGroupTmp;
      end if;

      if (PriorityGroupTmp + NVIC_PRIO_BITS) < 7 then
         SubPriorityBits := 0;
      else
         SubPriorityBits := PriorityGroupTmp - 7 + NVIC_PRIO_BITS;
      end if;

      Temp1 := Shift_Left (1, Integer (PreemptPriorityBits)) - 1;
      Temp2 := Preempt_Priority and Temp1;
      Temp3 := Shift_Left (Temp2, Integer (SubPriorityBits));

      Temp4 := Shift_Left (1, Integer (SubPriorityBits)) - 1;
      Temp5 := SubPriority and Temp4;

      return Temp3 or Temp5;
   end Encoded_Priority;

   ------------------
   -- Set_Priority --
   ------------------

   procedure Set_Priority
     (IRQn             : Interrupt_Id;
      Preempt_Priority : Word;
      Subpriority      : Word)
   is
      Priority_Group : constant Word := Priority_Grouping;
   begin
      Set_Priority
        (IRQn,
         Encoded_Priority (Priority_Group, Preempt_Priority, Subpriority));
   end Set_Priority;

   ----------------------
   -- Enable_Interrupt --
   ----------------------

   procedure Enable_Interrupt (IRQn : Interrupt_Id) is
      IRQn_As_Word : constant Word := Word (IRQn);
      Index        : constant Natural := Integer (Shift_Right (IRQn_As_Word, 5));
   begin
      NVIC.ISER (Index) := Shift_Left (IRQn_As_Word and 16#1F#, 1);
   end Enable_Interrupt;

   -----------------------
   -- Disable_Interrupt --
   -----------------------

   procedure Disable_Interrupt (IRQn : Interrupt_Id) is
      IRQn_As_Word : constant Word := Word (IRQn);
      Index        : constant Natural := Integer (Shift_Right (IRQn_As_Word, 5));
   begin
      --  NVIC->ICER[((uint32_t)(IRQn) >> 5)] = (1 << ((uint32_t)(IRQn) & 0x1F));
      NVIC.ICER (Index) := Shift_Left (1, Integer (IRQn_As_Word and 16#1F#));
   end Disable_Interrupt;

   ------------
   -- Active --
   ------------

   function Active (IRQn : Interrupt_Id) return Boolean is
      IRQn_As_Word : constant Word := Word (IRQn);
      Index        : constant Natural := Integer (Shift_Right (IRQn_As_Word, 5));
      Value        : constant Word := Shift_Left (1, Integer (IRQn_As_Word and 16#1F#));
   begin
      return (NVIC.IABR (Index) and Value) /= 0;
   end Active;

   -------------
   -- Pending --
   -------------

   function Pending (IRQn : Interrupt_Id) return Boolean is
      IRQn_As_Word : constant Word := Word (IRQn);
      Index        : constant Natural := Integer (Shift_Right (IRQn_As_Word, 5));
      Value        : constant Word := Shift_Left (1, Integer (IRQn_As_Word and 16#1F#));
   begin
      return (NVIC.ISPR (Index) and Value) /= 0;
   end Pending;

   -----------------
   -- Set_Pending --
   -----------------

   procedure Set_Pending (IRQn : Interrupt_Id) is
      IRQn_As_Word : constant Word := Word (IRQn);
      Index        : constant Natural := Integer (Shift_Right (IRQn_As_Word, 5));
   begin
      NVIC.ISPR (Index) := Shift_Left (1, Integer (IRQn_As_Word and 16#1F#));
   end Set_Pending;

   -------------------
   -- Clear_Pending --
   -------------------

   procedure Clear_Pending (IRQn : Interrupt_Id) is
      IRQn_As_Word : constant Word := Word (IRQn);
      Index        : constant Natural := Integer (Shift_Right (IRQn_As_Word, 5));
   begin
      NVIC.ICPR (Index) := Shift_Left (1, Integer (IRQn_As_Word and 16#1F#));
   end Clear_Pending;

   ------------------
   -- Reset_System --
   ------------------

   procedure Reset_System is
      Key : constant := 16#05FA#;  --  required for write to be accepted
      use Memory_Barriers;
   begin
      -- Ensure all outstanding memory accesses including
      -- buffered writes are completed
      Data_Synchronization_Barrier;

      SCB.AIRCR := Shift_Left (Key, SCB_AIRCR_VECTKEY_Pos) or
        -- keep priority group unchanged
        (SCB.AIRCR and SCB_AIRCR_PRIGROUP_Mask)  or
        SCB_AIRCR_SYSRESETREQ_Mask;

      -- TODO: why is all code from here onward in the CMSIS???

      Data_Synchronization_Barrier;

      --  wait until reset
      pragma Warnings (Off);
      <<spin>> goto spin;
      pragma Warnings (On);
   end Reset_System;

end STM32F4.NVIC;

