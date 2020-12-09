--  This file provides the API for the random number generator on the STM32F4
--  (ARM Cortex M4F) microcontrollers from ST Microelectronics.
--
--  See the child packages for routines to initialze the generator and acquire
--  numbers, using either polling or interrupts.

package STM32.RNG is

   procedure Enable_RNG;

   procedure Disable_RNG;

   procedure Reset_RNG;

   procedure Enable_RNG_Clock;

   function RNG_Enabled return Boolean;

   procedure Enable_RNG_Interrupt;

   procedure Disable_RNG_Interrupt;

   function RNG_Interrupt_Enabled return Boolean;

   function RNG_Data return UInt32;

   function RNG_Data_Ready return Boolean;

   function RNG_Seed_Error_Status return Boolean;

   function RNG_Clock_Error_Status return Boolean;

   procedure Clear_RNG_Seed_Error_Status;

   procedure Clear_RNG_Clock_Error_Status;

end STM32.RNG;
