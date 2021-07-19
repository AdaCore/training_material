--  This file provides the API for the random number generator on the STM32F4
--  (ARM Cortex M4F) microcontrollers from ST Microelectronics.
--
--  Random numbers are acquired by responding to interrupts from the on-board
--  generator.

package STM32.RNG.Interrupts is

   procedure Initialize_RNG with
     Post => RNG_Interrupt_Enabled;
   --  Must be called once, prior to any call to get a random number via
   --  interrupts. Both necessary and sufficient.
   --  Enables the clock as well.

   function Random return UInt32;
   --  Uses the interrupt interface to get the next available number.
   --  NB: call Initialize_RNG before any calls to this function.

end STM32.RNG.Interrupts;
