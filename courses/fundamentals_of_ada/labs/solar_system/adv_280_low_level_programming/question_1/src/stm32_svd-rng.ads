pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.RNG is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  control register

   type CR_Register is null record;
   --  TODO: Implement the CR_Register record type according to the
   --  STM32F429Discovery datasheet.

   --  status register

   type SR_Register is null record;
   --  TODO: Implement the SR_Register record type according to the
   --  STM32F429Discovery datasheet.

   -----------------
   -- Peripherals --
   -----------------

   --  Random number generator

   type RNG_Peripheral is null record;
   --  TODO: Implement the RNG_Peripheral record type according to the
   --  STM32F429Discovery datasheet.
   --  Use the previously defined CR_Register and SR_Register record types.

   --  Random number generator

   RNG_Periph : aliased RNG_Peripheral with
      Import;
      --  TODO: Map RNG_Periph to the physical address specified in the
      --  STM32F429Discovery datasheet.

end STM32_SVD.RNG;
