with STM32_SVD.RCC; use STM32_SVD.RCC;
with STM32_SVD.RNG; use STM32_SVD.RNG;

package body STM32.RNG is

   ----------------------
   -- Enable_RNG_Clock --
   ----------------------

   procedure Enable_RNG_Clock is
   begin
      --  TODO: enable the right bit of the RCC_Periph peripheral to enable
      --  the RNG clock.
      null;
   end Enable_RNG_Clock;

   ----------------
   -- Enable_RNG --
   ----------------

   procedure Enable_RNG is
   begin
      --  TODO: enable the right bit of the RNG_Periph peripheral to start
      --  the Random Number Generator.
      null;
   end Enable_RNG;

   -----------------
   -- Disable_RNG --
   -----------------

   procedure Disable_RNG is
   begin
      --  TODO: disable the right bit of the RNG_Periph peripheral to disable
      --  the Random Number Generator.
      null;
   end Disable_RNG;

   ---------------
   -- Reset_RNG --
   ---------------

   procedure Reset_RNG
   is
   begin
      --  TODO: Send the proper bit sequence to the RCC_Periph peripheral
      --  in order to reset the RNG module.
      null;
   end Reset_RNG;

   -----------------
   -- RNG_Enabled --
   -----------------

   function RNG_Enabled return Boolean is
      (RNG_Periph.CR.RNGEN);

   --------------------------
   -- Enable_RNG_Interrupt --
   --------------------------

   procedure Enable_RNG_Interrupt is
   begin
      --  TODO: enable RNG interrupts
      null;
   end Enable_RNG_Interrupt;

   ---------------------------
   -- Disable_RNG_Interrupt --
   ---------------------------

   procedure Disable_RNG_Interrupt is
   begin
      --  TODO: disable RNG interrupts
      null;
   end Disable_RNG_Interrupt;

   ---------------------------
   -- RNG_Interrupt_Enabled --
   ---------------------------

   function RNG_Interrupt_Enabled return Boolean is
      (RNG_Periph.CR.IE);

   --------------
   -- RNG_Data --
   --------------

   function RNG_Data return UInt32
     is (RNG_Periph.DR);

   --------------------
   -- RNG_Data_Ready --
   --------------------

   function RNG_Data_Ready return Boolean
     is (RNG_Periph.SR.DRDY);

   ---------------------------
   -- RNG_Seed_Error_Status --
   ---------------------------

   function RNG_Seed_Error_Status return Boolean is
      (RNG_Periph.SR.SECS);

   ----------------------------
   -- RNG_Clock_Error_Status --
   ----------------------------

   function RNG_Clock_Error_Status return Boolean is
      (RNG_Periph.SR.CECS);

   ---------------------------------
   -- Clear_RNG_Seed_Error_Status --
   ---------------------------------

   procedure Clear_RNG_Seed_Error_Status
   is
   begin
      --  TODO: unset the right bit in the RNG_Periph peripheral to clear
      --  the seed error status flag.
      null;
   end Clear_RNG_Seed_Error_Status;

   ----------------------------------
   -- Clear_RNG_Clock_Error_Status --
   ----------------------------------

   procedure Clear_RNG_Clock_Error_Status
   is
   begin
      --  TODO: unset the right bit in the RNG_Periph peripheral to clear
      --  the clockerror status flag.
      null;
   end Clear_RNG_Clock_Error_Status;

end STM32.RNG;
