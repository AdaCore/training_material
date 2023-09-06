with STM32_SVD.RCC; use STM32_SVD.RCC;
with STM32_SVD.RNG; use STM32_SVD.RNG;

package body STM32.RNG is

   ----------------------
   -- Enable_RNG_Clock --
   ----------------------

   procedure Enable_RNG_Clock is
   begin
      RCC_Periph.AHB2ENR.RNGEN := True;
   end Enable_RNG_Clock;

   ----------------
   -- Enable_RNG --
   ----------------

   procedure Enable_RNG is
   begin
      RNG_Periph.CR.RNGEN := True;
   end Enable_RNG;

   -----------------
   -- Disable_RNG --
   -----------------

   procedure Disable_RNG is
   begin
      RNG_Periph.CR.RNGEN := False;
   end Disable_RNG;

   ---------------
   -- Reset_RNG --
   ---------------

   procedure Reset_RNG
   is
   begin
      RCC_Periph.AHB2RSTR.RNGRST := True;
      RCC_Periph.AHB2RSTR.RNGRST := False;
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
      RNG_Periph.CR.IE := True;
   end Enable_RNG_Interrupt;

   ---------------------------
   -- Disable_RNG_Interrupt --
   ---------------------------

   procedure Disable_RNG_Interrupt is
   begin
      RNG_Periph.CR.IE := False;
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
      RNG_Periph.SR.SECS := False;
   end Clear_RNG_Seed_Error_Status;

   ----------------------------------
   -- Clear_RNG_Clock_Error_Status --
   ----------------------------------

   procedure Clear_RNG_Clock_Error_Status
   is
   begin
      RNG_Periph.SR.CECS := False;
   end Clear_RNG_Clock_Error_Status;

end STM32.RNG;
