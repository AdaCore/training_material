with Ada.Interrupts.Names;

package body STM32.RNG.Interrupts is

   type Buffer_Content is array (Integer range <>) of UInt32;

   type Ring_Buffer is record
      Content : Buffer_Content (0 .. 9);
      Head    : Integer := 0;
      Tail    : Integer := 0;
   end record;

   --------------
   -- Receiver --
   --------------

   protected Receiver is
      --  Implement the Receiver protected object as an interrupt handler for
      --  the Ada.Interrupts.Names.HASH_RNG_Interrupt.
      --
      --  The Last, Buffer and Data_Available provate variables are used to
      --  store and control the data present in the RNG_Perpih peripheral.
      --
      --  Pay attention in settting the right priority.

      entry Get_Random_32 (Value : out UInt32);
   private

      Last           : UInt32 := 0;
      Buffer         : Ring_Buffer;
      Data_Available : Boolean := False;

      procedure Interrupt_Handler;

   end Receiver;

   --------------
   -- Receiver --
   --------------

   protected body Receiver is

      --  TODO: implement the body of the Receiver interrupt handler

      -------------------
      -- Get_Random_32 --
      -------------------

      entry Get_Random_32 (Value : out UInt32) when Data_Available is begin
         null;
      end Get_Random_32;

      -----------------------
      -- Interrupt_Handler --
      -----------------------

      procedure Interrupt_Handler is null;

   end Receiver;

   --------------------
   -- Initialize_RNG --
   --------------------

   procedure Initialize_RNG is
   begin
      --  TODO: initialize the RNG module in interrupt modue using the
      --  subprograms defined in the STM32.RNG package.
      null;
   end Initialize_RNG;

   ------------
   -- Random --
   ------------

   function Random return UInt32 is
      Result : UInt32;
   begin
      Receiver.Get_Random_32 (Result);
      return Result;
   end Random;

end STM32.RNG.Interrupts;
