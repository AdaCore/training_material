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
      pragma Interrupt_Priority;
      entry Get_Random_32 (Value : out UInt32);
   private

      Last           : UInt32 := 0;
      Buffer         : Ring_Buffer;
      Data_Available : Boolean := False;

      procedure Interrupt_Handler;

      pragma Attach_Handler
        (Interrupt_Handler,
         Ada.Interrupts.Names.HASH_RNG_Interrupt);

   end Receiver;

   --------------
   -- Receiver --
   --------------

   protected body Receiver is

      -------------------
      -- Get_Random_32 --
      -------------------

      entry Get_Random_32 (Value : out UInt32) when Data_Available is
         Next : constant Integer :=
           (Buffer.Tail + 1) mod Buffer.Content'Length;
      begin
         --  Remove an item from our ring buffer.
         Value := Buffer.Content (Next);
         Buffer.Tail := Next;

         --  If the buffer is empty, make sure we block subsequent callers
         --  until the buffer has something in it.
         if Buffer.Tail = Buffer.Head then
            Data_Available := False;
         end if;

         Enable_RNG;
      end Get_Random_32;

      -----------------------
      -- Interrupt_Handler --
      -----------------------

      procedure Interrupt_Handler is
         Current : UInt32;
      begin
         if RNG_Seed_Error_Status then
            Clear_RNG_Seed_Error_Status;

            --  Clear then set the RNGEN bit to reinitialize and restart
            --  the RNG.
            Reset_RNG;
         end if;

         if RNG_Clock_Error_Status then
            --  TODO: reconfigure the clock and make sure it's okay

            --  Clear the bit.
            Clear_RNG_Clock_Error_Status;
         end if;

         if RNG_Data_Ready then
            Current := RNG_Data;

            if Current /= Last then
               --  This number is good.
               if (Buffer.Head + 1) mod Buffer.Content'Length = Buffer.Tail
               then
                  --  But our buffer is full.  Turn off the RNG.
                  Disable_RNG;
               else
                  --  Add this new data to our buffer.
                  Buffer.Head := (Buffer.Head + 1) mod Buffer.Content'Length;
                  Buffer.Content (Buffer.Head) := Current;

                  Data_Available := True;
                  Last := Current;
               end if;
            end if;
         end if;
      end Interrupt_Handler;

   end Receiver;

   --------------------
   -- Initialize_RNG --
   --------------------

   procedure Initialize_RNG is
      Discard : UInt32;
   begin
      Enable_RNG_Clock;
      Enable_RNG_Interrupt;
      Enable_RNG;

      --  Discard the first randomly generated number, according to STM32F4
      --  docs.
      Receiver.Get_Random_32 (Discard);
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
