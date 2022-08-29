-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- History:
-- 2020 Nov 01     J. Carter     V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2016 Oct 01     J. Carter     V1.3--Pulled out Random_Range into PragmARC.Random_Ranges
-- 2016 Jun 01     J. Carter     V1.2--Changed comment for empty declarative part
-- 2013 Nov 01     J. Carter     V1.1--Eliminated an impossible case
-- 2013 Aug 01     J. Carter     V1.0--Initial release

package body PragmARC.Randomness.KISS is
   use type Raw_Value;

   procedure Set_Seed (State : in out Generator;
                       New_W : in     Raw_Value    := Default_W;
                       New_X : in     Positive_Raw := Default_X;
                       New_Y : in     Positive_Raw := Default_Y;
                       New_Z : in     Positive_Raw := Default_Z)
   is
      -- Empty
   begin -- Set_Seed
      State.W := New_W;
      State.X := New_X;
      State.Y := New_Y;
      State.Z := New_Z;
   end Set_Seed;

   function Raw (State : in out Generator) return Raw_Value is
      function ML (Value : in Raw_Value; Shift : in Natural) return Raw_Value;
      -- Returns Value xor Shift_Left (Value, Shift)

      function MR (Value : in Raw_Value; Shift : in Natural) return Raw_Value;
      -- Returns Value xor Shift_Right (Value, Shift)

      function ML (Value : in Raw_Value; Shift : in Natural) return Raw_Value is
         (Value xor Interfaces.Shift_Left (Value, Shift) );

      function MR (Value : in Raw_Value; Shift : in Natural) return Raw_Value is
         (Value xor Interfaces.Shift_Right (Value, Shift) );
   begin -- Raw
      State.W := 30903 * (State.W and 65535) + Interfaces.Shift_Right (State.W, 16);
      State.X := 69069 * State.X + 1327217885;
      State.Y := ML (MR (ML (State.Y, 13), 17), 5);
      State.Z := 18000 * (State.Z and 65535) + Interfaces.Shift_Right (State.Z, 16);

      return State.X + State.Y + Interfaces.Shift_Left (State.Z, 16) + State.W;
   end Raw;

   function Random (State : in out Generator) return Result_Subtype is
   begin
      return Result_Subtype'Val
        (Result_Subtype'Pos (Result_Subtype'First)
         + (Raw (State) mod Raw_Value
          (Result_Subtype'Pos (Result_Subtype'Last) - Result_Subtype'Pos (Result_Subtype'First) + 1)));
   end Random;
end PragmARC.Randomness.KISS;
