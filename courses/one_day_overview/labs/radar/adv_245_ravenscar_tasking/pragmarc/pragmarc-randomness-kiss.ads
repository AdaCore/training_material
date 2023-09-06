-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Marsaglia's KISS Random Number Generator
-- Alogirithm taken from www.fortran.com/kiss.f90

-- History:
-- 2020 OCT 15     J. Carter     V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2016 Oct 01     J. Carter     V1.1--Pulled out Random_Range into PragmARC.Random_Ranges
-- 2013 Aug 01     J. Carter     V1.0--Initial release
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

with Interfaces;

generic
    type Result_Subtype is (<>);
package PragmARC.Randomness.KISS is
   subtype Raw_Value is Interfaces.Unsigned_32;
   subtype Positive_Raw is Raw_Value range 1 .. Raw_Value'Last;

   Default_W : constant := 916_191_069;
   Default_X : constant := 123_456_789;
   Default_Y : constant := 362_436_069;
   Default_Z : constant := 521_288_629;

   type Generator is tagged limited private;

   procedure Set_Seed (State : in out Generator;
                       New_W : in     Raw_Value    := Default_W;
                       New_X : in     Positive_Raw := Default_X;
                       New_Y : in     Positive_Raw := Default_Y;
                       New_Z : in     Positive_Raw := Default_Z);
   -- Sets the seeds for State to those given
   -- The initial values for a Generator are the defaults listed

   function Raw (State : in out Generator) return Raw_Value;
   -- Returns a random value

   function Random (State : in out Generator) return Result_Subtype;
   -- Returns a random value

private -- PragmARC.KISS_Random
   type Generator is tagged limited record
      W : Raw_Value := Default_W;
      X : Raw_Value := Default_X;
      Y : Raw_Value := Default_Y;
      Z : Raw_Value := Default_Z;
   end record;
end PragmARC.Randomness.KISS;
